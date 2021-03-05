# Functions to compute the BF using Stan
#


#' Sample and compute BF for various models using Stan
#'
#' Sample and compute BF for various models using Stan.
#'
#' Two-sample model:
#'
#' - samples are stored as matrix `mtx`
#' - reference items are indexed in `mtx` by `idx.ref`
#' - questioned items are indexed in `mtx` by `idx.quest`
#' - other items, non indexed, are **discarded**.
#'
#' Hypotheses:
#'
#' - \eqn{H_1}: samples in `idx.ref` and `idx.quest` come from the same source
#' - \eqn{H_2}: samples in `idx.ref` and `idx.quest` come from different sources
#'
#' Return a `stanBF` object with these properties:
#' 
#' - `model_name`
#' - `stanmodel` (named list of Stan models)
#' - `stanfit` (named list of `stanfit` objects)
#' - `stanbridge` (named list of `bridgesampler` objects)
#' - `BF` (a double: the Bayes Factor)
#'
#' For Dirichlet likelihoods, the returned object is a `stanBF_turn`, inheriting from `stanBF`.
#' These objects contain also:
#' - `df_samples` (data.frame with posterior samples)
#' - custom plot methods
#'
#' The object contains methods to plot and to extract samples.
#'
#' @param data a list containing `mtx`, `idx.ref`, `idx.quest`
#' @param model the model shortname (e.g. `'DirDir'`, `'DirFNorm'`)
#' @param hyperpriors a list containing hyperparameter definitions
#' @param n.iter number of HMC iterations (default: 1000)
#' @param n.burnin number of HMC burn-in iterations (default: 200)
#' @param n.chains number of HMC chains (default: 1)
#' @param n.cores number of cores to use for HMC and bridge sampling (default: 1)
#' @param data_other a list containing additional data for \eqn{H_1} and \eqn{H_2} models (default: `NULL`)
#' @param silent if TRUE, do not print any progress
#' @param ... list of additional parameters to pass to [rstan::sampling()] method
#' @return a `stanBF` object
#' @importFrom utils modifyList
#' @importFrom rlang abort
#' @importFrom assertthat assert_that
#' @importFrom rstan sampling
#' @importFrom bridgesampling bridge_sampler bf
#' @export
#' @md
compute_BF_Stan <- function(data, model, hyperpriors,
                            n.iter = 1000, n.burnin = 200, n.chains = 1, n.cores = 1,
                            data_other=NULL, silent = FALSE, ...) {

  # Setup returned fields --------------

  # Will be filled and returned
  stanBF_obj <- list(model_name=NULL, stanmodel=NULL, stanfit=NULL, df_samples=NULL, stanbridge=NULL, BF=NULL)
  class(stanBF_obj) <- 'stanBF'

  # Parameter validation ------------


  implemented_models <- rstanBF:::env_stanBF$stanBF_model_shortnames

  # Validate model names
  assertthat::assert_that(is.character(model))
  if (!model %in% implemented_models) {
    msg <- paste0('model "', model, '" has not been implemented, must be one of: ', paste_vec(implemented_models))
    rlang::abort(msg, 'stanBF_model_not_implemented')
  }

  # Load Stan compiled modules
  module_file <- rstanBF:::env_stanBF$stanBF_modules[[model]]
  stanBF_obj$model_name <- rstanBF:::env_stanBF$stanBF_model_names[[model]]

  # Assign S3 inheritance if available
  # Notice the order: child class before the parent class, else the method is never called.
  if (!is.null(env_stanBF$stanBF_child_class[[model]])){
    class(stanBF_obj) <- c(env_stanBF$stanBF_child_class[[model]], class(stanBF_obj))
  }

  # Validate data requirements
  required_data <- c('mtx', 'idx.ref', 'idx.quest')
  assertthat::assert_that(is.list(data))
  if (!all(required_data %in% names(data))) {
    msg <- paste0('one of data variables is missing, must have all of: "', paste_vec(required_data), '"\n have: "', paste_vec(names(data)), '"')
    rlang::abort(msg, 'stanBF_data_missing_var')
  }

  # Validate hyperprior requirements
  assertthat::assert_that(is.list(hyperpriors))

  default_hyperpriors <- rstanBF:::env_stanBF$stanBF_default_hyperpriors[[model]]

  if (!all(default_hyperpriors %in% names(hyperpriors))) {
    msg <- paste0('one of the hyperparameters is missing: have: "', paste_vec(names(hyperpriors)), '", required: "', paste_vec(default_hyperpriors), '"')
    rlang::abort(msg, 'stanBF_data_missing_hyperparameter')
  }

  # Process data ---------------------

  # Data, fixed
  p <- ncol(data$mtx)
  n.ref <- length(data$idx.ref)
  n.quest <- length(data$idx.quest)

  # Reference and questioned indexes should constitute a partition of all available samples
  # assertthat::assert_that(length(setdiff(setdiff(1:n, data$idx.ref), data$idx.quest)) == 0)

  if (length(intersect(data$idx.ref, data$idx.quest)) != 0) {
    msg <- 'Reference and questioned samples should not be intersecting.'
    rlang::abort(msg, 'stanBF_intersecting_ref_quest')
  }

  idx.H1 <- union(data$idx.ref, data$idx.quest)
  n.H1 <- length(idx.H1)

  default_data_H1 <- list(
    n_ref = n.H1,
    d_ref = data$mtx[idx.H1, ],
    p = p
  )
  default_data_H2 <- list(
    n_ref = n.ref,
    d_ref = data$mtx[data$idx.ref, ],
    n_quest = n.quest,
    d_quest = data$mtx[data$idx.quest, ],
    p = p
  )


  # Merge and overwrite hyperparameters, if passed
  data_H1 <- utils::modifyList(default_data_H1, hyperpriors)
  data_H2 <- utils::modifyList(default_data_H2, hyperpriors)

  # Inject additional data, if exists
  if (!is.null(data_other)) {
    data_H1 <- utils::modifyList(data_H1, data_other)
    data_H2 <- utils::modifyList(data_H2, data_other)
  }

  # Simulation parameters
  default_iter <- list(iter = n.iter, warmup = n.burnin, chains = n.chains, cores = n.cores)

  # Begin computation code -----------------------------------------------------------------

  stanmodel_h1 <- stanmodels[[module_file$H1]]
  stanmodel_h2 <- stanmodels[[module_file$H2]]
  stanBF_obj$stanmodel <- list(H1 = stanmodel_h1, H2 = stanmodel_h2)

  # Fitting -----------------------------------------------------------------

  # Hypotheses
  stanfit_h1 <- rstan::sampling(
    object = stanmodel_h1,
    data = data_H1,
    iter = default_iter$iter,
    warmup = default_iter$warmup,
    chains = default_iter$chains,
    cores = default_iter$cores,
    show_messages = !silent, ...)

  stanfit_h2 <- rstan::sampling(
    object = stanmodel_h2,
    data = data_H2,
    iter = default_iter$iter,
    warmup = default_iter$warmup,
    chains = default_iter$chains,
    cores = default_iter$cores,
    show_messages = !silent, ...)

  stanBF_obj$stanfit <- list(H1 = stanfit_h1, H2 = stanfit_h2)

  if (stanfit_h1@mode == 2L) {
    rlang::abort('Sampling under H1 failed: Stan generated no samples', 'stanBF_sampling_failure')
  }
  if (stanfit_h2@mode == 2L) {
    rlang::abort('Sampling under H2 failed: Stan generated no samples', 'stanBF_sampling_failure')
  }

  # Sample extraction --------------------------------------------------------
  # Only for Dirichlet likelihoods

  df_samples <- samples(stanBF_obj)
  if (is.null(df_samples)) {
    df_samples <- NA
  }
  stanBF_obj[['df_samples']] <- df_samples

  # Bridge sampling --------------------------------------------------------

  if (!silent) cat('Bridge sampling...\n')

  # Hypotheses
  bridge_h1 <- bridgesampling::bridge_sampler(stanfit_h1, silent=TRUE, cores=default_iter$cores, method = "normal")
  bridge_h2 <- bridgesampling::bridge_sampler(stanfit_h2, silent=TRUE, cores=default_iter$cores, method = "normal")

  stanBF_obj$stanbridge <- list(H1=bridge_h1, H2=bridge_h2)

  if (!silent) cat('Finished.\n')
  BF.stan <- bridgesampling::bf(bridge_h1, bridge_h2, log = FALSE)

  stanBF_obj$BF <- BF.stan$bf

  stanBF_obj
}


# Print methods -------------------------------------------

#' Print a `stanBF` object
#'
#' @param x a `stanBF` object
#' @param verbose print more details
#' @export
print.stanBF <- function(x, verbose = FALSE) {

  if (verbose) {
    print.default(x)
  } else {
    cat('stanBF object containing posterior samples from H1, H2.\n')

    n.chains <- length(x$stanfit$H1@stan_args)
    n.iter <- x$stanfit$H1@stan_args[[1]]$iter
    cat('Model:', x$model_name, '\n')
    cat('Obtained BF:', x$BF, '\n')
    cat('Ran with', n.chains, 'chains,', n.iter, 'HMC iterations.\n')
  }
}


# Diagnostics -------------------------------------------------------------

