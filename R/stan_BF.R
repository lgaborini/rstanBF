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
#' - $H_1$: samples in `idx.ref` and `idx.quest` come from the same source
#' - $H_2$: samples in `idx.ref` and `idx.quest` come from the different sources
#'
#' Return a `stanBF` object with these properties:
#' - `model_name`
#' - `stanmodel` (named list of Stan models)
#' - `stanfit` (named list of stanfit objects)
#' - `stanbridge` (named list of bridgesampler objects)
#' - `BF` (a double)
#'
#' For Dirichlet likelihoods, the returned object is a `stanBF_turn`, inheriting from `stanBF`.
#' These objects contain also:
#' - `df_samples` (data.frame with posterior samples)
#'
#' The object contains methods to plot and to extract samples.
#'
#' @param data a list containing `mtx`, `idx.ref`, `idx.quest`
#' @param model the model shortname (e.g. `'DirDir'`, `'DirFNorm'`)
#' @param hyperpriors a list containing hyperparameter definitions
#' @param data_other a list containing additional data for $H_1$ and $H_2$ models (default: `NULL`)
#' @param n.iter number of HMC iterations (default: 1000)
#' @param n.burnin number of HMC burn-in iterations (default: 100)
#' @param n.chains number of HMC chains (default: 1)
#' @param n.cores number of cores to use for HMC and bridgesampling (default: 1)
#' @return a `stanBF` object
#' @export
#' @md
compute_BF_Stan <- function(data, model, hyperpriors, data_other=NULL, n.iter = 1000, n.burnin = 100, n.chains = 1, n.cores = 1) {

  # Setup returned fields --------------

  # Will be filled and returned
  stanBF_obj <- list(model_name=NULL, stanmodel=NULL, stanfit=NULL, df_samples=NULL, stanbridge=NULL, BF=NULL)
  class(stanBF_obj) <- 'stanBF'


  # S3 inheritance children classes
  child_class <- list()
  child_class[['DirDir']] <- 'stanBF_turn'
  child_class[['DirFNorm']] <- 'stanBF_turn'
  child_class[['DirDirGamma']] <- 'stanBF_turn'
  # child_class[['logNormNHN']] <- NA

  implemented_models <- rstanBF:::env_stanBF$stanBF_model_shortnames

  # Parameter validation ------------

  paste_vec <- function(...){ paste(..., collapse = ',') }
  paste0_vec <- function(...){ paste0(..., collapse = ', ') }

  # Validate model names
  assertthat::assert_that(is.character(model))
  assertthat::assert_that(model %in% implemented_models,
    msg = paste0('model "', model, '" has not been implemented, must be one of: ', paste_vec(implemented_models)))

  # Load Stan compiled modules
  module_file <- rstanBF:::env_stanBF$stanBF_modules[[model]]
  stanBF_obj$model_name <- rstanBF:::env_stanBF$stanBF_model_names[[model]]

  # Assign inheritance if available
  if (!is.null(child_class[[model]])){
    class(stanBF_obj) <- c(class(stanBF_obj), child_class[[model]])
  }

  # Validate data requirements
  required_data <- c('mtx', 'idx.ref', 'idx.quest')
  assertthat::assert_that(is.list(data))
  assertthat::assert_that(all(required_data %in% names(data)),
                          msg = paste0('one of data variables is missing, must have all of: "', paste_vec(required_data), '"\n have: "', paste_vec(names(data)), '"')
  )

  # Validate hyperprior requirements
  assertthat::assert_that(is.list(hyperpriors))
  default_hyperpriors <- rstanBF:::env_stanBF$stanBF_default_hyperpriors[[model]]
  assertthat::assert_that(all(default_hyperpriors %in% names(hyperpriors)),
                          msg = paste0('one of the hyperparameters is missing: have: "', paste_vec(names(hyperpriors)), '", required: "', paste_vec(default_hyperpriors), '"'))

  # Process data ---------------------

  # Data, fixed
  p <- ncol(data$mtx)
  n.ref <- length(data$idx.ref)
  n.quest <- length(data$idx.quest)

  # Reference and questioned indexes should constitute a partition of all available samples
  # assertthat::assert_that(length(setdiff(setdiff(1:n, data$idx.ref), data$idx.quest)) == 0)

  assertthat::assert_that(length(intersect(data$idx.ref, data$idx.quest)) == 0, msg = 'Reference and questioned samples should not be intersecting.');

  idx.H1 <- union(data$idx.ref, data$idx.quest)
  n.H1 <- length(idx.H1)

  default_data_H1 <- list(
    n_ref = n.H1, d_ref = data$mtx[idx.H1, ], p = p)
  default_data_H2 <- list(
    n_ref = n.ref, d_ref = data$mtx[data$idx.ref, ],
    n_quest = n.quest, d_quest = data$mtx[data$idx.quest, ], p = p)


  # Merge and overwrite hyperparameters, if passed
  data_H1 <- modifyList(default_data_H1, hyperpriors)
  data_H2 <- modifyList(default_data_H2, hyperpriors)

  # Inject additional data, if exists
  if (!is.null(data_other)) {
    data_H1 <- modifyList(data_H1, data_other)
    data_H2 <- modifyList(data_H2, data_other)
  }

  # Simulation parameters
  default_iter = list(iter=n.iter, warmup=n.burnin, chains=n.chains, cores=n.cores)



  # Begin computation code -----------------------------------------------------------------

  stanmodel_h1 <- stanmodels[[module_file$H1]]
  stanmodel_h2 <- stanmodels[[module_file$H2]]
  stanBF_obj$stanmodel <- list(H1=stanmodel_h1, H2=stanmodel_h2)

  # Fitting -----------------------------------------------------------------

  # Hypotheses
  stanfit_h1 <- rstan::sampling(stanmodel_h1, data=data_H1, iter=default_iter$iter, warmup=default_iter$warmup, chains=default_iter$chains, cores=default_iter$cores)
  stanfit_h2 <- rstan::sampling(stanmodel_h2, data=data_H2, iter=default_iter$iter, warmup=default_iter$warmup, chains=default_iter$chains, cores=default_iter$cores)
  stanBF_obj$stanfit <- list(H1=stanfit_h1, H2=stanfit_h2)

  # Sample extraction --------------------------------------------------------
  # Only for Dirichlet likelihoods

  df_samples <- samples(stanBF_obj)
  if (is.null(df_samples)) df_samples <- NA
  stanBF_obj[['df_samples']] <- df_samples

  # Bridge sampling --------------------------------------------------------

  print('Bridge sampling...')

  # Hypotheses
  bridge_h1 <- bridgesampling::bridge_sampler(stanfit_h1, silent=TRUE, cores=default_iter$cores)
  bridge_h2 <- bridgesampling::bridge_sampler(stanfit_h2, silent=TRUE, cores=default_iter$cores)

  stanBF_obj$stanbridge <- list(H1=bridge_h1, H2=bridge_h2)

  print('Finished.')
  BF.stan <- bridgesampling::bf(bridge_h1, bridge_h2)

  stanBF_obj$BF <- BF.stan$bf
  stanBF_obj
}


# Print methods -------------------------------------------

#' Print a `stanBF` object.
#'
#' @param stanBF a `stanBF` object
#' @param verbose print more details
#' @export
print.stanBF <- function(stanBF, verbose=FALSE) {

  if (verbose) {
    print.default(stanBF)
  } else {
    print('stanBF object containing posterior samples from H1, H2.')

    n.chains <- length(stanBF$stanfit$H1@stan_args)
    n.iter <- stanBF$stanfit$H1@stan_args[[1]]$iter
    print(paste('Model:', stanBF$model_name))
    print(paste('Obtained BF:', stanBF$BF))
    print(paste('Ran with', n.chains, 'chains,', n.iter, 'HMC iterations.'))
  }
}

# Sample extraction methods -------------------------------

#' Extract posterior samples from a stanBF object
#'
#' @param x a `stanBF` object
#' @param ...
#' @export
samples <- function(x, ...) {
  UseMethod('samples')
}

# samples.stanBF <- function(...) {
#   print('Not implemented.')
#   invisible(NULL)
# }

#' Exctract theta posterior samples for a turn-like object
#'
#' `stanBF_turn` objects share the Dirichlet likelihood, with $\theta$ as prior parameter.
#'
#' This function extract posterior samples for $\theta$.
#' Also returns the normalized version of them ($\rho$).
#'
#' @param stanBF a `stanBF_turn` object
#' @export
samples.stanBF_turn <- function(stanBF) {
   make_theta_df <- function(x.samples, ...) {
      n.cols <- ncol(x.samples)
      theta.names <- paste0('theta.', 1:n.cols)

      tbl.out <- tibble::as_tibble(x.samples) %>% purrr::set_names(theta.names) %>% tibble::add_column(...)
      tbl.out
   }

   theta_H1 <- rstan::extract(stanBF$stanfit$H1)$theta_ref
   theta_ref_H2 <- rstan::extract(stanBF$stanfit$H2)$theta_ref
   theta_quest_H2 <- rstan::extract(stanBF$stanfit$H2)$theta_quest

   df_theta_samples <- dplyr::bind_rows(
      make_theta_df(theta_H1, Hypothesis = 'Hp', Source = 'Both'),
      make_theta_df(theta_ref_H2, Hypothesis = 'Hd', Source = 'Reference'),
      make_theta_df(theta_quest_H2, Hypothesis = 'Hd', Source = 'Questioned'))

  # Normalize theta.* by their sums, creating rho.*
  # Hackish
  tmp <- df_theta_samples %>%
     tibble::rowid_to_column('Iteration') %>%
     tidyr::gather('variable', 'value', dplyr::starts_with('theta.')) %>%
     dplyr::group_by(Iteration) %>% dplyr::arrange(Iteration) %>%
     dplyr::mutate(value.norm = value / sum(value)) %>%
     dplyr::mutate(variable.norm = gsub('theta.', 'rho.', variable, fixed = TRUE))

  df_theta_samples <- tmp %>%
     dplyr::select(-value.norm, -variable.norm) %>%
     tidyr::spread(data = ., key = variable, value = value) %>%
     dplyr::inner_join(tmp %>% dplyr::select(Iteration, variable.norm, value.norm) %>% tidyr::spread(data = ., variable.norm, value.norm), by = 'Iteration')

  df_theta_samples %>% dplyr::ungroup()
}


# Diagnostics -------------------------------------------------------------



# Plotting methods -------------------------------

#' Plot posterior distributions of a stanBF object.
#'
#' @param x a `stanBF` object
#' @param ...
#' @export
plot_posteriors <- function(x, ...) {
  UseMethod('plot_posteriors')
}

#' Make boxplots for turn-point posteriors.
#'
#' @param obj_turn a supported `stanBF` object
#' @param variable 'theta' or 'rho' (normalized theta)
#' @export
#' @return a ggplot plot
#' @import dplyr tidyr ggplot2
plot_posteriors.stanBF_turn <- function(obj_turn, variable=NULL, type='boxplots') {

  if (type != 'boxplots') error('plot not implemented.')

  default.variables <- c('rho', 'theta')
  if (is.null(variable)) {
    variable <- default.variables[1]
    message(paste0('Missing plotting variable: using ', variable))
  }
  assertthat::assert_that(variable %in% default.variables, msg = paste0('Variable must be in: ', paste0(default.variables, collapse = ', ')))

  n.chains <- length(obj_turn$stanfit$H1@stan_args)
  n.iter <- obj_turn$stanfit$H1@stan_args[[1]]$iter

  obj_turn$df_samples %>%
    group_by(Hypothesis) %>%
    gather('Variable', 'Value', starts_with(paste0(variable, '.'))) %>%
    # mutate(Grouping = paste0(ifelse(Hypothesis == 'Hp', 'H_p', 'H_d'), ', ', tolower(Source))) %>%
    mutate(Grouping = paste0(Hypothesis, ', ', tolower(Source))) %>%
    ggplot() +
    geom_boxplot(aes(x = Variable, y = Value, fill = Grouping) ) +
    ggtitle(bquote(paste(.(obj_turn$model_name), ' model for delays: posterior samples for ', .(variable))),
           subtitle = bquote(paste(.(n.chains), ' chains, ', .(n.iter), ' HMC iterations')) ) +
    labs(x = NULL, y = variable) + scale_y_continuous(limits = c(0,NA), expand = expand_scale(mult = c(0, .1)))
}
