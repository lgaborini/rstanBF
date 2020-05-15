#' Elicit hyperpriors to use for Stan BF
#'
#' @param df_background the dataframe containing background data, with the source column
#' @param model the model short name
#' @param mode_hyperparameter how the parameters are estimated (can be `'ML'` or `'vague'`)
#' @param col_source quoted name of the source column (default: `'source'`)
#' @param ... arguments to hyperprior estimation methods
#' @return a list of hyperprior parameters, as many as expected by [compute_BF_Stan()]
#' @export
#' @md
stanBF_elicit_hyperpriors <- function(df_background, model, mode_hyperparameter, col_source = 'source', ...) {

   assertthat::assert_that(mode_hyperparameter %in% c('ML', 'vague'),
                           msg = paste0('mode_hyperparameter not valid, must be one of: "ML", "vague"'))

   assertthat::assert_that(model %in% rstanBF:::env_stanBF$stanBF_model_shortnames,
                           msg = paste0('model "', model, '" has not been implemented, must be one of: ', paste_vec(rstanBF:::env_stanBF$stanBF_model_shortnames)))

   assertthat::assert_that(col_source %in% colnames(df_background),
                           msg = paste0('cannot find source column "', col_source, '" in the background data.frame'))

   # p variables + the source column
   p <- ncol(df_background) - 1

   # Assign the hyperprior function
   # It MUST return a list of hyperparameters

   if (model == 'DirDir') {
      if (mode_hyperparameter == 'ML') {
         fun_estimate_hyperpriors <- function(df_background, mode_ML = 'ML', ...){
            if (mode_ML == 'naive') {
               alpha <- fun_estimate_DirDir_hyperparameter(df_background, method = 'naive', col_source = col_source, ...)
            } else {
               alpha <- fun_estimate_DirDir_hyperparameter(df_background, method = 'ML', col_source = col_source, ...)
            }

            list(alpha = alpha)
         }
      } else {
         fun_estimate_hyperpriors <- function(df_background,...) {
            alpha <- rep(1, p)
            list(alpha = alpha)
         }
      }
   }
   if (model == 'DirFNorm') {
      if (mode_hyperparameter == 'ML') {
         stop('Not implemented.')
      } else {
         fun_estimate_hyperpriors <- function(df_background,...) {
            sigma <- sqrt(pi)/sqrt(2)*rep(1, p)
            mu <- rep(0, p)
            list(sigma = sigma, mu = mu)
         }
      }
   }

   if (model == 'DirDirGamma') {
      if (mode_hyperparameter == 'ML') {

         fun_estimate_hyperpriors <- function(df_background, ...) {
            l <- fun_estimate_DirDirGamma_hyperparameter(df_background, col_source = col_source, ...)
            list(
               alpha = l$nu_0,
               alpha_0 = l$alpha_0,
               beta_0 = l$beta_0
            )
         }

      } else {

         fun_estimate_hyperpriors <- function(df_background,...) {
            alpha <- rep(1, p)
            alpha_0 <- 1
            beta_0 <- 1
            list(alpha = alpha, alpha_0 = alpha_0, beta_0 = beta_0)
         }
      }
   }

   if (model == 'NormNorm') {
      if (mode_hyperparameter == 'ML') {
         stop('Not implemented.')
      } else {
         fun_estimate_hyperpriors <- function(df_background,...) {
            mu_mu0 <- 0
            mu_sigma0 <- 100
            sigma_mu0 <- 0
            sigma_sigma0 <- 100
            list(mu_mu0 = mu_mu0, mu_sigma0 = mu_sigma0, sigma_mu0 = sigma_mu0, sigma_sigma0 = sigma_sigma0)
         }
      }
   }

   # Estimate the hyperparameters
   list_hyper <- fun_estimate_hyperpriors(df_background,...)
   list_hyper
}



# DirDir model -------------------------------------------------------------



#' Compute Dirichlet hyperparameters, using 'MLE' or 'naive' estimators
#'
#' For the `'DirDir'` model.
#' It computes an estimate using ML estimates of the parameters in each group.
#'
#' Returns a numeric vector.
#'
#' @param df_background dataframe with background data
#' @param method `'ML'` or `'naive'` (see [fun_estimate_Dirichlet_from_samples()])
#' @param col_source name of the source column (default: 'source')
#' @return a numeric vector for the estimated Dirichlet hyperparameter
fun_estimate_DirDir_hyperparameter <- function(df_background, method, col_source = 'source') {

   df_sources_MLE <- fun_estimate_Dirichlet_from_samples(df_background, use = 'ML', col_source = col_source)

   # Use the estimated source parameters as a new data matrix, with the same source
   df_sources_MLE_hyper <- df_sources_MLE
   df_sources_MLE_hyper[, col_source] <- 1

   fun_estimate_Dirichlet_from_samples(df_sources_MLE_hyper, use = method, name_param = 'alpha') %>%
      dplyr::select(-tidyselect::all_of(col_source)) %>%
      as.numeric()
}



# DirDirGamma model -------------------------------------------------------


#' Compute Dirichlet-DirichletGamma hyperparameters, using 'MLE' estimators
#'
#' For the `'DirDirGamma'` model.
#' It computes an estimate using ML estimates of the parameters in each group.
#'
#' Returns a list.
#'
#' @param df_background dataframe with background data
#' @param eps convergence parameters for Dirichlet MLE (see: [dirichlet.mle()])
#' @param convcrit convergence parameters for Dirichlet MLE (see: [dirichlet.mle()])
#' @param col_source name of the source column (default: 'source')
#' @importFrom dplyr group_by_at group_nest mutate vars select
#' @importFrom tidyselect all_of matches
#' @importFrom tidyr unnest_wider
#' @importFrom purrr map map_dbl
#' @importFrom MASS fitdistr
#' @return a numeric vector for the estimated Dirichlet hyperparameter
fun_estimate_DirDirGamma_hyperparameter <- function(df_background, col_source = 'source', eps = 1e-12, convcrit = 1e-8) {

   if (is.null(df_background)) return(NULL)
   stopifnot(is.data.frame(df_background))

   df_diri_MLE <- df_background %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::all_of(col_source))) %>%
      dplyr::group_nest() %>%
      dplyr::mutate(
         df_diri = purrr::map(.data$data, rstanBF:::dirichlet.mle, eps = eps, convcrit = convcrit),
         nu_i = purrr::map(df_diri, 'xsi'),
         alpha_0 = purrr::map_dbl(df_diri, 'alpha0')
      ) %>%
      dplyr::select(-data, -df_diri) %>%
      tidyr::unnest_wider(nu_i)

   colnames(df_diri_MLE) <- stringr::str_replace_all(colnames(df_diri_MLE), pattern = 'x', replacement = 'nu')

   df_nu_i_MLE <- df_diri_MLE %>%
      dplyr::select({{col_source}}, tidyselect::matches('nu\\[.*'))

   df_alpha_0_i_MLE <- df_diri_MLE %>%
      dplyr::select({{col_source}}, alpha_0)

   # Hyperparameters: nu_0

   nu_0_MLE <- df_nu_i_MLE %>%
      select(-{{col_source}}) %>%
      rstanBF::fun_estimate_Dirichlet_from_single_source(name_param = 'nu', use = 'ML', eps = eps, convcrit = convcrit)

   # Hyperparameters: alpha_0, beta_0 from alpha_i_0

   gamma_hyp <- MASS::fitdistr(df_alpha_0_i_MLE$alpha_0, "gamma")$estimate
   alpha_0_ML <- gamma_hyp[['shape']]
   beta_0_ML <- gamma_hyp[['rate']]

   list(
      alpha_0 = alpha_0_ML,
      beta_0 = beta_0_ML,
      nu_0 = as.numeric(nu_0_MLE)
   )

}

