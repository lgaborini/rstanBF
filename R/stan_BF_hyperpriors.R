#' Elicit hyperpriors to use for Stan BF
#'
#' @param df_background the dataframe containing background data, as returned by [rsamplestudy::make_dataset_splits()]
#' @param model the model short name
#' @param mode_hyperparameter how the parameters are estimated
#' @return a list of hyperprior parameters
#' @export
#' @md
stan_BF_elicit_hyperpriors <- function(df_background, model, mode_hyperparameter, ...) {

   assertthat::assert_that(mode_hyperparameter %in% c('ML', 'vague'),
                           msg = paste0('mode_hyperparameter not valid, must be one of: "ML", "vague"'))

   assertthat::assert_that(model %in% rstanBF:::env_stanBF$stanBF_model_shortnames,
                           msg = paste0('model "', model, '" has not been implemented, must be one of: ', paste_vec(rstanBF:::env_stanBF$stanBF_model_shortnames)))

   # Assign the hyperprior function

   if (model == 'DirDir') {
      if (mode_hyperparameter == 'ML') {
         fun_estimate_hyper <- function(df_background, ...){
            fun_estimate_Dirichlet_hyperparameter(df_background, 'MLE', ...)
         }
      } else {
         fun_estimate_hyper <- function(df_background,...) {
            alpha <- rep(1, p)
         }
      }
   }
   if (model == 'DirFNorm') {
      if (mode_hyperparameter == 'ML') {
         stop('Not implemented.')
      } else {
         fun_estimate_hyper <- function(df_background,...) {
            sigma <- sqrt(pi)/sqrt(2)*rep(1, p)
            mu <- rep(0, p)
            list(sigma = sigma, mu = mu)
         }
      }
   }

   if (model == 'DirDirGamma') {
      if (mode_hyperparameter == 'ML') {
         stop('Not implemented.')
      } else {
         fun_estimate_hyper <- function(df_background,...) {
            alpha <- rep(1, p)
            alpha_0 <- 1
            beta_0 <- 1
            list(alpha = alpha, alpha_0 = alpha_0, beta_0 = alpha_0)
         }
      }
   }

   # Estimate the hyperparameters
   list_hyper <- fun_estimate_hyper(df_background,...)
   list_hyper
}




#' Compute Dirichlet hyperparameters, using 'MLE' or 'naive'
#'
#' Returns a numeric vector
#' @param df_background dataframe with background data
#' @param method 'MLE' or 'naive'
fun_estimate_Dirichlet_hyperparameter <- function(df_background, method) {

   df_sources_MLE <- fun_Dirichlet_MLE_from_samples(df_background, use = 'naive')

   df_sources_MLE %>% dplyr::mutate(source = 1) %>%
      fun_Dirichlet_MLE_from_samples(use = method, name_param = 'alpha') %>%
      dplyr::select(-source) %>%
      as.numeric()
}

