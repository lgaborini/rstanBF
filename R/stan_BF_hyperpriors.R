#' Elicit hyperpriors to use for Stan BF
#'
#' @param df_background the dataframe containing background data, with the source column
#' @param model the model short name
#' @param mode_hyperparameter how the parameters are estimated (can be 'ML' or 'vague')
#' @param col_source name of the source column (default: 'source')
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
                           msg = paste0('cannot find source column "', col_source, '" in the data.frame'))

   # p variables + the source column
   p <- ncol(df_background) - 1

   # Assign the hyperprior function
   # It MUST return a list of hyperparameters

   if (model == 'DirDir') {
      if (mode_hyperparameter == 'ML') {
         fun_estimate_hyperpriors <- function(df_background, mode_ML = 'ML', ...){
            if (mode_ML == 'naive') {
               alpha <- fun_estimate_Dirichlet_hyperparameter(df_background, method = 'naive', col_source = col_source, ...)
            } else {
               alpha <- fun_estimate_Dirichlet_hyperparameter(df_background, method = 'ML', col_source = col_source, ...)
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
         stop('Not implemented.')
      } else {
         fun_estimate_hyperpriors <- function(df_background,...) {
            alpha <- rep(1, p)
            alpha_0 <- 1
            beta_0 <- 1
            list(alpha = alpha, alpha_0 = alpha_0, beta_0 = beta_0)
         }
      }
   }

   # Estimate the hyperparameters
   list_hyper <- fun_estimate_hyperpriors(df_background,...)
   list_hyper
}




#' Compute Dirichlet hyperparameters, using 'MLE' or 'naive' estimators
#'
#' For the 'DirichletDirichlet' model.
#' It computes an estimate using ML estimates of the parameters in each group.
#'
#' Returns a numeric vector.
#'
#' @param df_background dataframe with background data
#' @param method 'ML' or 'naive' (see [fun_estimate_Dirichlet_from_samples()])
#' @param col_source name of the source column (default: 'source')
#' @return a numeric vector for the estimated Dirichlet hyperparameter
fun_estimate_Dirichlet_hyperparameter <- function(df_background, method, col_source = 'source') {

   df_sources_MLE <- fun_estimate_Dirichlet_from_samples(df_background, use = 'ML', col_source = col_source)

   # Use the estimated source parameters as a new data matrix, with the same source
   df_sources_MLE_hyper <- df_sources_MLE
   df_sources_MLE_hyper[, col_source] <- 1

   fun_estimate_Dirichlet_from_samples(df_sources_MLE_hyper, use = method, name_param = 'alpha') %>%
      dplyr::select(-col_source) %>%
      as.numeric()
}

