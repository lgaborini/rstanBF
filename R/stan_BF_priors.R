#' Elicit hyperpriors to use for Stan BF
#'
#' @param list_samples a list containing the samples as returned by [rsamplestudy::make_dataset_splits()]
#' @param model the model short name
#' @param mode_hyperparameter how the parameters are estimated
#' @importFrom purrr pluck map
#' @importFrom dplyr select mutate
#' @return a list of hyperprior parameters
#' @export
#' @md
stan_BF_elicit_hyperpriors <- function(list_samples, model, mode_hyperparameter, ...) {

   stopifnot(any(str_detect(mode_hyperparameter, c('ML', 'vague'))))

   stopifnot(any(model %in% env_stanBF$stanBF_model_shortnames))

   # Use only the background data
   df_background <- pluck(list_samples, 'df_background')

   if (model == 'DirDir') {

      fun_estimate_hyper <- function(df_background, ...) {
         if (mode_hyperparameter == 'ML') {
            alpha <- fun_estimate_Dirichlet_hyperparameter(df_background, 'MLE', ...)
         } else {
            alpha <- rep(1, p)
         }
         list(alpha = alpha)
      }

      list_hyper <- fun_estimate_hyper(df_background, ...)
   }

   if (model == 'DirFNorm') {

      fun_estimate_hyper <- function(list_samples, ...) {
         if (mode_hyperparameter == 'ML') {
            stop('Not implemented.')
         } else {
            sigma <- sqrt(pi)/sqrt(2)*rep(1, p)
            mu <- rep(0, p)
         }
         list(sigma = sigma, mu = mu)
      }

      list_hyper <- fun_estimate_hyper(df_background, ...)
   }

   if (model == 'DirDirGamma') {

      fun_estimate_hyper <- function(list_samples, ...) {
         if (mode_hyperparameter == 'ML') {
            stop('Not implemented.')
         } else {
            alpha <- rep(1, p)
            alpha_0 <- 1
            beta_0 <- 1
         }
         list(alpha = alpha, alpha_0 = alpha_0, beta_0 = alpha_0)
      }
   }

   # Estimate the hyperparameters
   list_hyper <- fun_estimate_hyper(df_background, ...)
   list_hyper
}




#' Compute Dirichlet hyperparameter, using 'MLE' or 'naive'
#' 
#' Returns a numeric vector
#' @param df_background
#' @param method 'MLE' or 'naive'
fun_estimate_Dirichlet_hyperparameter <- function(df_background, method) {

   df_sources_MLE <- fun_Dirichlet_MLE_from_samples(df_background, use = 'naive')

   df_sources_MLE %>% mutate(source = 1) %>%
      fun_Dirichlet_MLE_from_samples(use = method, name_param = 'alpha') %>%
      select(-source) %>%
      as.numeric()
}

