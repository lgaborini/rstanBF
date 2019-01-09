# Functions to fit Dirichlet distributions and estimate Dirichlet parameters

#' Estimate Dirichlet parameter from a dataframe using ML or naive estimator
#'
#' Assume that data is ~ Dir(alpha).
#'
#' This function returns a matrix containing the estimate for alpha.
#'
#' @param df the dataframe containing only samples
#' @param name_param name of output matrix columns
#' @param use if 'MLE', use MLE estimator, else the sample mean (unbiased for alpha)
#' @return a dataframe with the Dirichlet parameter named columns
#' @export
fun_Dirichlet_MLE_from_single_source <- function(df, name_param = 'theta', use = 'MLE') {

   if (!any(use %in% c('MLE', 'naive'))) stop("use must be either 'MLE' or 'naive'")

   mtx <- df %>% as.matrix()

   # Choose between MLE or sample mean
   fun_est <- NULL
   if (use == 'MLE') {
      fun_est <- function(mtx){
         est <- Compositional::diri.est(mtx, type = 'mle')
         pluck(est, 'param')
      }
   }
   if (use == 'naive') {
      fun_est <- colMeans
   }
   stopifnot(!is.null(fun_est))

   df_MLE <- fun_est(mtx) %>%
      purrr::set_names(rsamplestudy::fun_var_names(p, name_param)) %>% tibble::as_tibble() %>% t() %>% tibble::as_tibble()
   df_MLE
}

# Compute MLE in each source
#' Estimate MLE parameters from a dataframe of Dirichlet samples from different sources
#'
#' Obtain estimates for each source.
#'
#' @param df_samples dataframe of Dirichlet samples
#' @param col_source the column name, unquoted, containing the source column
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang enquo
#' @return a dataframe containing the Dirichlet parameter estimates for each source
#' @export
fun_Dirichlet_MLE_from_samples <- function(df_samples, col_source = source, ...) {

   df_samples %>%
      group_by(!! enquo(col_source)) %>%
      nest() %>%
      mutate(param = map(data, fun_Dirichlet_MLE_from_single_source, ...)) %>%
      select(-data) %>%
      unnest() %>%
      ungroup()
}
