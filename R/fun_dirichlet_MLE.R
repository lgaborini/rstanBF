# Functions to fit Dirichlet distributions and estimate Dirichlet parameters

#' Estimate Dirichlet parameter from a dataframe using ML or naive estimator
#'
#' Assume that data is \eqn{X ~ Dir(\theta)} iid.
#' This function estimates \eqn{\theta}.
#'
#' @param df the dataframe, must contain only the samples
#' @param name_param name of output parameter (default: `'theta'`)
#' @param use if `'MLE'`, use MLE estimator, else the sample mean (unbiased for \eqn{\theta})
#' @return a dataframe (tibble) with the Dirichlet parameter named columns
#' @export
#' @md
fun_Dirichlet_MLE_from_single_source <- function(df, name_param = 'theta', use = 'MLE') {

   if (!any(use %in% c('MLE', 'naive'))) stop("use must be either 'MLE' or 'naive'")

   mtx <- as.matrix(df)

   # Choose between MLE or sample mean
   fun_est <- NULL
   if (use == 'MLE') {
      fun_est <- function(mtx){
         est <- Compositional::diri.est(mtx, type = 'mle')
         purrr::pluck(est, 'param')
      }
   }
   if (use == 'naive') {
      fun_est <- colMeans
   }
   stopifnot(!is.null(fun_est))

   theta_est <- fun_est(mtx)
   theta_est_named <- purrr::set_names(theta_est, paste0(name_param, '[', seq(p), ']'))

   # Convert to tibble
   tibble::as_tibble(t(tibble::as_tibble(theta_est_named)))

}

#' Compute Dirichlet parameter MLE for samples from multiple sources
#'
#' Estimate MLE parameters from a dataframe of Dirichlet samples from different sources.
#' The sources must be known.
#'
#' Suppose that rows in source i are $X ~ Dir(\theta_i)$ iid.
#' This function estimates $\theta_i$.
#'
#' @param df_samples dataframe of Dirichlet samples with a source column
#' @param col_source the column name, unquoted, containing the source column
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang enquo
#' @inheritDotParams fun_Dirichlet_MLE_from_single_source -df
#' @return a dataframe containing the Dirichlet parameter estimates for each source
#' @export
#' @md
fun_Dirichlet_MLE_from_samples <- function(df, col_source = source, ...) {

   df %>%
      group_by(!! enquo(col_source)) %>%
      nest() %>%
      mutate(param = map(data, fun_Dirichlet_MLE_from_single_source, ...)) %>%
      select(-data) %>%
      unnest() %>%
      ungroup()
}
