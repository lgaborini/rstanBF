# Functions to fit Dirichlet distributions and estimate Dirichlet parameters
#
#



#' Estimate Dirichlet parameter from a dataframe using ML or naive estimator
#'
#' Assume that data is \eqn{X ~ Dir(\theta)} iid.
#' This function estimates \eqn{\theta}.
#'
#' @param df the dataframe, must contain only the samples
#' @param name_param name of output parameter (default: `'theta'`)
#' @param use if `'ML'`, use MLE estimator, else the sample estimators (unbiased for \eqn{\theta}, but not optimal)
#' @param ... additional parameters to the estimation method
#' @return a dataframe (tibble) with the columns named as the Dirichlet parameter
#' @export
#' @importFrom stats cov
#' @md
fun_estimate_Dirichlet_from_single_source <- function(df, name_param = 'theta', use = 'ML', ...) {

   if (!any(use %in% c('ML', 'naive'))) stop("use must be either 'ML' or 'naive'")

   mtx <- as.matrix(df)
   p <- ncol(mtx)

   # Choose between MLE or sample mean
   fun_est <- NULL
   if (use == 'ML') {
      fun_est <- function(mtx, ...){

         # Using package Compositional
         # iterative MLE

         # est <- Compositional::diri.est(mtx, type = 'mle')
         # est$param

         # using sirt:
         est <- dirichlet.mle(mtx, ...)
         est$alpha
      }
   }
   if (use == 'naive') {
      # Use sample estimators
      #
      # Reference:
      # Ng, Kai Wang, Guo-Liang Tian, and Man-Lai Tang, "Dirichlet and Related Distributions: Theory, Methods and Applications", Wiley Series in Probability and Statistics. Chichester, UK: John Wiley & Sons, Ltd, 2011. https://doi.org/10.1002/9781119995784.
      fun_est <- function(mtx) {
         p <- ncol(mtx)
         m <- colMeans(mtx)
         C <- stats::cov(mtx)
         conc_initial <- prod((m * (1-m) / diag(C))[1:(p-1)])^(1/(p-1)) - 1
         alpha_initial <- conc_initial * m
         alpha_initial
      }
   }
   stopifnot(!is.null(fun_est))

   theta_est <- fun_est(mtx, ...)
   theta_est_named <- purrr::set_names(theta_est, paste0(name_param, '[', seq(p), ']'))

   # Convert to tibble
   named_vector_to_tibble(theta_est_named)
}

#' Estimate Dirichlet parameters for samples from multiple sources
#'
#' Estimate MLE parameters from a dataframe of Dirichlet samples from different sources.
#' The sources must be known.
#'
#' Suppose that rows in source i are \eqn{X ~ Dir(\theta_i)} iid.
#' This function estimates \eqn{\theta_i}.
#'
#' @param df dataframe of Dirichlet samples with a source column
#' @param col_source the column name containing the source column
#' @importFrom dplyr group_by_at mutate select ungroup
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @inheritDotParams fun_estimate_Dirichlet_from_single_source -df
#' @return a tibble containing the Dirichlet parameter estimates for each source
#' @export
#' @md
fun_estimate_Dirichlet_from_samples <- function(df, col_source = 'source', ...) {

   assertthat::assert_that(length(col_source) == 1)

   df %>%
      dplyr::group_by_at(.vars = col_source) %>%
      tidyr::nest() %>%
      dplyr::mutate(param = purrr::map(.data$data, fun_estimate_Dirichlet_from_single_source, ...)) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = c(param)) %>%
      dplyr::ungroup()
}
