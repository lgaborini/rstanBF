
# Sample extraction methods -------------------------------

#' Extract posterior samples from a stanBF object
#'
#' @param stanBF a `stanBF` object
#' @param ... other arguments
#' @export
samples <- function(stanBF, ...) {
   UseMethod('samples')
}


samples.default <- function(...) {
   # message('Not implemented.')
   invisible(NULL)
}


#' Extract theta posterior samples for a turn-like object
#'
#' `stanBF_turn` objects share the Dirichlet likelihood, with \eqn{\theta} as prior parameter.
#'
#' This function extract posterior samples for \eqn{\theta}.
#' Also returns the normalized version of them (\eqn{\rho}).
#'
#' @param stanBF a `stanBF_turn` object
#' @rdname stanBF_turn
#' @export
samples.stanBF_turn <- function(stanBF) {

   theta_H1 <- rstan::extract(stanBF$stanfit$H1)$theta_ref
   theta_ref_H2 <- rstan::extract(stanBF$stanfit$H2)$theta_ref
   theta_quest_H2 <- rstan::extract(stanBF$stanfit$H2)$theta_quest

   if (!is.null(theta_H1)) {
      df_theta_H1 <- make_tbl_variable_range(theta_H1, text = 'theta', Hypothesis = 'Hp', Source = 'Both')
   } else {
      df_theta_H1 <- NULL
   }
   if (!is.null(theta_ref_H2)) {
      df_theta_ref_H2 <- make_tbl_variable_range(theta_ref_H2, text = 'theta', Hypothesis = 'Hd', Source = 'Reference')
   } else {
      df_theta_ref_H2 <- NULL
   }
   if (!is.null(theta_quest_H2)) {
      df_theta_quest_H2 <- make_tbl_variable_range(theta_quest_H2, text = 'theta', Hypothesis = 'Hd', Source = 'Questioned')
   } else {
      df_theta_quest_H2 <- NULL
   }

   df_theta_samples <- dplyr::bind_rows(
      df_theta_H1,
      df_theta_ref_H2,
      df_theta_quest_H2
   )

   # Normalize theta[* by their sums, creating rho[*
   # Hackish

   # Suppress CRAN checks
   Iteration <- value <- variable <- value.norm <- variable.norm <- NULL

   tmp <- df_theta_samples %>%
      tibble::rowid_to_column('Iteration') %>%
      tidyr::gather('variable', 'value', dplyr::starts_with('theta[')) %>%
      dplyr::group_by(Iteration) %>% dplyr::arrange(Iteration) %>%
      dplyr::mutate(value.norm = value / sum(value)) %>%
      dplyr::mutate(variable.norm = gsub('theta[', 'rho[', variable, fixed = TRUE))

   . <- NULL   # fix CRAN check
   df_theta_samples <- tmp %>%
      dplyr::select(-value.norm, -variable.norm) %>%
      tidyr::spread(data = ., key = variable, value = value) %>%
      dplyr::inner_join(
         tmp %>%
            dplyr::select(Iteration, variable.norm, value.norm) %>%
            tidyr::spread(data = ., variable.norm, value.norm),
         by = 'Iteration'
      )

   df_theta_samples %>% dplyr::ungroup()
}
