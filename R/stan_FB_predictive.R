
# Prior and posterior distribution extraction methods ---------------------

#' Extract prior predictive distributions
#'
#' Extract prior predictive distributions.
#' @param stanBF a `stanBF` object
#' @param ... additional parameters
#' @export
prior_pred <- function(stanBF, ...) {
   UseMethod('prior_pred')

   # all_variables <- purrr::map(obj_StanBF$stanfit, names)
   # prior_pred_variables <- purrr::map(all_variables, ~ purrr::keep(., ~ stringr::str_detect(., '^sim_')))

   # purrr::map2(obj_StanBF$stanfit, prior_pred_variables, rstan::extract)
}

#' Extract posterior predictive distributions
#'
#' Extract posterior predictive distributions.
#' @param stanBF a `stanBF` object
#' @param ... additional parameters
#' @export
posterior_pred <- function(stanBF, ...) {
   UseMethod('posterior_pred')
}

#' Extract prior predictive distributions for turn-point posteriors
#'
#' Extract prior predictive distributions for turn-point posteriors.
#'
#' @param stanBF a `stanBF_turn` object
#' @param var_name the base name for output variable columns (default: `'x'`)
#' @param ... additional parameters
#' @return a tibble containing prior predictions across hypotheses and sources
#' @rdname stanBF_turn
#' @export
prior_pred.stanBF_turn <- function(stanBF, var_name = 'x', ...) {

   assertthat::assert_that(assertthat::is.string(var_name), nchar(var_name) > 0)

   sim_d_H1 <- rstan::extract(stanBF$stanfit$H1, pars = 'sim_d_ref')$sim_d_ref
   sim_d_ref_H2 <- rstan::extract(stanBF$stanfit$H2, pars = 'sim_d_ref')$sim_d_ref
   sim_d_quest_H2 <- rstan::extract(stanBF$stanfit$H2, pars = 'sim_d_quest')$sim_d_quest

   df_prior_samples <- dplyr::bind_rows(
      make_tbl_variable_range(sim_d_H1, text = var_name, Hypothesis = 'Hp', Source = 'Both'),
      make_tbl_variable_range(sim_d_ref_H2, text = var_name, Hypothesis = 'Hd', Source = 'Reference'),
      make_tbl_variable_range(sim_d_quest_H2, text = var_name, Hypothesis = 'Hd', Source = 'Questioned')
   )

   df_prior_samples
}

#' Extract posterior predictive distributions for turn-point posteriors
#'
#' Extract posterior predictive distributions for turn-point posteriors.
#'
#' @param stanBF a `stanBF_turn` object
#' @param var_name the base name for output variable columns (default: `'x'`)
#' @param ... additional parameters
#' @return a tibble containing posterior predictions across hypotheses and sources
#' @rdname stanBF_turn
#' @export
posterior_pred.stanBF_turn <- function(stanBF, var_name = 'x', ...) {

   assertthat::assert_that(assertthat::is.string(var_name), nchar(var_name) > 0)

   pred_d_H1 <- rstan::extract(stanBF$stanfit$H1, pars = 'pred_d_ref')$pred_d_ref
   pred_d_ref_H2 <- rstan::extract(stanBF$stanfit$H2, pars = 'pred_d_ref')$pred_d_ref
   pred_d_quest_H2 <- rstan::extract(stanBF$stanfit$H2, pars = 'pred_d_quest')$pred_d_quest

   df_posterior_samples <- dplyr::bind_rows(
      make_tbl_variable_range(pred_d_H1, text = var_name, Hypothesis = 'Hp', Source = 'Both'),
      make_tbl_variable_range(pred_d_ref_H2, text = var_name, Hypothesis = 'Hd', Source = 'Reference'),
      make_tbl_variable_range(pred_d_quest_H2, text = var_name, Hypothesis = 'Hd', Source = 'Questioned')
   )

   df_posterior_samples
}
