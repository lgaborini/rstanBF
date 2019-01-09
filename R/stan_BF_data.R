#' Convert the output of rsamplestudy to rstanBF data parameter
#'
#' @param list_pop the output of a `rsamplestudy`-generated population
#' @param list_samples the output of [rsamplestudy::make_dataset_splits()]
#' @return a list containing `mtx`, `idx.ref`, `idx.quest`
#' @export
stanBF_prepare_rsamplestudy_data <- function(list_pop, list_samples) {

   mtx_samples <- list_pop$df_pop %>% select(-source) %>% as.matrix

   list_data <- list(mtx = mtx_samples, idx.ref = list_samples$idx_reference, idx.quest = list_samples$idx_questioned)
   list_data
}
