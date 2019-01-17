# Generic functions

# Paste and collapse a vector, with separators
paste0_vec <- function(...){ paste(..., collapse = ',') }
paste_vec <- function(...){ paste0(..., collapse = ', ') }


#' Convert a named vector to a tibble
#'
#' Convert a named vector to a tibble.
#' The names must be unique.
#'
#' @param v a named vector
#' @return a tibble
named_vector_to_tibble <- function(v) {
   assertthat::not_empty(names(v))
   assertthat::assert_that(identical(names(v), unique(names(v))), msg = 'column names must not be duplicated')

   tidyr::spread(tibble::enframe(v), 'name', 'value')
}

#' Check if an object is a stanBF object
#'
#' Check if an object is a 'stanBF' object.
#'
#' @param x an object
#' @return TRUE if an object is a 'stanBF' object
#' @export
is.stanBF <- function(x) inherits(x, "stanBF")



#' Make a tibble with columns representing a range
#'
#' Make a tibble with columns representing a range.
#' All column will have the same base name, and will follow the format of `'text[col_idx]'`, indexed from 1 to `ncol(x.samples)`.
#'
#' It can be used to convert messy `rstan::extract` output from multidimensional variables, to a more manageable form.
#'
#' @param x.samples a matrix, data.frame or text which will be converted and re-named to a tibble
#' @param text the base name for the new column names
#' @param ... additional value-name pairs which will be added as new columns ( [tibble::add_column()])
#' @return a tibble
make_tbl_variable_range <- function(x.samples, text, ...) {

   p <- ncol(x.samples)
   col_names <- rsamplestudy::fun_var_names(p, text = text)

   tbl.out <- tibble::as_tibble(x.samples) %>% purrr::set_names(col_names) %>% tibble::add_column(...)
   tbl.out
}
