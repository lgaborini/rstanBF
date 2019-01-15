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

