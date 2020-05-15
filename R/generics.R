# Generic functions

# Paste and collapse a vector, with separators
paste0_vec <- function(...){ paste(..., collapse = ',') }
paste_vec <- function(...){ paste0(..., collapse = ', ') }

#' ggplot2 labeller function for plotmath labels
#'
#' ggplot2 labeller function for plotmath labels.
#' To be used in ggplot2 scale_*_ functions as the `label` argument.
#'
#' It is useful when breaks are plotmath-ready character vectors.
#' This function correctly formats them in order to be parsed and displayed as expressions.
#'
#' @param labels character vector of labels to be parsed
#' @return a list of `expressions`
#' @export
#' @examples
#' df <- data.frame(
#'    var = c('x[1]', 'x[2]', 'x[3]', 'alpha'),
#'    value = c(1,2,3,4)
#' )
#'
#' library(ggplot2)
#'
#' ggplot(df) +
#'    geom_point(aes(x = var, y = value)) +
#'    scale_x_discrete(label = label_parse)
#'
label_parse <- function(labels){
   parse(text = labels)
}


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


#' Generate variable names representing a range
#'
#' Generated variable names follow the rule "text\[index\]", with index from 1 to p.
#'
#' @param p number of variables
#' @param text variable name (default: `'x'`)
#' @return variable names as strings
#' @keywords internal
fun_var_names <- function(p, text = 'x'){
   paste0(text, '[', seq(p), ']')
}


#' Make a tibble with columns representing a range
#'
#' Make a tibble with columns representing a range.
#' All column will have the same base name, and will follow the format of `'text[col_idx]'`, indexed from 1 to `ncol(x.samples)`.
#'
#' It can be used to convert messy `rstan::extract` output from multidimensional variables, to a more manageable form.
#'
#' @param x.samples a matrix or data.frame which will be converted and re-named to a tibble
#' @param text the base name for the new column names
#' @param ... additional value-name pairs which will be added as new columns ( [tibble::add_column()])
#' @return a tibble
#' @importFrom tibble as_tibble add_column
make_tbl_variable_range <- function(x.samples, text, ...) {

   stopifnot(is.character(text) & length(text) == 1 & nchar(text) > 0)

   stopifnot(is.data.frame(x.samples) | is.matrix(x.samples))
   stopifnot(ncol(x.samples) > 0)

   p <- ncol(x.samples)
   col_names <- fun_var_names(p, text = text)

   colnames(x.samples) <- col_names
   tbl.out <- tibble::as_tibble(x.samples)

   tibble::add_column(tbl.out, ...)
}
