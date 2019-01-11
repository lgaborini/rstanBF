# Generic functions

# Paste and collapse a vector, with separators
paste0_vec <- function(...){ paste(..., collapse = ',') }
paste_vec <- function(...){ paste0(..., collapse = ', ') }
