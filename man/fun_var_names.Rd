% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{fun_var_names}
\alias{fun_var_names}
\title{Generate variable names representing a range}
\usage{
fun_var_names(p, text = "x")
}
\arguments{
\item{p}{number of variables}

\item{text}{variable name (default: \code{'x'})}
}
\value{
variable names as strings
}
\description{
Generated variable names follow the rule "text[index]", with index from 1 to p.
}
\keyword{internal}
