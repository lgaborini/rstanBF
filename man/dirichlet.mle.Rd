% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sirt_dirichlet.R
\name{dirichlet.mle}
\alias{dirichlet.mle}
\title{Maximum Likelihood Estimation of the Dirichlet Distribution}
\usage{
dirichlet.mle(
  x,
  weights = NULL,
  eps = 10^(-5),
  convcrit = 1e-05,
  maxit = 1000,
  oldfac = 0.3,
  progress = FALSE
)
}
\arguments{
\item{x}{x}
}
\value{
A list with following entries

\code{alpha} Vector of α parameters
\code{alpha0} The concentration parameter $α_0=∑_k α_k$
\code{xsi} Vector of proportions $ξ_k=α_k / α_0$
}
\description{
Maximum likelihood estimation of the parameters of the Dirichlet distribution
}
\keyword{internal}
