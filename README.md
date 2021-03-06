
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rstanBF

rstanBF computes the Bayes Factor for data in specified two-level
hierarchical models.

[![Build
Status](https://travis-ci.org/lgaborini/rstanBF.svg?branch=master)](https://travis-ci.org/lgaborini/rstanBF)
[![Codecov test
coverage](https://codecov.io/gh/lgaborini/rstanBF/branch/master/graph/badge.svg)](https://codecov.io/gh/lgaborini/rstanBF?branch=master)

It wraps the package
[bridgesampling](https://cran.r-project.org/package=bridgesampling), and
provides methods to compute the Bayes Factor for comparing two sets of
samples.

## Documentation

Documentation for the stable version is available
[here](https://lgaborini.github.io/rstanBF/).

## Installation

rstanBF is not yet available on CRAN.  
You can install the development version from this repository using
`devtools` or `remotes`:

``` r
# install.packages('remotes')
remotes::install_github('lgaborini/rstanBF')
```

## Implemented models

The implemented models are:

  - Dirichlet-Dirichlet
  - Dirichlet-FoldedNormal
  - Dirichlet-DirichletGamma
  - Normal-Normal
