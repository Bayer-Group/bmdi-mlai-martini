
<!-- README.md is generated from README.Rmd. Please edit that file -->

# martini

<!-- badges: start -->

[![R-CMD-check](https://github.com/bayer-int/bmdi-mlai-martini/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayer-int/bmdi-mlai-martini/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

martini is the data preparation module of the BMDI MLAI pipeline. The
goal of martini is to enable the creation of a machine learning ready
data set for use with the substream modules.

## Installation

You can install the development version of martini like so:

``` r
devtools::install_git(
  "https://gitlab.bayer.com/ahrensmaike/martini_prep.git",
  dependencies    = TRUE,
  build_vignettes = TRUE
)
```

<!-- # TODO -->
<!-- or from artifactory -->

## Example

Please refer to the ‘hands-on’ vignette in the package

``` r
vignette('hands-on', package = 'martini')
```
