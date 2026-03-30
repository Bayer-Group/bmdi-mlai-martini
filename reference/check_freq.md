# Identify factors with low frequency classes

Identify factors with low frequency classes

## Usage

``` r
check_freq(x, thres = NULL, quiet = FALSE)
```

## Arguments

- x:

  data set to check

- thres:

  integer. Factors with at least one class of size smaller than `thres`
  will be identified. Defaults to NULL,

- quiet:

  whether to suppress printing messages to the console. defaults to
  FALSE.

## Value

invisibly returns a list for downstream use in
[`check_feature()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_feature.md)

## Details

If `x` is the output of `\link{prepare_ml}()`, the tibble
`x$data$prep$train` is checked for factors with at least one low
frequency class. Please refer to the package vignette for further
details.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
