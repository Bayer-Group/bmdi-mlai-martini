# Check for variables that resemble count variables

Identify variables that only have only (non-negative) integer values and
a relatively small number of distinct values

## Usage

``` r
check_count(x, thres = NULL, non_neg = TRUE, quiet = FALSE)
```

## Arguments

- x:

  data set to check

- thres:

  number of distinct integer values

- non_neg:

  logical controlling whether to only consider variables with
  non-negative values. Defaults to TRUE.

- quiet:

  whether to suppress printing messages to the console. defaults to
  FALSE.

## Value

invisibly returns a list for downstream use in
[`check_feature()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_feature.md)
