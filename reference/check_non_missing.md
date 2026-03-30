# Check for the proportion of non-missing values

Check for the proportion of non-missing values

## Usage

``` r
check_non_missing(x, thres = NULL, quiet = FALSE)
```

## Arguments

- x:

  data set to check

- thres:

  Minimum proportion of data available

- quiet:

  whether to suppress printing messages to the console. defaults to
  FALSE.

## Value

invisibly returns a list for downstream use in
[`check_feature()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_feature.md)
