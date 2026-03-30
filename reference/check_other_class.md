# Check for occurrence of level that would cause issue with lumping

Check for occurrence of level that would cause issue with lumping

## Usage

``` r
check_other_class(x, other2_class = NULL, quiet = FALSE)
```

## Arguments

- x:

  data set to check

- other2_class:

  name of class to check for. If `NULL` (the default), uses the default
  of
  [`step_other2()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/step_other2.md)'s
  argument `other`.

- quiet:

  whether to suppress printing messages to the console. defaults to
  FALSE.

## Value

invisibly returns a list for downstream use in
[`check_feature()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_feature.md)
