# Check (near) zero variance

Check (near) zero variance

## Usage

``` r
check_nzv(x, thres_freq = NULL, thres_unique = NULL, quiet = FALSE)
```

## Arguments

- x:

  data set to check

- thres_freq, thres_unique:

  by default (`NULL`), the respective default of
  [`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)
  is used

- quiet:

  whether to suppress printing messages to the console. defaults to
  FALSE.

## Value

invisibly returns a list for downstream use in
[`check_feature()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_feature.md)
