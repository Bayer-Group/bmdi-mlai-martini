# Stretch a correlation matrix to long format

Stretch a correlation matrix from
[`stats::cor()`](https://rdrr.io/r/stats/cor.html) to a long format
tibble

## Usage

``` r
corr_stretch(x, shave = FALSE)
```

## Arguments

- x:

  a symmetric matrix containing the correlations

- shave:

  logical. if TRUE, only the lower triangle of the correlation matrix is
  kept.

## Value

A tibble with the pair of variable names in the columns `x` and `y` and
the corresponding correlation in the column `r`
