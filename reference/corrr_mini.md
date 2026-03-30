# Correlation matrix in long format

Calculate correlation matrix of a numeric data set and stretch to long
format for convenient filtering..

## Usage

``` r
corrr_mini(x, method = "pearson", use = "pairwise.complete.obs", shave = FALSE)
```

## Arguments

- x:

  A tibble, data frame or matrix containing numeric columns to be
  correlated.

- method, use:

  Arguments that are passed to
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html)

- shave:

  logical. if TRUE, only the lower triangle of the correlation matrix is
  kept.

## Value

A tibble with the pair of variable names in the columns `x` and `y` and
the corresponding correlation in the column `r`

TODO deprecate
