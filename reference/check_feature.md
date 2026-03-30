# check feature matrix

`check_feature()` is by default run in
[`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)
on the input `feature` to notify the user on sources of potential
issues.

## Usage

``` r
check_feature(
  x,
  check_low_freq = TRUE,
  check_other = TRUE,
  check_missing = TRUE,
  check_count = TRUE,
  check_nzv = TRUE,
  quiet = FALSE,
  thres_count = 30,
  thres_low_freq = NULL,
  thres_missing = NULL,
  thres_nzv_freq = NULL,
  thres_nzv_unique = NULL,
  ...
)
```

## Arguments

- x:

  feature matrix to check, such as the output of
  [`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md).

- check_low_freq, check_other, check_missing, check_count, check_nzv:

  logicals to control which checks to include. All default to TRUE.

- quiet:

  logical controlling whether any informative messages are printed to
  the console. Defaults to FALSE.

- thres_count:

  passed to
  [`check_count()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_count.md)

- thres_low_freq:

  passed to
  [`check_freq()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_freq.md)

- thres_missing:

  passed to
  [`check_non_missing()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_non_missing.md)

- thres_nzv_freq, thres_nzv_unique:

  passed to
  [`check_nzv()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_nzv.md)

- ...:

  arguments to be passed to other methods

## Value

a nested list, where each entry has a `vars` entry that contains column
names that do not pass the respective check (if check is included),
character of length 0 for empty sets. NULL if not tested at all.

## Examples

``` r
# check_feature() on martini_feat (package data, few issues expected)
check_feature(martini_feat)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.

# Synthetic data set designed to trigger all checks:
n <- 100
feat_issues <- tibble::tibble(
  .id = 1:n,
  # two levels with n=1, below default threshold
  low_freq   = factor(c(rep("common", n - 2), "rare1", "rare2")),
  # 99/100 values identical -> near-zero variance
  near_const = c(1, rep(2, n - 1)),
  # 25% NAs, below thres_imp = 0.8
  high_miss  = c(rep(NA, 25), runif(n - 25)),
  # non-negative integers, only 3 distinct values
  count_var  = sample(1:3, n, replace = TRUE),
  # already contains the lumping class "other_ml"
  has_other  = factor(c(rep("A", n - 1), "other_ml"))
)
check_feature(feat_issues)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_freq(), check_other_class(), check_non_missing(), check_nzv(), and
#>   check_count() on the input to `prepare_ml()`'s `feature` to learn more.
check_freq(feat_issues)
#> ℹ The following factors have low frequencies (<5) in at least one class:
#>   low_freq and has_other
check_other_class(feat_issues)
#> Low frequency classes may be pooled during ML data prep into a class other_ml
#> in `prepare_ml()`.
#> ℹ Note that other_ml is already a value in column has_other.
#> • See `step_other2()` for details on downstream processing and modify your data
#>   as needed before proceeding.
check_non_missing(feat_issues)
#> ℹ Variables with a high proportion of missing values will be discarded instead
#>   of imputed during ML preparation.
#> ! For the tested threshold of 80% the following variable would be discarded:
#>   high_miss.
#> • See `recipes::step_filter_missing()` for details.
check_nzv(feat_issues)
#> ℹ Variables that are either constant or highly sparse and unbalanced will be
#>   discarded during ML preparation.
#> ! For the tested thresholds of 19 (frequency ratio) and 10 (unique value
#>   percent) the following variables would be discarded: low_freq, near_const,
#>   and has_other.
#> • See `recipes::step_nzv()` for details.
check_count(feat_issues)
#> ℹ Data set contains numeric variables with only positive integer values and few
#>   distinct values: near_const and count_var.
#> • Please check whether conversion to factors is appropriate.
```
