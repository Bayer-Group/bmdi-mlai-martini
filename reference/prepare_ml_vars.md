# Prepare ML helper function

Identify variable sets from input matrix that might require extra steps
in data preparation, e.g. skewed variables to be log transformed, counts

## Usage

``` r
prepare_ml_vars(
  data,
  thres_count = NULL,
  thres_log = NULL,
  thres_lump = NULL,
  remove = c(".id", ".out", ".status", ".time")
)
```

## Arguments

- data:

  the data set to be searched for feature sets with specific
  characteristics relevant for further data preparation

- thres_count:

  used to detect integer columns with up to `thres_count` distinct
  values (might be excluded from further processing, e.g. log &
  normalization)

- thres_log:

  threshold for log transformation

- thres_lump:

  proportion threshold for factor lumping; used to detect factors with
  exactly one level having a relative frequency below `thres_lump`

- remove:

  columns to be excluded from all identified sets; defaults to c(".id",
  ".out", ".status", ".time")

## Value

A list with slots specifying the detected variable sets of interest. NA
if required thresholds were not defined; `NULL` if no variables meet the
corresponding criteria.

- count:

  assumed to be counts

- log:

  to be log transformed as the skewness exceeds `thres_log`

- nolump:

  to be excluded from lumping

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
