# Prepare ML ready outcome data set

Prepares an ML ready outcome data set (used in
[`prepare_ml`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md))

## Usage

``` r
prepare_ml_outcome(
  outcome,
  outcome_name = NULL,
  level_order = NULL,
  outlier_remove = FALSE,
  outlier_ctrl = list(coef = 3)
)
```

## Arguments

- outcome:

  tibble containing `.id` column and the outcome of interest

- outcome_name:

  single character giving the name of the outcome for regression or
  classification. For survival and repeated measurements analysis
  (classification or regression), resp., a named vector of length two
  needs to be specified,
  `c(.time = "<time-coln>", .status = "<status-coln>")` for survival and
  `c('.rmtime' = "<timepoint-coln>", '.out' = "<endpoint-coln>")` for
  repeated measurements, resp. See Details section.

- level_order:

  Level order for a classification outcome. `NULL` keeps the natural
  order (only used for classification).

- outlier_remove:

  Remove outliers in a regression outcome based on the 'boxplot
  definition'. The outlier coefficient can be modified in `outlier_ctrl`
  (only used for regression).

- outlier_ctrl:

  Control list for the outlier removal, if `outlier_remove` is `TRUE`.
  Currently, the list contains only the boxplot outlier coefficient
  `coef`, which defaults to 3.

## Value

A list with the following entries

- outcome:

  The outcome data set containing only the id and one or two columns
  with standardized column names (`.out` for regression or
  classification (with an additional `.rmtime` column in case of
  repeated measurements), `.time` and `.status` for survival).

- outcome_name:

  Named vector with the original name(s) of the outcome variable(s).

- outcome_label:

  Named vector with the labels(s) of the outcome variable(s). If the
  columns of `outcome` do not contain labels, the column name is used
  instead.

- outcome_mode:

  The outcome mode (`regression`, `classification` or `survival`,
  `outcome_mode` is guessed to be either classification or regression if
  a single column was specified as outcome based on the class of the
  column.

- outcome_dict:

  Dictionary tibble for the outcome variable(s). If no label was
  provided for the selected columns, the column name will be reused as
  label in the dictionary.

- na_outcome:

  The IDs of NAs in `outcome`.

- id_outlier:

  The IDs of removed outliers.

## Details

Specification of `outcome_name` for survival analysis or repeated
measurements: For survival analysis, specify column names for 'time' and
'status' of the `Surv` object:
`c(.time = "<time-coln>", .status = "<status-coln>")`, where `.time` is
numeric and `.status` is binary with 0 coding for censored, and 1 coding
for event. Currently, only right-censoring is supported.

For repeated measurements, specify `outcome_name` as
`c('.rmtime' = "<timepoint-coln>", '.out' = "<endpoint-coln>")`. The
outcome mode will be guessed as regression or classification according
to the type of the column specified in `.out`.

If `outcome_name = NULL` (default), the first column that's not `.id` is
chosen for `outcome_name` and the outcome mode is guessed accordingly.
Thus, neither survival nor repeated measurement analysis will ever be
guessed.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
