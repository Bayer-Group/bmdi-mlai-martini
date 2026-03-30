# create recipe from data

create recipe from data

## Usage

``` r
prepare_ml_recipe(
  data,
  custom_recipe = NULL,
  corr_method = "spearman",
  corr_use = "pairwise.complete.obs",
  thres_list = NULL,
  step_list = NULL,
  vars_imp_ignore = c(".trt"),
  vars_fct_expl_na = NULL,
  vars_keep_corr = NULL,
  vars_no_trafo = NULL,
  one_hot,
  log_base
)
```

## Arguments

- data:

  raw data set to create recipe for

- custom_recipe:

  if `NULL`, recipe will be created

- corr_method, corr_use:

  defaulting to `corr_method` `spearman` and `corr_use`
  `pairwise.complete.obs`

- thres_list, step_list:

  named list objects collecting all threshold values and step selection
  info, resp. Please refer to the documentation of the `thres_*` and
  `prep_step_*` arguments in
  [`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)
  for detailed documentation and list entry names.

- vars_imp_ignore:

  variables that shall not be imputed can be specified in
  `vars_imp_ignore` (vector of column names, defaults to
  `vars_imp_ignore = '.trt'`). Observations with missing values in these
  variables will be removed. Removal is documented in `removed$rows`.

- vars_fct_expl_na:

  column names of factors for which NAs should be treated as an explicit
  factor level. Defaults to `NULL.`

- vars_keep_corr:

  choose these variables over other options when removing variables due
  to high correlation in
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html).
  See
  [`recipes::step_rm()`](https://recipes.tidymodels.org/reference/step_rm.html)
  below for details.

- vars_no_trafo:

  character vector defining variables that should be excluded from
  transformation steps such as log transformation and/or normalization
  (if applicable). Defaults to `NULL`.

- one_hot:

  boolean. passed to
  [`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html)
  to choose one hot encoding over dummy encoding

- log_base:

  base to use for log-transformation in
  [`recipes::step_log()`](https://recipes.tidymodels.org/reference/step_log.html).
  Defaults to *exp(1)*.

## Value

a named list with entries containing

- the unprepared recipe

- the prepared recipe

- info on steps included in the recipe

- a list of relevant variables

- a list of thresholds used

- `high_corr` a tibble listing correlations above `thres_corr`. `NULL`
  if `step_list$prep_step_corr = FALSE`.

## See also

[`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)
