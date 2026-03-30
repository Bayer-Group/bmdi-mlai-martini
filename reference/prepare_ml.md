# Prepare ML ready data set from outcome and predictor data

**\[maturing\]**

Given `feature`, a tibble representing a wide format feature matrix, and
`outcome`, a tibble containing the outcome information
(regression/classification/survival is supported), `prepare_ml()` will
provide data sets suitable for various machine learning problems along
with additional information. The data preparation steps include, but are
not limited to data splitting, handling missing values, normalization,
removal of redundant information (highly correlated features). Please
refer to the Details section for more information.

## Usage

``` r
prepare_ml(
  feature,
  outcome,
  outcome_name = NULL,
  level_order = NULL,
  train_prop = 1,
  strata_trt = TRUE,
  seed = 1130,
  prep_step_log = TRUE,
  prep_step_knnimpute = TRUE,
  prep_step_normalize = TRUE,
  prep_step_corr = TRUE,
  prep_step_dummy = FALSE,
  thres_log = 2,
  thres_corr = 0.9,
  thres_lump = 0.05,
  thres_imp = 0.8,
  thres_nzv_freq = 95/5,
  thres_nzv_unique = 10,
  vars_imp_ignore = c(".trt"),
  vars_fct_expl_na = NULL,
  vars_keep_corr = NULL,
  vars_no_trafo = NULL,
  one_hot = NULL,
  log_base = exp(1),
  corr_method = "spearman",
  outlier_remove = FALSE,
  outlier_ctrl = list(coef = 3),
  custom_recipe = NULL,
  quiet = FALSE,
  check_feature = TRUE,
  prep_recipe = lifecycle::deprecated(),
  vars_ordinalscore = lifecycle::deprecated(),
  thres_count = lifecycle::deprecated()
)
```

## Arguments

- feature:

  feature matrix in wide format, e.g. output object of
  [`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md),
  i.e. containing `.id` column and predictors

- outcome:

  tibble containing `.id` column and the outcome of interest,
  [`prepare_ml_outcome()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml_outcome.md)

- outcome_name:

  single character giving the name of the outcome for regression or
  classification. For survival and repeated measurements analysis
  (classification or regression), resp., a named vector of length two
  needs to be specified,
  `c(.time = "<time-coln>", .status = "<status-coln>")` for survival and
  `c('.rmtime' = "<timepoint-coln>", '.out' = "<endpoint-coln>")` for
  repeated measurements (**\[experimental\]**), resp. See Details
  section.

- level_order:

  level order for a classification outcome. Default `NULL` keeps the
  natural order (only used for classification).

- train_prop:

  the proportion of data to be used for the training set. Has to be in
  \[0.5;1.0\]. Defaults to 1, making use of the full data set.

- strata_trt:

  boolean. Expand default stratum variable (`.out` for classification,
  `.status` for tte, `NULL` for regression) by trt (if character, else
  ignored). Defaults to TRUE.

- seed:

  optionally set a seed before the data splitting.

- prep_step_log, prep_step_knnimpute, prep_step_normalize,
  prep_step_corr, prep_step_dummy:

  logicals determining whether or not the corresponding step function
  should be included in the recipe, possibly specified further using
  additional parameters (`thres_*`, `log_base`, `one_hot`). Please refer
  to the details section for the full list of recipe steps.

- thres_log:

  variables will be log-transformed (with base `log_base`) if
  `prep_step_log = TRUE`, all observations are positive, and
  `e1071::skewness() > thres_log`, where `thres_log` defaults to 2.

- thres_corr:

  if `prep_step_corr = TRUE`, `thres_corr` is passed to
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html)'s
  `threshold` argument with a default of 0.9 to remove highly correlated
  features

- thres_lump:

  threshold used in
  [`step_other2()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/step_other2.md).
  If at least two classes of a factor have low frequencies/proportions,
  they will be lumped into a class other_ml. If only one class is below
  the threshold, data is unmodified and the user is informed. Defaults
  to 0.05. See
  [`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)
  for usage of argument threshold.

- thres_imp:

  Minimal proportion of non-missing data per feature required to be kept
  in the data and completed using
  [`recipes::step_impute_knn()`](https://recipes.tidymodels.org/reference/step_impute_knn.html).
  Variables not meeting the threshold will be dropped and not be
  included in `prep` data entries. Per default `thres_imp = 0.8`, i.e.
  variables will be dropped if the proportion of available data is less
  than 80%. Variables listed in `vars_imp_ignore` will never be imputed,
  observations with missing data in the respective variables will be
  removed.

- thres_nzv_freq, thres_nzv_unique:

  parameters passed to
  [`recipes::step_nzv()`](https://recipes.tidymodels.org/reference/step_nzv.html)
  with defaults `thres_nzv_freq = 95/5)` and `thres_nzv_unique = 10`

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

- corr_method:

  passed to stats::cor(), defaults to `"spearman"` to tailor to use of
  random forests.

- outlier_remove, outlier_ctrl:

  For outcome mode regression only, see
  [`prepare_ml_outcome()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml_outcome.md)
  for details on how outliers are removed from outcome variables.
  `outlier_remove` defaults to FALSE, `outlier_ctrl` to
  `list(coef = 3)`.

- custom_recipe:

  **\[experimental\]** custom, pre-defined
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  that may be provided for data preparation. Defaults to `NULL`,
  yielding `martini`'s default preparation (please refer to the details
  section to learn about the default recipe steps).

- quiet:

  boolean. Suppress messages during outcome preparation to the console
  on NA and outlier removal, resp. Defaults to `FALSE`.

- check_feature:

  logical controlling whether to run basic checks on input of `feature`
  to identify sources for potential downstream issues such as low
  frequency classes in a character/factor column. defaults to `TRUE`.

- prep_recipe:

  **\[deprecated\]** Please use `custom_recipe` argument instead.
  custom, pre-defined
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  may be provided for data preparation. Defaults to NULL, yielding a
  data-driven preparation. Please refer to the details section to learn
  about the individual recipe steps.

- vars_ordinalscore:

  column names of ordinal factor variables to be converted into numeric
  scores (using [`as.numeric()`](https://rdrr.io/r/base/numeric.html)).
  Defaults to `NULL`. **\[deprecated\]**. Please handle factors
  individually prior to calling `prepare_ml()`.

- thres_count:

  **\[deprecated\]** non-negative integer variables with no more than
  `thres_count` distinct values are considered as count variables and
  are excluded from the log-transformation and normalization. Please use
  [`check_count()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_count.md)
  on the feature input to identify variables that resemble counts and
  handle appropriately prior to calling `prepare_ml()`.

## Value

`prepare_ml()` produces an object of class `martini_ml`, a nested list
containing data, `recipe` objects and description of data preparation,
information on the outcome.

### Data sets

The top level entry `data` is a nested list, that contains the data set
both prior to (`raw`) and after (`prep`) application of the specified ML
preparation steps. Both versions are split in `train` and `test` set. If
`train_prop` was set to 1, both `test` slots are `NULL` (i.e. no
splitting was done) and `train` slots contain the full data set.

The slot `outcome` contains a list giving `name`, the standardized names
of the output column in the data sets ( `.out` for
regression/classification, `.time` and `.status` for survival, as well
as a `mode`, character string of the outcome mode
`regression/classification/survival`

The dictionary available as an attribute of `feature` is updated with
information on the outcome variable, any log-transformation as well as
alternative labels (`label2`, `label3`) indicating correlated variable
groups e.g. HB (HCT), where HB is kept for the analysis, HCT was dropped
due to absolute correlation above `thres_corr`. Dictionary is available
from the `dict` slot, `NULL` if no such attribute is defined.

The `source` slot simply passes the `source` attribute of `feature`,
`NULL` if no such attribute is defined. If
[`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md)
from the `martini` package was used to generate `feature`, this
attribute lists the full paths of the files that were used in data
generation of `feature`.

### Data preparation and documentation

Within the `recipe` entry `raw` contains the untrained recipe object
(prior to `prep()`), `prep` contains the fully trained recipe object,
`params` documents the parameters/thresholds used in the data
preparation, giving bare `value` slots, as well as a verbose description
in `text`.

Top level entries `removed` gives a list of removed `rows` and `columns`
along with the information on why/in which recipe step the data was
removed. `high_corr` a tibble listing correlations above `thres_corr`.
`NULL` if `prep_step_corr = FALSE`. `input` a list giving the `martini`
`packageVersion` and a list of (most) input parameters, including the
seed used

## Details

The following order of recipe steps for data preparation will be applied
(if no recipe is provided). The variable sets that a particular step
function will be applied to are determined based on user input and
output of the function
[`prepare_ml_vars()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml_vars.md),
respectively. Further details on particular steps are given below.

- drop variables e.g. not meeting the minimum threshold for non-missing
  data proportion (`step_rm()`) or for variable removal related to the
  `vars_keep_corr` parameter (see below).

- remove observations with missing data in outcome (`step_naomit()`)

- knn imputation on variables with missing values that are not
  explicitly excluded from imputation (`vars_imp_ignore`). Please note,
  that missing values can still occur after imputation if a large
  majority (or all) of the imputing variables are also missing (see
  `?recipes::step_impute_knn()`). Related subjects/observations will be
  removed to obtain a complete data set and listed in removed\$rows of
  the output object.

- omit observations with remaining missing values (i.e. in variables
  that were excluded from imputation and not dropped before)
  (`step_naomit()`)

- removal of near-zero variance variables (`step_nzv()`)

- log-transformation (`step_log()`)

- normalization (`step_normalize()`)

- removal of highly correlated variables (`step_corr()`)

- lumping of low frequency factor levels into a single class
  (`step_other()`)

- transform ordinal factors into numeric variables
  (`step_ordinalscore()`)

- dummy/one hot encoding (`step_dummy()`)

The `vars_keep_corr` parameter allows to prioritize these variables in
the `step_corr()` part of the recipe over the variables that yield high
correlations with them (i.e. exceeding `thres_corr`). This allows to
choose a *representative* from a set of correlated variables that is
e.g. commonly used in the context of the indication or easier to
interpret. Please note, that these imposed restrictions may increase the
total number of removed variables in this step in comparison to the
unrestricted version.

A note on `step_impute_knn()` and the interpretation of the `prep()`ped
recipe: The variables listed for this step are the ones that are
**used** for the imputation step. It does not mean that missing values
in these variables have been or will be imputed. For more details on
this matter please refer to the documentation of tidymodels and the
difference in `prep()` and `bake()`, in particular. For example,
`vars_imp_ignore` includes the standard treatment variable `.trt` by
default to prevent any imputations; however, it will be listed in the
variable set of the `prep()`ped recipe (for older versions of `recipes`
package). Don't panic. \#rtfm.

For repeated measurement analyses, all observations of the same `.id`
will end up the either in the training or test set (using
[`rsample::group_initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)).
Note that the strata argument will be ignored (with a warning) for
versions below 1.1.1. Currently, grouping is not accounted for in
missing value imputation yet.

Specification of `outcome_name` for survival analysis or repeated
measurements: For survival analysis, specify column names for 'time' and
'status' of the `Surv` object:
`c(.time = "<time-coln>", .status = "<status-coln>")`, where `.time` is
numeric and `.status` is binary with 0 coding for censored, and 1 coding
for event. Currently, only right-censoring is supported.

For repeated measurements (experimental), specify `outcome_name` as
`c('.rmtime' = "<timepoint-coln>", '.out' = "<endpoint-coln>")`. The
outcome mode will be guessed as regression or classification according
to the type of the column specified in `.out`.

If `outcome_name = NULL` (default), the first column in `outcome` that's
not `.id` is chosen for `outcome_name` and the outcome mode is guessed
accordingly. Thus, neither survival nor repeated measurement analysis
will ever be guessed.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

## Examples

``` r
# Classification (martini_outc_class) and regression (martini_outc_regr)
prepare_ml(martini_feat, martini_outc_class)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
#> # Object of class 'martini_ml' with classification outcome .out
#> 
#> ── Data set sizes (n × p) 
#>             $train $test
#> data$raw  289 × 27     0
#> data$prep 289 × 25     0
#> 
#> ── (parametrized) Recipe Steps 
#> • filter_missing
#> • log_skewness
#> • impute_knn
#> • nzv
#> • normalize
#> • corr_keep
#> • other2
#> cli-150-1273
prepare_ml(martini_feat, martini_outc_regr)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
#> # Object of class 'martini_ml' with regression outcome .out
#> 
#> ── Data set sizes (n × p) 
#>             $train $test
#> data$raw  289 × 27     0
#> data$prep 289 × 25     0
#> 
#> ── (parametrized) Recipe Steps 
#> • filter_missing
#> • log_skewness
#> • impute_knn
#> • nzv
#> • normalize
#> • corr_keep
#> • other2
#> cli-150-1292

# Survival — outcome_name must be a named vector specifying time and status columns
prepare_ml(martini_feat, martini_outc_surv,
  outcome_name = c(.time = ".time", .status = ".status"))
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
#> # Object of class 'martini_ml' with survival outcome .time and .status
#> 
#> ── Data set sizes (n × p) 
#>             $train $test
#> data$raw  289 × 28     0
#> data$prep 289 × 26     0
#> 
#> ── (parametrized) Recipe Steps 
#> • filter_missing
#> • log_skewness
#> • impute_knn
#> • nzv
#> • normalize
#> • corr_keep
#> • other2
#> cli-150-1311

# With train/test split (80/20)
prepare_ml(martini_feat, martini_outc_class, train_prop = 0.8)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
#> # Object of class 'martini_ml' with classification outcome .out
#> 
#> ── Data set sizes (n × p) 
#>             $train   $test
#> data$raw  229 × 27 60 × 27
#> data$prep 229 × 25 60 × 25
#> 
#> ── (parametrized) Recipe Steps 
#> • filter_missing
#> • log_skewness
#> • impute_knn
#> • nzv
#> • normalize
#> • corr_keep
#> • other2
#> cli-150-1330

# Extract the prepared data from the ml object
get_data(martini_ml_regr, type = "prep")
#> # A tibble: 289 × 25
#>         .id .trt  SEX   RACE    AGE CALCIUM CREAT   GGT   HCT   HDL   LDL MAGNES
#>       <dbl> <fct> <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1   1.75e9 TRT   M     WHITE    85    9.45  1.65  4.73  46.0  34.5  73.5   2.28
#>  2   1.75e9 PLC   F     WHITE    90    9.75  1.14  2.80  38.5  65.9  70.4   1.81
#>  3   1.75e9 TRT   F     WHITE    62    8.95  1.28  3.99  34.9  64.7 109.    2.50
#>  4   1.75e9 TRT   F     ASIAN    86    9.63  1.28  3.78  37.2  66.5  94.6   2.00
#>  5   1.75e9 PLC   F     WHITE    76   10.1   1.15  4.02  42.1  53.6  65.8   2.34
#>  6   1.75e9 PLC   M     WHITE    47    9.32  1.18  3.46  47.7  77.2  51.7   2.11
#>  7   1.75e9 TRT   M     WHITE    53    9.25  2.12  4.55  34.3  58.9  70.2   2.38
#>  8   1.75e9 TRT   M     WHITE    75   10.1   1.33  4.63  39.7  21.9  95.8   1.96
#>  9   1.75e9 PLC   F     WHITE    77    9.02  1.44  3.71  42.2  27.4  76.8   2.14
#> 10   1.75e9 PLC   M     BLACK    88    9.28  1.04  2.99  40.9  37.5  74.5   2.00
#> # ℹ 279 more rows
#> # ℹ 13 more variables: POTASS <dbl>, SODIUM <dbl>, URICAC <dbl>, BMI <dbl>,
#> #   BPDIA <dbl>, BPSYS <dbl>, HR <dbl>, WEIGHT <dbl>,
#> #   atrial_fibrillation <fct>, myocardial_infarction <fct>,
#> #   coronary_artery_disease <fct>, ventricular_tachycardia <fct>, .out <dbl>

# check_feature() is run automatically on the feature input —
# see ?check_feature for a detailed example
check_feature(martini_feat)
#> ℹ Data set was checked for causes for potential downstream issues with ML
#>   preparation.
#> ! Potential issues were identified.
#> • Run check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
```
