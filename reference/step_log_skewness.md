# Logarithmic transformation based on skewness

`step_log_skewness()` creates a *specification* of a recipe step that
will log transform numeric variables if the skewness exceeds a given
threshold.

## Usage

``` r
step_log_skewness(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  skewness = NULL,
  base = exp(1),
  offset = 0,
  columns = NULL,
  skip = FALSE,
  id = recipes::rand_id("log_skewness")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skewness:

  Numeric threshold for the skewness. If the skewness of a variable
  exceeds this threshold, it will be log-transformed. Otherwise, it will
  remain as-is. If `NULL`, all selected numeric variables will be
  transformed.

- base:

  A numeric value for the base.

- offset:

  An optional value to add to the data prior to logging (to avoid
  `log(0)`).

- columns:

  A character vector of the variable names that are log-transformed.
  This field is a placeholder and will be populated once
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is used.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `base` , and `id`:

- terms:

  character, the selectors or variables selected

- base:

  numeric, value for the base

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`recipes::step_log()`](https://recipes.tidymodels.org/reference/step_log.html)

## Author

Modified from
[`recipes::step_log()`](https://recipes.tidymodels.org/reference/step_log.html).

## Examples

``` r
set.seed(313)
examples <- matrix(exp(rnorm(40)), ncol = 2)
examples <- as.data.frame(examples)

rec <- recipes::recipe(~ V1 + V2, data = examples)

log_trans <- rec |>
  step_log_skewness(recipes::all_numeric_predictors(), skewness = 1)

log_obj <- recipes::prep(log_trans, training = examples)

transformed_te <- recipes::bake(log_obj, examples)
plot(examples$V1, transformed_te$V1)

plot(examples$V2, transformed_te$V2)
```
