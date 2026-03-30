# Collapse infrequent categorical levels

`step_other2()` creates a *specification* of a recipe step that will
potentially pool infrequently occurring values into an `"other_ml"`
category.

## Usage

``` r
step_other2(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.05,
  other = "other_ml",
  single_low_level = c("as-is", "rename"),
  objects = NULL,
  skip = FALSE,
  id = recipes::rand_id("other2")
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

- threshold:

  A numeric value between 0 and 1, or an integer greater or equal to
  one. If less than one, then factor levels with a rate of occurrence in
  the training set below `threshold` will be pooled to `other`. If
  greater or equal to one, then this value is treated as a frequency and
  factor levels that occur less than `threshold` times will be pooled to
  `other`.

- other:

  A single character value for the other category, defaults to
  `"other_ml"`.

- single_low_level:

  character controlling handling of a single low rate/frequency class.
  Defaults to 'as-is', where data is unmodified if only a single level
  meets the criterion for pooling. This is different from
  [`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)'s
  behavior ('rename'), where the low rate/frequency class would not be
  pooled with other classes but renamed to `other`

- objects:

  A list of objects that contain the information to pool infrequent
  levels that is determined by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

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

## Details

The overall proportion (or total counts) of the categories are computed.
The `other` category is used to pool any two or more categorical levels
whose individual proportion (or frequency) in the training set is less
than `threshold`.

If no pooling is done the data are unmodified (although character data
may be changed to factors based on the value of `strings_as_factors` in
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)/[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)).
Otherwise, a factor is always returned with different factor levels.

If `threshold` is less than the largest category proportion, all levels
except for the most frequent are collapsed to the `other` level.

If `other_ml` is in the list of discarded levels, no error occurs.

If no pooling is done, novel factor levels are converted to missing. If
pooling is needed, they will be placed into the other_ml category.

When data to be processed contains novel levels (i.e., not contained in
the training set), the other category is assigned.

## Differences to step_other()

- a single class subject to lumping is kept as-is as opposed to renamed
  to `other` (during prep)

- If the level defined in `other` is an original class level that was
  not subject to pooling, the user is informed, but no error is raised.

## TODO check message in test case

- novel factor levels are not pooled with an existing lumped category
  (during bake)

## Tidying

When you
[`recipes::tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `retained` , and
`id`:

- terms:

  character, the selectors or variables selected

- retained:

  character, factor levels not pulled into `other`

- id:

  character, id of this step

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

The underlying operation does not allow for case weights.

## Author

This step is based on
[[`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)](https://github.com/tidymodels/recipes/blob/d269758cb171698f38f376fcd711de941840657f/R/other.R#L1)
with only minor modifications.
