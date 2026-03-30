# High correlation filter

`step_corr_keep()` creates a *specification* of a recipe step that will
potentially remove variables that have large absolute correlations with
other variables, keeping just one representative from highly correlated
variable pairs. The choice of representatives can be controlled by
providing a set of variables that should be prioritized.

## Usage

``` r
step_corr_keep(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  use = "pairwise.complete.obs",
  method = "spearman",
  keep = NULL,
  removals = NULL,
  high_corr = NULL,
  skip = FALSE,
  id = recipes::rand_id("corr_keep")
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

  A value for the threshold of absolute correlation values. The step
  will try to remove the minimum number of columns so that all the
  resulting absolute correlations are less than this value.

- use:

  A character string for the `use` argument to the
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) function.

- method:

  A character string for the `method` argument to the
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) function, defaults
  to `spearman`

- keep:

  A character vector, containing variables that should be kept. These
  will be prioritized when selecting a representative from a variable
  pair with an absolute correlation greater than `threshold` (see
  details). If `NULL`, this step is equivalent to
  [`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html).

- removals:

  A character string that contains the names of columns that should be
  removed. These values are not determined until
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is called.

- high_corr:

  A tibble containing all correlations above `threshold`. These values
  are not determined until
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is called.

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

This step can potentially remove columns from the data set. This may
cause issues for subsequent steps in your recipe if the missing columns
are specifically referenced by name. To avoid this, see the advice in
the *Tips for saving recipes and filtering columns* section of
[recipes::selections](https://recipes.tidymodels.org/reference/selections.html).

This step attempts to remove variables to keep the largest absolute
correlation between the variables less than `threshold`.

The filter tries to prioritize predictors for removal based on the
global affect on the overall correlation structure. If you have two
predictors with an absolute correlation above `threshold`, the variable
with the larger average correlation with all other predictors will be
removed, unless it is specified in `keep` as a variable the user wants
to prioritize. If the absolute correlation of two variables in `keep`
exceeds the `threshold`, the variable with the larger average
correlation to the other predictors will be removed and and the user is
informed by a message in the console.

When a column has a single unique value, that column will be excluded
from the correlation analysis. Also, if the data set has sporadic
missing values (and an inappropriate value of `use` is chosen), some
columns will also be excluded from the filter.

The arguments `use` and `method` don't take effect if case weights are
used in the recipe.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected to be removed

- id:

  character, id of this step

## Tuning Parameters

The `threshold` parameter can be tuned.

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

## See also

[`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html)

## Author

Modified from
[`recipes::step_corr()`](https://recipes.tidymodels.org/reference/step_corr.html).

## Examples

``` r
# create a data set
set.seed(1717)
p <- 5
corrm <- matrix(numeric(p^2), ncol = p, nrow = p)
# variable 2 has a higher average correlation 
# than all other variables
corrm[,2] <- corrm[2,] <- .2
# variable 1 and 2 have high correlation
corrm[1,2] <- corrm[2,1] <- .9
diag(corrm) <- 1
X <- MASS::mvrnorm(n = 100, mu = rep(0, p), Sigma = corrm) %>% 
  tibble::as_tibble(.name_repair = ~paste0("V", 1:p))

# apply correlation filter without specifying `keep`
rec_prep <- recipes::recipe(~., data = X) %>% 
  step_corr_keep(
    recipes::all_numeric_predictors(),
    threshold = .8
  ) %>% 
  recipes::prep()

recipes::bake(rec_prep, new_data = NULL)
#> # A tibble: 100 × 4
#>        V1      V3      V4      V5
#>     <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -0.519 -1.99   -0.918  -0.905 
#>  2 -1.48  -1.79    0.271   0.126 
#>  3  1.73   0.847   0.928   0.640 
#>  4 -1.10  -1.02   -0.735   1.38  
#>  5  0.512 -0.292  -0.750   1.03  
#>  6 -1.84   1.48    0.269  -0.0911
#>  7 -1.59  -0.0975 -0.340   1.22  
#>  8 -2.34   0.102  -0.508  -0.343 
#>  9 -0.386  0.183   0.0381  0.172 
#> 10  1.06  -0.749   0.573  -1.84  
#> # ℹ 90 more rows

# make sure that "V2" is kept
rec_keep_prep <- recipes::recipe(~., data = X) %>% 
  step_corr_keep(
    recipes::all_numeric_predictors(),
    threshold = .8,
    keep = "V2"
  ) %>% 
  recipes::prep()

recipes::bake(rec_keep_prep, new_data = NULL)
#> # A tibble: 100 × 4
#>        V2      V3      V4      V5
#>     <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -1.61  -1.99   -0.918  -0.905 
#>  2 -1.72  -1.79    0.271   0.126 
#>  3  2.04   0.847   0.928   0.640 
#>  4 -0.932 -1.02   -0.735   1.38  
#>  5  0.345 -0.292  -0.750   1.03  
#>  6 -1.42   1.48    0.269  -0.0911
#>  7 -1.47  -0.0975 -0.340   1.22  
#>  8 -2.38   0.102  -0.508  -0.343 
#>  9 -0.331  0.183   0.0381  0.172 
#> 10  0.743 -0.749   0.573  -1.84  
#> # ℹ 90 more rows

# inspect high correlations
rec_keep_prep$steps[[1]]$high_corr
#> # A tibble: 2 × 3
#>   x     y         r
#>   <chr> <chr> <dbl>
#> 1 V1    V2    0.863
#> 2 V2    V1    0.863
```
