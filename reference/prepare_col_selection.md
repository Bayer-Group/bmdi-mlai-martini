# Prepare column selection

Prepare column selection in `adam_spec_*()` functions for output. This
includes checks for column presence and guessing of column names based
on ADaM standards and transformation into a standard format for further
use within the `adam_spec_*()` functions.

## Usage

``` r
prepare_col_selection(
  data,
  ...,
  type = c("adsl", "bds", "occds"),
  call = rlang::caller_env()
)
```

## Arguments

- data:

  data set to check

- ...:

  objects containing the column names for the roles in the data set
  following the standard naming convention in the `adam_spec_*()`
  functions (`id`, `value`, etc.).

- type:

  character. either "bds" or "occds"

- call:

  the execution environment of a currently running function.

## Value

A list with `col_select` containing the column names for the roles in
the data set and `use_for_build` indicating, if all checks on the
columns have passed.
