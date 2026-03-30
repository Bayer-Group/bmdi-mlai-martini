# Check role specification for ADaM data set

Checks, if provided column for a role in an ADaM data set is present in
the data. If no column is provided, it is guessed based on ADaM
standards.

## Usage

``` r
check_and_guess_column(
  data,
  role,
  column_spec = NULL,
  type = c("adsl", "bds", "occds"),
  spec_id = NULL,
  required = TRUE,
  call = rlang::caller_env()
)
```

## Arguments

- data:

  data set to check

- role:

  character. the role to check, e.g. "param", "id", "value" or "time"

- column_spec:

  character. the selected column name. will be for presence in `data`and
  type. If `NULL` (the default), it will be guessed based on `domain` or
  `type`.

- type:

  character. either "bds" or "occds"

- spec_id:

  character. an optional id for the specification that is used for
  informative warnings

- required:

  boolean. `TRUE`, if `role` is required, `FALSE` if optional.

- call:

  the execution environment of a currently running function.

## Value

A list with `role`, the column name `column` or `NULL` (if column check
was not successful or no column for `role` could be guessed), `required`
(passed as-is from the input) and a boolean `check_passed`, indicating,
if all checks on the column have passed. Throws an informative warning
if any check fails.
