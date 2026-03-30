# Helper for factor (level) adjustment of spec

Helper for factor (level) adjustment of spec

## Usage

``` r
check_adjust_adsl_factors(spec, fctrs, entry = "adsl")
```

## Arguments

- spec:

  object of class martini_spec

- fctrs:

  named list(column in data) of named vectors with factor levels
  (values) and labels (names)

- entry:

  name of spec entry to modify, defaults to 'adsl'

## Value

subset of `fctrs` that are valid to apply, `NULL` if none

## Details

The function checks if the modifications to the factor definitions
provided in `fctrs` are valid:

- The names of `fctrs` top level entries must be present in the data of
  the entry (checked by attached data or dictionary). Else: ignore with
  message

- The factor levels provided must contain all current/known levels.
  Else: ignore with warning.

- New factor levels (not seen in data) can be introduced (with message)

- if only levels are provided, labels are created via
  [`purrr::set_names()`](https://rlang.r-lib.org/reference/set_names.html)
