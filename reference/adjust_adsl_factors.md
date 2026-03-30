# Adjust factor (levels) from adsl

Adjust factor (levels) from adsl

## Usage

``` r
adjust_adsl_factors(spec, fctrs, entry = "adsl")
```

## Arguments

- spec:

  object of class `martini_spec`

- fctrs:

  named list (column in data) of named vectors with factor levels
  (values) and labels (names)

- entry:

  name of spec entry to modify, defaults to "adsl"

## Value

the `spec` object with all valid modifications to the factor definitions
applied. The modification of a factor definition is skipped (with info)
if not all factor levels of this factor that were derived from the data
set are included in the modified list of levels. Additional factor level
can be introduced, the user is informed.
