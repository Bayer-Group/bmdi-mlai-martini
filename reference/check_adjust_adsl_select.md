# checks for adjustments in `adjust_adsl_select()`

checks for adjustments in
[`adjust_adsl_select()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adjust_adsl_select.md)

## Usage

``` r
check_adjust_adsl_select(spec, add, drop, select, entry = "adsl")
```

## Arguments

- spec:

  object of class `martini_spec`

- add, drop:

  character vectors of columns to add to/discard from current selection

- select:

  character vector of columns to select (override current selection)

- entry:

  name of spec entry to modify

## Value

named list of valid modifications to apply (entries `add`, `drop`,
`select`)
