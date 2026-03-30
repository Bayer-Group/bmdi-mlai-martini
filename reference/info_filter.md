# Extract filter info from a spec object

Extract applied filters and (optionally) compare to reference set

## Usage

``` r
info_filter(spec, filter = NULL, quiet = FALSE)
```

## Arguments

- spec:

  spec object as returned by
  [`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md)

- filter:

  if not `NULL` (default), applied filters are compared against a
  reference set, identifying filters that cannot be applied to the data
  without an error

- quiet:

  if `TRUE` instead of printing message to console, return list with
  messages on applied and discarded filters. Defaults to `FALSE`.

## Value

List of applied filters by data set is printed to the console. If
`filter` is applied, missing filters are listed, if any are identified.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
