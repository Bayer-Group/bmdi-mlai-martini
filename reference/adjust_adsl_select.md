# Adjust column selection from data sets of type 'adsl' in spec object

Helper function to make common adjustments to the spec object built by
`\link{adam_spec}()` for data sets of type 'adsl' to be used with the
`%>%`.

## Usage

``` r
adjust_adsl_select(
  spec,
  add = NULL,
  drop = NULL,
  select = NULL,
  entry = "adsl"
)
```

## Arguments

- spec:

  spec object to modify

- add, drop:

  character vector of columns to add to/discard from the automated
  selection that is stored in the `select` entry of the corresponding
  `spec` entry (drop wins over add).

- select:

  character vector of column names to be selected. if not NULL (the
  default), arguments `add` and `drop` will be ignored. Overrides
  `select` entry of the corresponding `spec` entry.

- entry:

  name of list element to modify in the spec, defaults to "adsl"

## Value

A modified version of `spec` to be used as input to `\link{build}()`

## Details

if data is provided, the column names stored in the `select` slot will
be intersected with actually existing column names. User will be
informed in case of misspecifications.

If a trt column shall be used and is provided in the `select` slot, the
user has to make sure to update the `trt` slot accordingly in
[`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md).

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
