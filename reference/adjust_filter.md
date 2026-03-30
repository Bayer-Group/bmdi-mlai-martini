# Adjust spec object filter

Helper function to make adjustments to the filter of the spec object
built by
[`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md)
to be used with the pipe, keeping consistency with other spec entries
(`dict` and `data_info`) and attributes used by
[`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md).

## Usage

``` r
adjust_filter(spec, filter, append = TRUE)
```

## Arguments

- spec:

  `martini_spec` object to modify

- filter:

  character vector of filter conditions

- append:

  logical, if TRUE (default), append `filter` to existing filter(s),
  else replace

## Value

A modified version of `spec` to be used as input to
[`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md)

## Details

The function checks if `filter` can be applied to the data attached to
the spec. If the data is not attached, the `filter` will be added to the
spec as-is.
