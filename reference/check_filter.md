# Check filter

Check a given (set of) filter(s) against a data set and assess whether
or not a non-empty data set would be returned.

## Usage

``` r
check_filter(data, filter, data_id = NULL)
```

## Arguments

- data:

  data set that the given filter(s) should be applied to

- filter:

  an expression to be used in
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  (either a single filter, or multiple ones separated by ',')

- data_id:

  character to include in warning message to help identify the data sets
  when used in
  [`adam_spec()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec.md).
  defaults to NULL.

## Value

A list with two entries: `individual` for single filter assessment,
`overall_norow` for the combined filter assessment, if all individually
applicable filters are applied to `data`. `Individual` consists of a
list of three logicals per filter, with values TRUE if the application
of the *individual* filter to `data` yields

- is_error:

  an error

- is_norow:

  a tibble with 0 rows

- keep:

  neither of the above

`overall_norow` is TRUE, if the combination of all applicable filters to
`data` results in a 0-row tibble. In this case a warning is thrown.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
