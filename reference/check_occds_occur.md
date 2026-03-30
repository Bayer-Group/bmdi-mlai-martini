# Check for 'N' values in –OCCUR column in occds

Check for 'N' values in –OCCUR column in occds

## Usage

``` r
check_occds_occur(
  data,
  domain = NULL,
  filters = NULL,
  quiet = FALSE,
  no_char = "N",
  no_num = 0
)
```

## Arguments

- data:

  data set to check

- domain:

  name of data set

- filters:

  filters to be applied before checking for issues. defaults to NULL, in
  which case no filters are applied.

- quiet:

  whether to suppress messaging in the console. defaults to FALSE.

- no_char, no_num:

  values that code for 'no' in pre-specified list of events. defaults to
  `N` and `0` for character and numeric variables, resp.

## Value

invisibly returns character vector of potentially problematic columns.
(Invisible) return value has length 0 if no problems were detected.
