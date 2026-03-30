# Prepare bds data for pivoting step in build

Preparation of dataset bds_full as well as parameters to be passed to
pivot_wider in build_bds to allow for appropriate unit testing

## Usage

``` r
pivot_prepare_bds(
  bds_full,
  spec,
  values_fn = NULL,
  arrange = NULL,
  clean_fn = ~stringr::str_replace_all(.x, "[:punct:]|[:space:]", "_"),
  names_sep = "_",
  rm = FALSE
)
```

## Arguments

- bds_full:

  original bds-type data set

- spec:

  Top level entry of an object of class `martini_spec`.

- values_fn:

  function to control duplicate handling in `pivot wider()`. See docu of
  [`build_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
  for details.

- arrange:

  expression to be passed to
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  before pivoting. Relevant in case of duplicates and if `values_fn` is
  sensitive to the order of values to aggregate.

- clean_fn:

  function to clean future column names (after pivoting), defaults to
  `~ stringr::str_replace_all(.x, '[:punct:]|[:space:]', '_')`

- names_sep:

  to be passed to `pivot wider()`. defaults to `_`.

- rm:

  boolean. defaults to FALSE. if TRUE, pivoting for a repeated
  measurement feature matrix with an additional `.rmtime` column is
  prepared. Only used, if `is.null(spec$rm)`.

## Value

A list containing the pivot_wider arguments (pivot_args) as well as the
function to clean column names (clean_fn). The `pivot_args` list
includes the prepared data set (filtered, arranged) as well as
pivot_wider params (key(s), value, values_fn, names_sep)

## Details

Data preparation of bds_full for pivoting includes filtering and
arranging the data set before relevant columns are selected and renamed
using `clean_fn` (`param`, `time` only) If the prepared data set has
more than one level in the `time` column, names_from will be a vector of
the form `c(param, time)`

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
