# identify/categorize columns from adsl

family of helper functions to identify columns to drop from adsl data
set

## Usage

``` r
adsl_identify(
  data,
  dict = NULL,
  type = c("dttm", "constant", "combined", "flag", "factor", "redundant"),
  dict_label = "label",
  dict_param = "param",
  id = "SUBJID",
  trt = "TRT01A",
  black_list = c("RANDNO", "SITEID", "SITENAM", "INVID", "INVNAM")
)

adsl_identify_dttm(data)

adsl_identify_constant(data)

adsl_identify_combined(
  data,
  dict = NULL,
  dict_label = "label",
  dict_param = "param"
)

adsl_identify_redundant(data, id, trt, clmn_flag)

adsl_identify_flag(data, dict, dict_param = "param", dict_label = "label")

adsl_identify_factor(
  data,
  id,
  clmn_flag = NULL,
  dict,
  dict_param = "param",
  dict_label = "label"
)

adsl_identify_factor_data(data)
```

## Arguments

- data:

  adsl-like data set in which to identify particular columns of interest

- dict, dict_param, dict_label:

  dict is `tibble` as created by
  [`adsl_dict()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adsl_dict.md)
  where `dict_param` and `dict_label` indicate the columns in `dict`
  containing for parameter names (column names of `data`) and labels,
  resp.

- type:

  character vector determining the categories of column types to
  identify. defaults all possible categories: `dttm`, `constant`,
  `combined`, `flag`, `factor`, `redundant`

- id, trt:

  user-selected column names in `data` for ID and treatment column,
  defaulting to `USUBJID` and `TRT01A`, resp.

- black_list:

  character vector of columns that should be dropped for most analyses,
  see details.

- clmn_flag:

  (factor and redundants only) character vector of names identified as
  flags

## Value

list with two top level entries, where `to_remove` is a list of column
names from `data` that were identified as candidates for a given
category and `lev_list` a `list` required to set factor level orders.

## Details

Columns meeting the following criteria are returned

`adsl_identify_dttm()`: `methods::is(.x, "Date")` is TRUE, the label
contains strings 'year', 'month', 'day', 'date' or 'time' (not case
sensitive), class is one of 'difftime', 'hms', 'Period', 'POSIXct',
'POSIXt', 'Date'

`adsl_identify_constant()`: identification via
`janitor::remove_empty(which = 'cols')`,
`janitor::remove_constant(na.rm = TRUE)`

`adsl_identify_redundant()`: redundant columns to selected trt and id
columns

`adsl_identify_combined()`: if labels (from dict) contain '/' and all
parts are column names themselves

By default, `black_list` contains `RANDNO`, `SITEID`, `SITENAM`,
`INVID`, `INVNAM`.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
