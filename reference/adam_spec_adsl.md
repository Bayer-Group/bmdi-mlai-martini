# Create specification object for ADaM data sets of type 'adsl'

Given a file containing an adsl data set, `adam_spec_adsl()` will create
a specification object for use in
[`build_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
to actually create a subset of the data to be used in machine learning.
For adsl specifically, the main task is the identification of noise and
redundancies in the data and the selection of a potentially meaningful
set of columns (returned in `select`) and redundancies in the data.

## Usage

``` r
adam_spec_adsl(
  file = NULL,
  data = NULL,
  id = "USUBJID",
  trt = NULL,
  keep = NULL,
  drop = NULL,
  filter = NULL,
  fct_levels = NULL,
  attach_data = FALSE,
  catalog_file = NULL
)
```

## Arguments

- file:

  the path of the sas(7bdat) or rds file to process

- data:

  tibble with the data in adsl format for which the specification is
  created

- id:

  name of id (e.g. USUBJID, SUBJIDN) column to keep. Highly redundant
  variables will not be included in the suggested set of columns
  returned in `select` (see Details).

- trt:

  column to be used as the treatment variable. All other predefined
  treatment variables (see Details) are added to the `drop_list`. If
  NULL, all treatment variables will be added to the `drop_list`.

- keep, drop:

  columns to be kept/dropped, independent of the technical selection
  process within this function

- filter:

  character vector of filters following
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  syntax for use in
  [`build_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
  (see Details). Defaults to NULL.

- fct_levels:

  optional list of named vectors providing code-decode pairs and/or
  setting the level order (see details section for structure).

- attach_data:

  boolean. attach the imported raw data.

- catalog_file:

  path to the catalog file to be passed to
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html).
  Defaults to NULL. Ignored if `file` is not a sas7bdat file.

## Value

A list containing the following

- `file`, `md5`:

  the name and md5 checksum, resp., of the file the generated spec is
  based upon

- `data`:

  the raw data set if `attach_data`, `NULL` otherwise

- `data_info`:

  a list containing the number of subjects `nsubj` and columns `ncol` in
  the data after applying `filter`

- `type`:

  character string `adsl`, generally giving the type of ADaM data set
  processed (`adsl`/`bds`/`occds`)

- `filter`:

  subset of `filter` that yields non-empty result when applied
  individually (using
  [`check_filter()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_filter.md)

- `select`:

  the suggested list of columns to select from the data set

- `factor_levels`:

  a list containing a factor level code/decode for each column
  identified as a factor

- `flag_table`:

  a tibble with columns id and any columns identified as flag (character
  and matching numeric) based on matching column names or labels

- `id`, `trt`:

  passing unchanged input

- `drop_list`:

  a list containing column names suggested to be dropped with the entry
  name identifying the rationale for the discard

  `drop`

  :   passing the user input `drop`

  `datetime`

  :   date/times columns

  `numcode`

  :   numeric code for another variable (incl numeric flags)

  `flag`

  :   flags (both numeric and character columns), see also `flag_table`

  `combination`, `empty`, `constant`

  :   combined, empty and constant columns, resp.

  `redundancy`

  :   columns with redundant information to `id` and `trt` if provided)

- `spec_id`:

  character string `adsl`, generally the name of the domain

- `dict`:

  a tibble of column names and labels (if present in the data set)

## Details

- *Subject id*:

  Non-numeric columns are recoded as numeric, based on the order in
  which they appear in the data (sorted by `id`). All columns with a
  perfect Spearman correlation to `id` are considered redundant and
  added to the `drop_list`. In addition, all numeric columns with a
  perfect Spearman correlation to RANDDT (if available in the data) are
  also added to the `drop_list`, as well as RANDNO (if present in data).

- *Treatment variable*:

  The predefined list of treatment variables is TRT01A, ARMCD, ARM,
  ACTARM, ACTARMCD, TRT01P, TR01PG1, TR02PG1, TR01AG1, TR02AG1. No more
  than one of these variables will be returned in `select`. Note that
  the chosen treatment representing variable will be renamed to the
  standard '.trt' in
  [`build_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md).

- *Filter check*:

  Filters will be checked against the data and will only be kept if the
  filter would not throw an error and if the resulting data set has
  positive number of rows. See
  [`check_filter()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_filter.md)
  for further details.

- *fct_levels*:

  `adam_spec_adsl()` will try and derive the factor levels from the data
  set by identifying column of code/decode pairs using a simple
  heuristic and any formats present in the optionally provided catalog
  file. For ADaM 2.0, the number of the code/decode column pairs is
  expected to be reduced to a minimum and mainly numeric codes are
  expected to be present. `fct_levels` can be used to ensure that the
  columns are treated as factors in the first place and to provide the
  factor labels manually for interpretability of the results.
  `fct_levels` is provided as a named list, containing one entry per
  factor column, that should be defined/updated. Each entry is a named
  vector with the names being the level names and values the
  corresponding entries used in the actual data.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
