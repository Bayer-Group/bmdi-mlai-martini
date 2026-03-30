# Create specification object for ADaM data sets of type 'bds'

Given a file containing a bds data set (e.g. adlb or advs),
`adam_spec_bds()` will create a specification object for use in
[`build_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
to prepare the data to be used in machine learning. The main task is to
collect the key columns for reshaping the data into wide format and
prepare the data filter.

## Usage

``` r
adam_spec_bds(
  file = NULL,
  data = NULL,
  id = "USUBJID",
  param = NULL,
  label = NULL,
  unit = NULL,
  time = NULL,
  value = NULL,
  filter = NULL,
  attach_data = FALSE,
  domain = NULL
)
```

## Arguments

- file:

  the path of the sas(7bdat) or rds file to process, ignored if `data`
  is provided

- data:

  tibble with the data in bds format for which the specification is
  created

- id:

  name of id column to be kept and used for merge of data sets

- param:

  name of the column that identifies the parameter. Defaults to `NULL`,
  will be guessed if not set (see Details).

- label:

  name of the column that gives column labels. Defaults to `NULL`.

- unit:

  Defaults to `NULL`, will be guessed if not set (see Details).

- time:

  Defaults to `NULL`, will be guessed if not set (see Details).

- value:

  Defaults to `NULL`, will be guessed if not set (see Details).

- filter:

  character vector of filters to be applied to the bds data set.
  Individual filters will only be considered if the resulting data set
  has positive number of rows. Defaults to `NULL`.

- attach_data:

  boolean. Attach the imported raw data.

- domain:

  character string to be included in dictionary. Automatically derived
  for standard ADaM data sets. If not set for `data` provided,
  dictionary entry will be 'custom'.

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

  character string `bds`, generally giving the type of ADaM data set
  processed (`adsl`/`bds`/`occds`)

- `filter`:

  subset of `filter` that yields valid and non-empty result when applied
  individually (using
  [`check_filter()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_filter.md))

- `id`:

  passing unchanged input

- `param`, `label`, `value`, `unit`, `time`:

  names of the key columns to be used in
  [`build_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
  for reshaping

- `spec_id`:

  character string, generally the name of the domain

- `dict`:

  a tibble with unique combinations within the `param` and `label`
  column (if present in the data set) to be used as a data dictionary

- `dupl_ctrl`:

  a list of length 2 with parameters `values_fn` and `arrange` that are
  passed to
  [`build_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
  to handle pivoting for duplicated values. Both default to NULL.

## Details

Values for arguments `param`, `label`, `unit`, `time` and `value` will
be guessed if not provided. Guess will be the first of the following
options that matches a column name (exact match).

- `param`:

  `PARAMCD`

- `label`:

  `PARAM`

- `time`:

  `AVISIT`, `AVISITN`, `VISIT`, `VISITN`

- `value`:

  `AVAL`, `AVALC`

- `unit`:

  `AVALU`

Function will escape if one of `param` or `value` are neither provided
nor can be guessed. The other columns are optional.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
