# Create specification object for ADaM data sets of type `occds`

Given a file containing a occds data set (e.g. admh or adcm),
`adam_spec_occds()` will create a specification object for use in
[`build_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
to prepare the data to be used in machine learning. The main task is to
collect the key columns for reshaping the data into wide format and
prepare the data filter.

## Usage

``` r
adam_spec_occds(
  file = NULL,
  data = NULL,
  id = "USUBJID",
  label = NULL,
  value = NULL,
  valuen = NULL,
  filter = NULL,
  count = TRUE,
  attach_data = FALSE
)
```

## Arguments

- file:

  the path of the sas(7bdat) or rds file to process, ignored if `data`
  is provided

- data:

  tibble with the data in occds format for which the specification is
  created

- id:

  name of id column to be kept and used for merge of data sets

- label:

  name of the column that identifies the occurrence labels. Defaults to
  NULL, will be guessed if not set (see Details).

- value:

  optional value column (e.g. AE severity). Defaults to `NULL`, which
  leads to an Y/N coding of the event.

- valuen:

  optional numeric coding column for `value`. Defaults to `NULL`,
  ignored if `value` is `NULL.`

- filter:

  character vector of filters to be applied to the bds data set.
  Individual filters will only be considered if the resulting data set
  has positive number of rows. Defaults to `NULL`.

- count:

  boolean, defaults to `FALSE`.

- attach_data:

  boolean. attach the imported raw data in `data` slot of output object

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

  character string `occds`, generally giving the type of ADaM data set
  processed (`adsl`/`bds`/`occds`)

- `filter`:

  subset of `filter` that yields valid and non-empty result when applied
  individually (using
  [`check_filter`](https://bayer-group.github.io/bmdi-mlai-martini/reference/check_filter.md)`())`

- `id`:

  passing unchanged input

- `label`, `value`, `valuen`:

  names of the key columns to be used in
  [`build_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build_x.md)
  for reshaping

- `spec_id`:

  character string, generally the name of the domain

- `dict`:

  a tibble with unique combinations within the `param` and `label`
  column (if present in the data set) to be used as a data dictionary

## Details

For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat',
values for arguments `label` will be guessed if not provided. Please
refer to
[`adam_guess()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_guess.md)
for details on guessing procedure. Function will exit if `label` is
neither provided nor can be guessed. Note that the original values in
the `label` column will end up being the parameter labels, not the
parameters in the ML feature matrix. These might be modified later using
[`make.names()`](https://rdrr.io/r/base/make.names.html) or the like in
[`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md).

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
