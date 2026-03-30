# Create wide format data following a given spec

Extract and reshape data from a single data set according to the given
specification as created by `adam_spec_*()`. In addition, an (updated)
dictionary is returned along with the md5 checksum of the specified
file.

## Usage

``` r
build_adsl(spec)

build_bds(
  spec,
  dupl_ctrl = list(values_fn = NULL, arrange = NULL),
  names_ctrl = list(clean_fn = ~stringr::str_replace_all(.x, "[:punct:]|[:space:]", "_"),
    names_sep = "_"),
  rm = FALSE
)

build_occds(spec, values_fn_occds = NULL)
```

## Arguments

- spec:

  result of `adam_spec_*()`

- dupl_ctrl:

  bds only. A list with two entries

  - `values_fn` function to handle duplicates in pivoting step. see
    details section for default.

  - `arrange` expression passed to `arrange()` optional sorting of data
    set prior to pivoting, e.g. in order to select the first/last value
    by date. defaults to `NULL`.

- names_ctrl:

  bds only. A list with two entries handling cleaning and renaming of
  columns after pivoting

  - `clean_fn` defaults to
    `stringr::str_replace_all(.x, '[:punct:]|[:space:]', '_')`.

  - `names_sep` defaults to '\_'

- rm:

  bds only. boolean. defaults to FALSE. if TRUE, a repeated measurement
  feature matrix with an additional `.rmtime` column is prepared. Only
  used, if `is.null(spec$rm)`.

- values_fn_occds:

  occds only. function that is used to summarize values in the pivoting
  step, if multiple rows per observation unit are present. If `NULL`,
  the maximum is used for numeric values and the last factor level is
  used for categorical values.

## Value

A list with the following entries

- `data` a tibble in wide format with one row per `id`

- `dict` a tibble listing the distinct combinations of columns `param`,
  `label`, `unit`, `time`, `column`, `source` (if provided).

- `source` a list passing the `file` slot from the given `spec` that the
  created data set is based upon along with the md5 checksum of this
  file if `file` was provided, NULL otherwise

- `flag_table` `build_adsl()` only. flag table is passed from
  `spec$flag_table` slot

## Details

Note that the output dictionary may differ from the dictionary created
by `adam_spec_*()`, as multiple features may be derived from a single
parameter at different time points.

`values_fn` is passed to `pivot_wider()`. The default is
`function(x) {ifelse(all(is.numeric(x)), mean(x, na.rm = TRUE), na.omit(x)[1])}`

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
