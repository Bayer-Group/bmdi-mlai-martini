# Prepare outcome from adtte for MARTINI

Convenience function to prepare an outcome object from adtte-like data
sets, either for tte or binarized endpoint.

## Usage

``` r
build_out_tte(
  data = NULL,
  file = NULL,
  filter = NULL,
  cut = NULL,
  unit = "unit",
  label = ".out",
  columns = NULL
)
```

## Arguments

- data:

  adtte-like tibble, e.g. output of haven::read_sas('adtte.sas7bdat')

- file:

  path to ADaM-like tte data set, e.g. files/adtte.sas7bdat, ignored if
  data is provided

- filter:

  character vector to be applied to the data set (e.g. to select PARAMCD
  or subset to particular population)

- cut:

  optional numeric, required for binarized version from tte data. Note
  that cut must provided in the same unit as the time column (e.g. AVAL)

- unit:

  only relevant if outcome should be binarized, i.e. if `cut` is
  provided. `unit` should describe the unit of the numeric (time) column
  in data set ass well as of `cut`, and will be included in outcome
  values

- label:

  optional label for the binarized outcome variable, defaults to '.out'

- columns:

  list defining the mapping of required information to columns in the
  data with default entries. See details for defaults.

## Value

A tibble with column `.id` and either an additional character column
`.out` for the binarized version or with the addition of the pair
`.time` and `.status` for tte outcomes.

Observations with missing values in either `.time` or `.status` will be
removed. In this case, a notification with the number of removed
observations will be printed to the console.

Note that sample sizes may differ for binarization and tte outcome, as
subjects are dropped if the observed time is censored and below `cut`.

## Details

Note that `unit` is solely a description to be included in the outcome
value (e.g. "event in first 2 year(s)"). No conversion of the time is
done to `unit`.

Column mapping defaults are chosen to match ADaM adtte data sets:

- `id`:

  `'USUBJID'`

- `time`:

  `'AVAL'`

- `censor`:

  `'CNSR'`

If `status` instead of `censoring` indicator should be used, specify
e.g. status = 'event' (defaults to `NULL`). If both are defined,
`status` is used, `censor` is ignored.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
