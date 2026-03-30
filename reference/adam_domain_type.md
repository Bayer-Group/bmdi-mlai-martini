# Identify the data set type of ads files by file name

Files are read from the given `path` and file names are matched to their
corresponding ADaM data type (ads, bds or occds) using a look up table.

## Usage

``` r
adam_domain_type(
  path = NULL,
  keep = NULL,
  drop = NULL,
  add_bds = NULL,
  add_occds = NULL,
  quiet = TRUE
)
```

## Arguments

- path:

  ads path to the file of interest

- keep:

  only keep the domains provided, e.g. `keep = 'adsl'`

- drop:

  exclude the domains provided, e.g. `drop = 'adxb'`

- add_bds, add_occds:

  character vector of domain names of type bds or occds that are not
  included in the package library of ADaM types (yet), but should be
  processed as per usual.

- quiet:

  whether to suppress printing info on unknown domains to the console,
  defaults to `TRUE`

## Value

A tibble with one row for each matched `.sas7bdat` file in the specified
folder and the following columns

- file:

  File path of the individual selected files

- type:

  File type: *adsl*, *bds*, *occds* or *none* (if no matches are found
  in the look up table, see `adam_domain_type()`)

- domain:

  Name of the ADaM domain, i.e. the file name without its extension

If unknown domains are found in `path` that cannot be matched to a type,
these can be found in the `unknown_domains` attribute of the outcome
table. In addition, a message is printed to the console, unless `quiet`
is set to `TRUE`.

## Details

The derived information is e.g. used to determine which version of
`adam_spec_*()` and `build_*()` to use for further processing.
Parameters `keep` and `drop` allow control over which files to use and
ignore, resp. (If both are provided, `drop` is ignored and only
information in `keep` is used.)

Without any arguments given, *`adam_domain_type()`* returns the look up
table that is used for determining the type of an ads data set (ads, bds
or occds). The column `domain` does not only contain explicit domains
(e.g. `adqskccq`) for human readability, but also regular expressions
(`adqs.*` matches e.g. `adqskccq`, `adqsnyha`, `adqseq5d`, `adqspad`,
...)

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
