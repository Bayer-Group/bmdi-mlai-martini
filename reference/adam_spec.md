# Creating a specification for building a wide format data set from ADaM data

**\[maturing\]**

`adam_spec()` is a wrapper for the `adam_spec_*()` functions. It creates
a list of specifications on how to extract and process data from ADaM
data sets in a given location. The resulting list can be passed to
[`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md),
where the created specs are applied and the generated data sets are
combined into a single wide format data set.

## Usage

``` r
adam_spec(
  path,
  filter = NULL,
  keep = NULL,
  drop = NULL,
  pre_study = lifecycle::deprecated(),
  attach_data = TRUE,
  id = "USUBJID",
  trt = "TRT01A",
  add_bds = NULL,
  add_occds = NULL,
  file_ext = c("rds", "sas7bdat"),
  fct_levels = NULL,
  catalog_file = NULL
)
```

## Arguments

- path:

  path to a directory containing ads files in `.sas7bdat` or `.rds`
  format

- filter:

  a character vector of conditions to be passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  e.g. regarding visits, treatment arms or parameters. Defaults to
  `NULL`.

- keep, drop:

  character vectors controlling the subset of data sets in the given
  `path` to create the specification for (e.g. `c('adsl', 'advs'))`). If
  both `keep` and `drop` are specified, only `keep` will be used. Both
  default to `NULL`, which means that all (known) domains are included.

- pre_study:

  **\[deprecated\]**. boolean. Include only pre-study events from
  occurrence data sets (see
  [`adam_spec_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_occds.md)
  for details). Defaults to `FALSE`.

- attach_data:

  boolean indicating whether the imported raw data is included in the
  output. Defaults to `TRUE`.

- id, trt:

  id and treatment column names (see e.g.
  [`adam_spec_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_adsl.md)
  for details).

- add_bds, add_occds:

  character vector of domain names of type bds or occds that are not
  included in the package library of ADaM types (yet), but should be
  processed as per usual.

- file_ext:

  only rds and sas7bdat data sets are allowed (e.g. `file_ext = 'rds'`).
  User may select only sas7bdat, only rds or set a priorization rule
  (`file_ext = c('rds', 'sas7bdat')`, see Details). Defaults to c('rds',
  'sas7bdat'), i.e. rds if available, sas7bdat else.

- fct_levels:

  optional list of named vectors providing code-decode pairs and/or
  setting the level order for factors in an adsl data set (see details
  section of
  [`adam_spec_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_adsl.md)
  for structure).

- catalog_file:

  path to the catalog file to be passed to
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html)
  for adsl. Defaults to `NULL`. Ignored if `file` is not a `.sas7bdat`
  file.

## Value

`adam_spec()` returns named list of specifications that can be passed to
the
[`build()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/build.md)
function. Each element contains the specification for a single data set
and is named with the domain abbreviation (e.g. adsl, adlb). The list
can be manually adjusted if required, e.g. adding further specifications
or altering existing ones. See the documentation of the `adam_spec_*()`
for a detailed description of the output object.

## Details

`adam_spec()` matches file names in the given path against an internal
library to decide on which `adam_spec_*()` function to use for which
data set. Only files in the library will be processed, the rest will be
ignored. Names of unprocessed files will be printed to the console. For
those, specifications may be created manually using the appropriate
`adam_spec_*()` function and appended to the specification list created
by `adam_*_spec()`.

By specifying e.g. `file_ext = 'rds'`, only rds data will be considered
for building the specification. To use only sas7bdat, analogously
specify by file extension `file_ext = 'sas7bdat'`. Preferred file types
can be specified using a character vector
`file_ext = c('rds', 'sas7bdat')`: If the same file name is found in
`path` with both extensions, the file with the former extension is used,
the one with the latter ignored. For unambiguous file names (either only
`.sas7bdat` or only `.rds`) both are used.

Individual filters are only applied if the resulting data set has a
positive number of rows (ignoring those causing errors or yielding a
0-row data set).

Please refer to the documentations of the `adam_spec_*()` functions for
full details.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

## See also

[`adam_spec_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_adsl.md),
[`adam_spec_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_bds.md),
[`adam_spec_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_occds.md)

## Examples

``` r
ads_path <- system.file("martini_example_study/ads", package = "martini")
adam_spec(ads_path)
#> ℹ admh: The columns MHOCCUR and MHOCCURN contain N/0 values.
#> • Please check if an additional filter is required such as `--OCCUR == 'Y' |
#>   is.na(--OCCUR)`.
#> 
#>   Content
#>   name type   size nsubj ncol
#>   adsl adsl   128K   320    7
#>   adlb bds   1.31M   289   11
#>   advs bds    448K   289    5
#>   admh occds  192K   320    2
#> 
#>   Key columns used in bds-type data sets
#>   name param   value unit  time  
#>   adlb PARAMCD AVAL  AVALU AVISIT
#>   advs PARAMCD AVAL  AVALU AVISIT
#> 
#>   Key columns used in occds-type data sets
#>   name label  value valuen count
#>   admh MHHLGT NA    NA     TRUE 
```
