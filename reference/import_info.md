# Import file and collect info

Import file and collect info

## Usage

``` r
import_info(file, catalog_file = NULL)
```

## Arguments

- file:

  filepath

- catalog_file:

  path to the catalog file to be passed to
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html).
  Defaults to `NULL`. Ignored if `file` is not a `.sas7bdat` file.

## Value

list containing data and corresponding md5 sum and file size
