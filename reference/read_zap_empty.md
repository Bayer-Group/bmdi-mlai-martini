# a convenience function combining [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html) with [`haven::zap_empty()`](https://haven.tidyverse.org/reference/zap_empty.html) functionality

a convenience function combining
[`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html)
with
[`haven::zap_empty()`](https://haven.tidyverse.org/reference/zap_empty.html)
functionality

## Usage

``` r
read_zap_empty(data_file, catalog_file = NULL)
```

## Arguments

- data_file:

  sas7bdat file to read

- catalog_file:

  catalog file to read
