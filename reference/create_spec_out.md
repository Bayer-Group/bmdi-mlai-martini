# Create output object for build specifications

Create output object for build specifications

## Usage

``` r
create_spec_out(..., type = c("adsl", "bds", "occds"), attach_data = TRUE)
```

## Arguments

- ...:

  output objects

- type:

  character. either "bds" or "occds"

- attach_data:

  boolean indicating whether the imported raw data is included in the
  output. Defaults to `TRUE`.

## Value

Output object of `adam_spec_*()`

## See also

[`adam_spec_adsl()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_adsl.md)
[`adam_spec_bds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_bds.md)
[`adam_spec_occds()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/adam_spec_occds.md)
