# consistent renaming of character vectors/factor levels

consistent renaming of character vectors/factor levels

## Usage

``` r
prepare_replace(x = NULL)
```

## Arguments

- x:

  character vector

## Value

list with the updated x, obtained from call
`stringr::str_replace_all(x, replacement)`, where replacement is
returned as separate same-name list entry
