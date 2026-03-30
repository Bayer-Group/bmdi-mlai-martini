# Removes columns if options apply

This helper function removes columns based on character vectors.

## Usage

``` r
martini_recipes_remove_cols(new_data, object, col_names = character())
```

## Arguments

- new_data:

  A tibble.

- object:

  A step object.

- col_names:

  A character vector, denoting columns to remove. Will overwrite
  `object$removals` if set.

## Value

`new_data` with column names removed if specified by `col_names` or
`object$removals`.

## Author

this function is basically a copy of recipes::recipes_remove_cols()
