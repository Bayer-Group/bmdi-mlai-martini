# Extract data from an ml object

Combine split data (training and test, if available) from either
prepared or raw data

## Usage

``` r
get_data(ml_obj, type = c("prep", "raw"), split_id = NULL)
```

## Arguments

- ml_obj:

  ml object as returned by
  [`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)

- type:

  either `prep` or `raw`, determining which state of the data should be
  extracted. Defaults to `prep`.

- split_id:

  column name (character). Add column indicating split origin
  (train/test). Omitted if `NULL` (default).

## Value

result of
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
of data sets in `ml_obj` of the chosen type, either with or without an
added `train_test` column.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
