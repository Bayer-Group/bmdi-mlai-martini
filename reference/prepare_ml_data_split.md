# split data in train and test

split data in train and test

## Usage

``` r
prepare_ml_data_split(data, train_prop, strata_trt, seed = NULL, outcome_mode)
```

## Arguments

- data:

  data to split

- train_prop:

  proportion to use for training split, must be in (0.5, 1\]

- strata_trt:

  logical

- seed:

  defaults to NULL

- outcome_mode:

  used in stratification

## Value

a named list containing
