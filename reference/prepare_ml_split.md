# Split a prepared ML data set by factor

**\[maturing\]**

`prepare_ml_split()` allows to split a
[`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md)
object by a factor variable, e.g. treatment. This approach is preferable
over independent preparations of each data part if comparability of
resulting models is required (e.g. between treatment groups or studies).
Note that the data preparation recipe is trained on the complete data
set (instead of independent preparation) and the split happens after
preparation is completed.

## Usage

``` r
prepare_ml_split(ml_obj, by = ".trt")
```

## Arguments

- ml_obj:

  Result of
  [`prepare_ml()`](https://bayer-group.github.io/bmdi-mlai-martini/reference/prepare_ml.md).

- by:

  character. Name of the variable to split the ml object by. Must be a
  factor in `ml_obj$data$raw$train`.

## Value

A named list of length 'number of levels' of the `by` variable where
each entry contains the parts of the `ml_obj` that correspond the
respective factor level. Each entry has the same structure as the
original `ml_obj` and thus can be used in subsequent MARTINI modules. As
the `by` variable is constant in each data part per definition, it is
removed from the prepared data, while being kept in raw versions.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
