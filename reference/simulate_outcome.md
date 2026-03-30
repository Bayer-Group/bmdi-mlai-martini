# Simulate outcome for MLAI pipeline

Simulate regression, classification or survival outcome based on feature
matrix and effect vector

## Usage

``` r
simulate_outcome(
  X,
  beta = NULL,
  type = c("regression", "classification", "survival"),
  ctrl_regr = list(b0 = 0, sd = 0.4),
  ctrl_class = list(prob_ev = 0.5, mult_beta = 1),
  ctrl_surv = list(surv_mean = 18, cens_mean = 18, cens_max = 36, mult_beta = 1, int =
    FALSE)
)
```

## Arguments

- X:

  Prepared feature matrix (as tibble or matrix) with standardized and
  potentially log-transformed numeric variables and dummy-coded
  categorical variables. If interaction effects are desired, a
  corresponding column has to be present in `X` (e.g.
  `X$interaction_A_B = X$A*X$B`). Needs to contain an id column named
  ".id".

- beta:

  Named effect vector with the corresponding effects for the columns in
  `X`. Only non-zero effects have to be specified

- type:

  type of the simulated outcome: "regression", "classification" or
  "survival"

- ctrl_regr:

  list with the settings for regression outcome, simulated by a linear
  regression model

  `b0`

  :   model intercept

  `sd`

  :   standard deviation of the model error term

- ctrl_class:

  list with settings for classification outcome (event vs. no event),
  simulated by a logistic regression model

  `prob_ev`

  :   event probability

  `mult_beta`

  :   enhancement factor for the beta coefficient to control
      signal-to-noise ratio

- ctrl_surv:

  list with settings for right-censored survival outcome, simulated by a
  proportional hazard model with time-constant baseline hazard

  `surv_mean`

  :   mean survival time

  `cens_mean`

  :   mean censoring time, `NULL` for no censoring

  `cens_max`

  :   max censoring time, `NULL` for no censoring

  `mult_beta`

  :   enhancement factor for the beta coefficient to control
      signal-to-noise ratio

  `int`

  :   boolean, round the survival times to the next highest integer

## Value

A tibble with `nrow(X)` rows and columns `.id` and `.out` for regression
or classification outcome or columns `.id`, `.time` and `.status` for
survival outcome.

## Authors

Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
