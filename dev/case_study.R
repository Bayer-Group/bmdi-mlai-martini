
options(pillar.width = 70, pillar.print_min = 5)
library(tidyverse)

library(martini)

# path to the example study included in the package
path_study <- system.file("martini_example_study_rds/ads", package = "martini")

# create specification for the data preparation
data_prep_spec <- adam_spec(
  path = path_study,
  keep = c("adsl", "adlb", "advs", "admh")
)

# prepare relevant data filters
filters <- c(  
  # use only baseline data
  "AVISIT == 'Baseline'",
  # no baseline visit in advs domain, use visit 1 instead
  "AVISIT == 'Visit 1' & ADSNAME == 'ADVS'",
  # ITT population
  "ITTFL == 'Y'",
  "!is.na(TRT01A)",  
  # exclude a single parameter from 'advs'
  "PARAMDC != 'BMI'",
  # account for pre-defined event set
  "MHOCCUR == 'Y'"
)

# create specification again
data_prep_spec <- adam_spec(
  path = path_study,
  keep = c("adsl", "adlb", "advs", "admh"),
  filter = filters
)

data_prep_spec

# inspect specified column selection for 'adsl'
data_prep_spec$adsl$select

data_prep_spec <- data_prep_spec |> 
  # correct version of the filter that could not be applied 
  adjust_filter(filter = c("PARAMCD != 'BMI'")) |> 
  # AGEGR01 is a grouped version of AGE
  adjust_adsl_select(drop = "AGEGR01") |> 
  # we are interested in a finer granularity of the medical history
  adjust_spec("admh", label = "MHDECOD")

data_prep_spec

# build the specification and join the data sets based on 
# the subjects in 'adsl'  
data_feat_wide <- build(data_prep_spec, join = "adsl")

# specify a duplicate handling:
# use the latest available measurement
data_prep_spec <- data_prep_spec %>% 
  adjust_spec(
    entry = "advs", 
    dupl_ctrl = list(
      # in case of duplicated values, use the last one...
      values_fn = \(x) tail(x, 1),
      # ... after sorting by measurement date/time
      arrange = "VSDT"
    )
  )

# build again
data_feat_wide <- build(data_prep_spec, join = "adsl")

# raw feature matrix
data_feat_wide

# dictionary
attr(data_feat_wide, "dict")

# add new variable # https://en.wikipedia.org/wiki/Harris%E2%80%93Benedict_equation
data_feat_mod <- data_feat_wide |> 
  mutate(
    HEIGHT = sqrt(WEIGHT / BMI),
    BMR = (10 * WEIGHT) + (6.25 * HEIGHT) - (5 * AGE) + if_else(SEX == 'M', 5, -161)
  ) 

attr(data_feat_mod, 'dict') <- attr(data_feat_mod, 'dict') |> 
  add_row(
    param = 'HEIGHT',
    label = 'Height', 
    unit  = 'cm'
  ) |> 
  add_row(
    param = 'BMR',
    label = 'Basal metabolic rate (Harris–Benedict equation, 1990)',
    unit  = 'kcal'
  )

attr(data_feat_mod, 'dict') |> tail()

# check for low frequency classes before proceeding to prepare_ml()
check_freq(data_feat_wide, thres = ceiling(nrow(data_feat_wide)/10))

# example classification outcome object
martini_outc_class

# prepare data for ml analysis
data_ml <- prepare_ml(
  # feature data
  feature = data_feat_wide,
  # outcome data
  outcome = martini_outc_regr, 
  outcome_name = ".out",
  # training-test-split
  train_prop = 0.8,
  # keep only one representative of feature-groups with a correlation > 0.5 ...
  thres_corr = 0.5,
  # ... but make sure to keep these
  vars_keep_corr = c("BMI", "HB", "BPSYS"),
  # impute missing values (knn) ...
  prep_step_knnimpute = TRUE,
  # ... but do not impute these
  vars_imp_ignore = c(".trt")
)


# prepared training data
data_ml$data$prep$train

# information on removed rows
data_ml$removed$rows |> 
  map(\(x) if (!is.null(x)) paste(x, collapse = ", ") else NA_character_) |> 
  unlist() |> 
  enframe(name = "reason", value = "identifier")

# information on removed columns
data_ml$removed$cols |>  
  map(\(x) if (!is.null(x)) paste(x, collapse = ", ") else NA_character_) |> 
  unlist() |> 
  enframe(name = "reason", value = "variables")

# variable pairs that exceed the correlation threshold
data_ml$high_corr

# full preparation recipe
data_ml$prep_recipe

data_ml$prep_recipe |> broom::tidy()

data_ml$prep_params |>
  map("text") |>
  compact() |>
  set_names('*') |>
  cli::cli_bullets()
