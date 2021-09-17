
library(martini)

# ADS SPEC ####

# path to the sas-files of the example study
ads_path <- system.file("martini_example_study/ads", package = "martini")
if(ads_path == "") ads_path <- "inst/martini_example_study/ads"

filters  <- c(
  # intent-to-treat population
  "ITTFL  == 'Y'",
  # only baseline data
  "AVISIT == 'Baseline'",
  # some MH entries have a Y/N coding (but not all)
  "MHOCCUR != 'N'"
)

martini_spec <- adam_spec(ads_path, filter = filters, attach_data = TRUE, pre_study = TRUE)

martini_spec$admh$count <- FALSE
martini_spec$admh$label <- "MHDECOD"

martini_spec$adsl$select <- martini_spec$adsl$select %>% 
  stringr::str_subset("^AGEGR", negate = TRUE)

usethis::use_data(martini_spec, overwrite = TRUE)

# FEATURE MATRIX ####

martini_feat <- build(martini_spec, join = dplyr::left_join)

usethis::use_data(martini_feat, overwrite = TRUE)

# OUTCOME OBJECTS ####

# ... prepare feature matrix for simulation ####

rec <- recipes::recipe(~., data = martini_feat) %>% 
  recipes::update_role(.id, new_role = "id") %>% 
  recipes::step_impute_knn(recipes::all_predictors()) %>% 
  recipes::step_normalize(recipes::all_numeric(), -recipes::has_role("id")) %>% 
  recipes::step_dummy(recipes::all_nominal(), -recipes::has_role("id")) %>% 
  recipes::prep()

X <- recipes::juice(rec) %>% 
  # prepare linear interaction effect
  dplyr::mutate(int = -.trt_TRT*BMI)

# ... create effect vector ####

b <- ncol(X) %>% numeric() %>% setNames(colnames(X))

b["int"]                      <-  0.5
b["atrial_fibrillation_yes"]  <-  0.2
b["angina_pectoris_yes"]      <-  1.5
b["BPSYS"]                    <-  0.1
b[".trt_TRT"]                 <- -0.5

# intercept
b0 <- -.5

# ... simulate outcome ####

set.seed(1841)

## ... ... classification ####
martini_outc_class <- tibble::tibble(
  X %>% dplyr::select(.id),
  .out = rbinom(n = nrow(X), size = 1, prob = 1/(1 + exp(- b0 - as.matrix(X) %*% b)))
) %>% 
  dplyr::mutate(.out = factor(.out, labels = c("no event", "event")))

## ... ... regression ####
martini_outc_regr <- tibble::tibble(
  X %>% dplyr::select(.id),
  .out = (b0 + as.matrix(X) %*% b + rnorm(nrow(X), sd = 0.4)) %>% 
    round(2) %>% 
    .[,1]
)

# ... export ####

usethis::use_data(martini_outc_class, overwrite = TRUE)
usethis::use_data(martini_outc_regr, overwrite = TRUE)

# ML OBJECT ####

## ... classification ####

martini_ml_class <- prepare_ml(
  feature             = martini_feat,
  outcome             = martini_outc_class,
  outcome_name        = ".out",
  level_order         = c("no event", "event"),
  strata_trt          = TRUE, 
  prep_step_dummy     = FALSE,
  prep_step_normalize = FALSE,
  vars_imp_ignore     = ".trt",
  seed                = 2231
)

## ... regression ####

martini_ml_regr <- prepare_ml(
  feature             = martini_feat,
  outcome             = martini_outc_regr,
  outcome_name        = ".out",
  strata_trt          = TRUE, 
  prep_step_dummy     = FALSE,
  prep_step_normalize = FALSE,
  vars_imp_ignore     = ".trt",
  seed                = 2231
)

# ... export ####

usethis::use_data(martini_ml_class, overwrite = TRUE)
usethis::use_data(martini_ml_regr, overwrite = TRUE)

