
devtools::load_all()

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
  "MHOCCUR == 'Y' | is.na(MHOCCUR)"
)

martini_spec <- adam_spec(
  ads_path, filter = filters, attach_data = TRUE
  # pre_study = TRUE # deprecated
  ) %>% 
  adjust_adsl_select(drop = "AGEGR01") %>% 
  adjust_adsl_select(drop = "BMI") %>% 
  adjust_spec("admh", count = FALSE, label = "MHDECOD")

usethis::use_data(martini_spec, overwrite = TRUE)

# FEATURE MATRIX ####

martini_feat <- build(martini_spec, join = dplyr::left_join)

usethis::use_data(martini_feat, overwrite = TRUE)

# OUTCOME OBJECTS ####

# ... prepare feature matrix for simulation ####

rec <- withr::with_seed(
  1841,
  recipes::recipe(~., data = martini_feat) %>% 
    recipes::update_role(.id, new_role = "id") %>% 
    recipes::step_impute_knn(recipes::all_predictors()) %>% 
    recipes::step_normalize(recipes::all_numeric(), -recipes::has_role("id")) %>% 
    recipes::step_dummy(recipes::all_nominal(), -recipes::has_role("id")) %>% 
    recipes::prep()
)

X <- recipes::bake(rec, new_data = NULL) %>% 
  # prepare linear interaction effect
  dplyr::mutate(int = -.trt_TRT*BMI)

# ... create effect vector ####

b <- ncol(X) %>% numeric() %>% setNames(colnames(X))

beta <- c(
  # interaction
  "int"                     =  0.5,
  # main effect
  "atrial_fibrillation_yes" =  0.2,
  "angina_pectoris_yes"     =  1.5,
  "BPSYS"                   =  0.1,
  "HB"                      =  0.7,
  ".trt_TRT"                = -0.5
)

# intercept for linear model
lm_b0 <- 0

# case probability (for classification)
prob_case <- .5

# intercept for logistic model
logistic_b0 <- -log(1/prob_case-1)

# effect multiplicator for logistic model
logistic_mult_b <- 1.5

# ... simulate outcome ####

## ... ... classification ####
martini_outc_class <- withr::with_seed(
  1840,
  martini:::simulate_outcome(
    X, beta,
    type       = "classification",
    ctrl_class = list(prob_ev = .4, mult_beta = 1.5)
  )
)

## ... ... regression ####
martini_outc_regr <- withr::with_seed(
  1840,
  martini:::simulate_outcome(
    X, beta,
    type       = "regression",
    ctrl_class = list(b0 = 0, sd = .4)
  )
)

## ... ... survival ####
martini_outc_surv <- withr::with_seed(
  1840,
  martini:::simulate_outcome(
    X, beta,
    type      = "survival",
    ctrl_surv = list(surv_mean = 180,
                     cens_mean = 180,
                     cens_max  = 270,
                     mult_beta = 1.5,
                     int       = TRUE)
  )
)

# ... export ####

usethis::use_data(martini_outc_class, overwrite = TRUE)
usethis::use_data(martini_outc_regr,  overwrite = TRUE)
usethis::use_data(martini_outc_surv,  overwrite = TRUE)

# ML OBJECT ####

## ... classification ####

martini_ml_class <- prepare_ml(
  feature             = martini_feat,
  outcome             = martini_outc_class,
  outcome_name        = ".out",
  level_order         = c("event", "no event"),
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

## ... survival ####

martini_ml_surv <- prepare_ml(
  feature             = martini_feat,
  outcome             = martini_outc_surv,
  outcome_name        = c(".time" = ".time", ".status" = ".status"),
  strata_trt          = TRUE, 
  prep_step_dummy     = FALSE,
  prep_step_normalize = FALSE,
  vars_imp_ignore     = ".trt",
  seed                = 2231
)

# ... check changes ####

cur_pkg_data <- function(x){
  
  e <- new.env()
  load(paste0("data/", x, ".rda"), envir = e)
  e[[x]]
  
}

waldo_ignore <- \(ml){
  purrr::pluck(ml, "recipe", "prep") <- NULL
}
purrr::walk(c(
  "martini_ml_class",
  "martini_ml_regr",
  "martini_ml_surv"
), ~{
   
  waldo::compare(
    cur_pkg_data(.x) %>% waldo_ignore(),
    get(.x) %>% waldo_ignore(),
    ignore_formula_env = TRUE,
    max_diffs = Inf,
  ) %>% print()
})

# ... export ####

if(FALSE){
  
  # NOTE only run, if changes are as expected
  usethis::use_data(martini_ml_class, overwrite = TRUE)
  usethis::use_data(martini_ml_regr,  overwrite = TRUE)
  usethis::use_data(martini_ml_surv,  overwrite = TRUE)
  
}


# ... rds files ####

if(FALSE){
  
# path to the sas-files of the test data sets
ads_path <- system.file("tests/testthat/sas", package = "martini")
if(ads_path == "") ads_path <- "tests/testthat/sas"

ads_path %>% 
  list.files(full.names = TRUE) %>% 
  purrr::walk(~{
    data <- haven::read_sas(.x)
    saveRDS(
      data, 
      here::here(
        ads_path, 
        basename(.x) %>% 
          tools::file_path_sans_ext() %>% paste0(".rds")))
  })


}
