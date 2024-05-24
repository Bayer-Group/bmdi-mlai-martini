test_that("strata_trt works", {
  # strata_trt works ####
  # TODO WS rewrite 
  
  # test stratification WITH and WITHOUT added treatment
  
  trt_groups <- c('PLA', 'trt1', 'trt2')
  n_total    <- 90
  
  d_feat <- tibble::tibble(
    .id  = 1:n_total,
    .trt = rep(trt_groups, length.out = n_total),
    cont = rnorm(n_total)
  )
  
  d_out <- tibble::tibble(
    .id  = 1:n_total,
    .out = rep(c(
      rep('no event', round(n_total/length(trt_groups))-9),
      rep('event',    9)), 
      length.out = n_total) 
  )
  
  d_raw <- dplyr::inner_join(d_out, d_feat, by = ".id") %>% 
    tidyr::unite(trt.out, .trt, .out, remove = FALSE)
  prop_tot_event_trt <- d_raw %>% 
    dplyr::pull(trt.out) %>% 
    table %>% 
    {. / sum(.)}
  
  seed <- 1950 # 1130
  train_prop <- .5
  res_out <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = FALSE,
    seed       = seed
  )$data_raw

    
  res_out_trt <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = TRUE,
    seed       = seed
  )$data_raw
  
  # distribution of trt
  prop_event <- list(
    total   = d_raw       %>%                  dplyr::pull(   .out) %>% {. == "event"} %>% mean() ,
    out     = res_out     %>% purrr::map_dbl(~ dplyr::pull(., .out) %>% {. == "event"} %>% mean()) %>% round(2),
    out_trt = res_out_trt %>% purrr::map_dbl(~ dplyr::pull(., .out) %>% {. == "event"} %>% mean()) %>% round(2) 
  )
  
  # distribution of combined stratum variable trt_out
  smmry_reshape <- function(x, set = NA_character_){
    dplyr::pull(x, trt.out) %>% 
      table() %>% 
      {./sum(.)} %>%  
      round(2) %>% 
      as.data.frame.table() %>% tibble::as_tibble() %>%  
      tidyr::pivot_wider(names_from = '.', values_from = Freq) %>% 
      dplyr::mutate(set = set, .before = 1)
  }
  
  prop_out_trt <- list(
    
    total   = d_raw %>% smmry_reshape(set = 'total') ,
    
    out_trt = res_out_trt %>% 
      purrr::map(~ tidyr::unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape) %>% 
      dplyr::bind_rows(.id = 'set') %>% 
      dplyr::mutate(strata_trt = TRUE, .after = set),
    
    out     = res_out     %>% 
      purrr::map(~ tidyr::unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape)  %>% 
      dplyr::bind_rows(.id = 'set') %>% 
      dplyr::mutate(strata_trt = FALSE, .after = set)
  ) %>% 
    purrr::reduce(dplyr::bind_rows)
  
  # prop_out_trt
  
  # compute sum of absolute deviations WITH and WITHOUT strata_trt parameter set to TRUE
  comp_strata_trt <- prop_out_trt %>%  
    dplyr::mutate_if(is.numeric, ~ abs(.x - .x[set == 'total'])) %>% 
    dplyr::filter(set != 'total') %>% 
    tidyr::nest(e = dplyr::contains('event')) %>% 
    dplyr::mutate(sum_e = purrr::map_dbl(e, sum)) %>% 
    dplyr::group_by(strata_trt) %>% 
    dplyr::mutate(sum_e = sum(sum_e)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(strata_trt, sum_e) %>% 
    dplyr::distinct() %>% 
    tibble::deframe()
  
  testthat::expect_gt(
    comp_strata_trt['FALSE'], 
    comp_strata_trt['TRUE']
  )
  
})


testthat::test_that("vars_keep_corr works", {
# vars_keep_corr works ####

  n <- 20
  p <- 5
  R <- rep(1, p) %>% diag()
  R[2,1] <- R[1,2] <- 0.95
  # choose one of V1, V2
  col_to_keep <- 'V2'
  R[4,3] <- R[3,4] <-  0.95 # no preference, check with current seed which is discarded
  
  
  withr::with_seed(1492, {
    
    d_feat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = R) %>% 
      as.data.frame() %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(
        neg_corr = -V2,
        .id = 1:n
      )
    
    d_out <- tibble::tibble(
      .id  = 1:n,
      .out = rnorm(n)
    )
    
  })
  
  d_ml0 <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    train_prop = 1
  )
  
  d_ml1 <- prepare_ml(
    feature        = d_feat,
    outcome        = d_out,
    vars_keep_corr = col_to_keep, 
    train_prop     = 1
  )
  
  cols_0 <- d_ml0$data_prep$train %>% names()
  cols_1 <- d_ml1$data_prep$train %>% names()
  
  testthat::expect_true(
    !  col_to_keep %in% cols_0 
    && col_to_keep %in% cols_1 
  )
  
  testthat::expect_setequal(
    d_ml1$removed$cols$corr,
    c('V1', "neg_corr", 'V4') # V4 seed dependent, either V4 or V3
  )
  
})




test_that('row removal works', {
  # row removal works ####
  
  # create minimal data set with NA and set prep_step_knnimpute = FALSE
  # to imitate incomplete imputation
  
  n_total  <- 10
  n_remove <- 1
  
  set.seed(955)
  d_feat <- tibble::tibble(
    .id  = 1:n_total,
    .trt = sample(c('A', 'B'), n_total, replace = TRUE),
    cont  = c(rnorm(n_total-n_remove), rep(NA, n_remove))
  )
  
  d_out <- tibble::tibble(
    .id  = 1:n_total,
    .out = sample(c('out1', 'out2'), n_total, replace = TRUE),
  )
  
  res_out <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    prep_step_knnimpute = FALSE
  ) 
  
  # observation deleted from prepped data set
  testthat::expect_equal(
    res_out %>% martini::get_data(type = 'prep') %>% nrow(),
    n_total - n_remove
  )
  
  # documented id in na_feature
  testthat::expect_equal(
    res_out$removed$rows$na_feature,
    res_out %>% 
      martini::get_data(type = 'raw') %>% 
      dplyr::filter(is.na(cont)) %>% 
      dplyr::pull(.id) 
  )
    
})

test_that('repeated measurement implementation works', {
  #'repeated measurement implementation works'  ####
  
  ads_build <- martini_spec %>% 
    adjust_spec(id = "adlb", filter = "") %>% 
    build(rm = TRUE)
  
  outcome_regr <- martini_spec$adlb$data %>% 
    dplyr::filter(PARAMCD == "HDL") %>% 
    dplyr::rename(tidyselect::all_of(c(".id" = "SUBJID"))) %>% 
    dplyr::mutate(AVISIT = forcats::fct_reorder(AVISIT, AVISITN))
  
  ml_regr <- prepare_ml(
    feature             = ads_build,
    outcome             = outcome_regr,
    outcome_name        = c(".rmtime" = "AVISIT", ".out" = "AVAL"),
    strata_trt          = TRUE,
    prep_step_dummy     = FALSE,
    prep_step_normalize = FALSE,
    vars_imp_ignore     = ".trt",
    seed                = 1825
  )
  
  id_training <- unique(ml_regr$data_raw$train$.id)
  id_test     <- unique(ml_regr$data_raw$test$.id)
  
  expect_length(intersect(id_training, id_test), 0)
  
  expect_true('.rmtime' %in% colnames(ml_regr$data_raw$train))
  
})


test_that("prepare_ml snapshots",{
  # prepare_ml snapshots ####                 
  
  #skip_on_ci()
  withr::local_options(width = 80)
  
  skip_if_not_installed("jsonlite")
  
  ads_path  <- test_path('sas/')
  ads_build <- ads_path %>% 
    adam_spec(
      filter = c(
        "AVISIT == 'Baseline'",
        "ADSNAME == 'ADLB' & AVISIT == 'Visit 1'",
        "ABLFL == 'Y'"
      ),
      attach_data = TRUE
    ) %>% 
    build(join = "adsl")
  
  # classification ####
  
  ads_ml_class <- prepare_ml(
    feature             = ads_build,
    outcome             = martini_outc_class,
    outcome_name        = ".out",
    level_order         = c("event", "no event"),
    strata_trt          = TRUE, 
    prep_step_dummy     = FALSE,
    prep_step_normalize = FALSE,
    vars_imp_ignore     = ".trt",
    seed                = 2231
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_class$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$prep_recipe <- ads_ml_class$prep_recipe %>% 
    purrr::modify_tree(leaf = tibble_to_JSON) 
  
  expect_snapshot(
    ads_ml_class %>% purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
  
  # regression ####
  
  ads_ml_regr <- prepare_ml(
    feature             = ads_build,
    outcome             = martini_outc_regr,
    outcome_name        = ".out",
    strata_trt          = TRUE,
    prep_step_dummy     = FALSE,
    prep_step_normalize = FALSE,
    vars_imp_ignore     = ".trt",
    seed                = 2231
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_regr$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$prep_recipe <- ads_ml_class$prep_recipe %>% 
    purrr::modify_tree(leaf = tibble_to_JSON)
  
  expect_snapshot(
    ads_ml_regr %>% purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
  # time-to-event ####
  
  ads_ml_surv <- prepare_ml(
    feature             = ads_build,
    outcome             = martini_outc_surv,
    outcome_name        = c(".time" = ".time", ".status" = ".status"),
    strata_trt          = TRUE,
    prep_step_dummy     = FALSE,
    prep_step_normalize = FALSE,
    vars_imp_ignore     = ".trt",
    seed                = 2231
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_surv$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$prep_recipe <- ads_ml_class$prep_recipe %>% 
    purrr::modify_tree(leaf = tibble_to_JSON)
  
  expect_snapshot(
    ads_ml_surv  %>% purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
})


