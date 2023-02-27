testthat::test_that("strata_trt works", {

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


testthat::test_that("keep_vars_corr works", {

  set.seed(1492)
  
  n <- 20
  p <- 5
  R <- rep(1, p) %>% diag()
  R[2,1] <- R[1,2] <- 0.95
  
  
  d_feat <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = R) %>% 
    as.data.frame() %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(.id = 1:n)
  
  d_out <- tibble::tibble(
    .id  = 1:n,
    .out = rnorm(n)
  )
  
  
  col_to_keep <- 'V1'
  d_ml0 <- prepare_ml(
    feature = d_feat,
    outcome = d_out
  )
  
  d_ml1 <- prepare_ml(
    feature        = d_feat,
    outcome        = d_out,
    vars_keep_corr = col_to_keep
  )
  
  cols_0 <- d_ml0$data_prep$train %>% names()
  cols_1 <- d_ml1$data_prep$train %>% names()
  
  testthat::expect_true(
    !  col_to_keep %in% cols_0 
    && col_to_keep %in% cols_1 
  )
  
})

test_that("prepare_ml snapshots", {
  
  skip_on_ci()
  
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
  
  expect_snapshot(
    ads_ml_class
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
  
  expect_snapshot(
    ads_ml_regr
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
  
  expect_snapshot(
    ads_ml_surv
  )
  
})

