test_that("strata_trt works", {
  # strata_trt works ####
  # TODO WS rewrite 
  
  # test stratification WITH and WITHOUT added treatment
  
  trt_groups <- c("PLA", "trt1", "trt2")
  n_total    <- 90
  
  d_feat <- tibble::tibble(
    .id  = 1:n_total,
    .trt = rep(trt_groups, length.out = n_total),
    cont = rnorm(n_total)
  )
  
  d_out <- tibble::tibble(
    .id  = 1:n_total,
    .out = rep(c(
      rep("no event", round(n_total/length(trt_groups))-9),
      rep("event",    9)), 
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
  )$data$raw

    
  res_out_trt <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = TRUE,
    seed       = seed
  )$data$raw
  
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
    
    total   = d_raw %>% smmry_reshape(set = "total") ,
    
    out_trt = res_out_trt %>% 
      purrr::map(~ tidyr::unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape) %>% 
      dplyr::bind_rows(.id = "set") %>% 
      dplyr::mutate(strata_trt = TRUE, .after = set),
    
    out     = res_out     %>% 
      purrr::map(~ tidyr::unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape)  %>% 
      dplyr::bind_rows(.id = "set") %>% 
      dplyr::mutate(strata_trt = FALSE, .after = set)
  ) %>% 
    purrr::reduce(dplyr::bind_rows)
  
  # prop_out_trt
  
  # compute sum of absolute deviations WITH and WITHOUT strata_trt parameter set to TRUE
  comp_strata_trt <- prop_out_trt %>%  
    dplyr::mutate_if(is.numeric, ~ abs(.x - .x[set == "total"])) %>% 
    dplyr::filter(set != "total") %>% 
    tidyr::nest(e = dplyr::contains("event")) %>% 
    dplyr::mutate(sum_e = purrr::map_dbl(e, sum)) %>% 
    dplyr::group_by(strata_trt) %>% 
    dplyr::mutate(sum_e = sum(sum_e)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(strata_trt, sum_e) %>% 
    dplyr::distinct() %>% 
    tibble::deframe()
  
  testthat::expect_gt(
    comp_strata_trt["FALSE"], 
    comp_strata_trt["TRUE"]
  )
  
})


testthat::test_that("vars_keep_corr works", {
# vars_keep_corr works ####

  n <- 20
  p <- 5
  R <- rep(1, p) %>% diag()
  R[2,1] <- R[1,2] <- 0.95
  # choose one of V1, V2
  col_to_keep <- "V2"
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
  
  cols_0 <- d_ml0$data$prep$train %>% names()
  cols_1 <- d_ml1$data$prep$train %>% names()
  
  testthat::expect_true(
    !  col_to_keep %in% cols_0 
    && col_to_keep %in% cols_1 
  )
  
  testthat::expect_setequal(
    d_ml1$removed$cols$corr,
    c("V1", "neg_corr", "V4") # V4 seed dependent, either V4 or V3
  )
  
})


testthat::test_that("vars_no_trafo works", {
  # vars_no_trafo works ####
  
  withr::with_seed(2116,{
    
    n <- 250
    d_feat <- tibble::tibble(
      .id = 1:n,
      sym1 = rnorm(n, mean = 10, sd =1),
      sym2 = rnorm(n, mean = 100, sd =5),
      skw1 = exp(rnorm(n, mean = 1, sd = 2)),
      skw2 = exp(rnorm(n, mean = 0, sd = 1)),
      skw_corr = skw2 *2,
      # add a constant (skewness = NaN)
      const = rep(1,n)
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
  
  # find variable, that is log transformed by default to exclude using vars_no_trafo
  log_step <- d_ml0$recipe$prep %>%
    recipes::tidy() %>% 
    dplyr::filter(type == "log_skewness") %>% 
    dplyr::pull(number) %>% 
    head(1)
  var_notrafo <- tidy(d_ml0$recipe$prep, log_step)$terms %>%
    stringr::str_subset("skw") %>%
    head(1)
  
  d_ml1 <- prepare_ml(
    feature        = d_feat,
    outcome        = d_out,
    # keep the variable that would be discarded by default
    vars_keep_corr = d_ml0$removed$cols$corr_keep, 
    # vars_no_trafo: use one that is log transformed by default
    vars_no_trafo = var_notrafo,
    prep_step_normalize = FALSE, # for message test
    prep_step_log = FALSE,
    train_prop     = 1
  )
  
  cols_0 <- d_ml0$data$prep$train %>% names()
  cols_1 <- d_ml1$data$prep$train %>% names()
  
  # vars_no_trafo
  # ... input is documented correctly
  testthat::expect_true(
    var_notrafo %in% d_ml1$recipe$params$vars_no_trafo$value
  )
  testthat::expect_equal( # message adjusted according to prep_step_normalize
    d_ml1$recipe$params$vars_no_trafo$text %>% 
      stringr::str_detect("normali.ation"),
    (d_ml1$input$args$prep_step_normalize) || # if normalized or...
      (d_ml1$input$args %>% 
         purrr::keep_at(c("prep_step_normalize", "prep_step_log")) %>% 
         purrr::none(isTRUE)) # neither log nor normalize
  )
  
 
  # vars_keep_corr
  testthat::expect_false(
    d_ml0$removed$cols$corr_keep %in% cols_0
  )
  testthat::expect_true(
    d_ml0$removed$cols$corr_keep %in% cols_1
  )
  
})

test_that("row removal works", {
  # row removal works ####
  
  # create minimal data set with NA and set prep_step_knnimpute = FALSE
  # to imitate incomplete imputation
  
  n_total  <- 10
  n_remove <- 1
  
  # feat matrix with two NAs in separate subjects: 
  # first in feature cont, last in feature .trt which is ignored in imputation by default (2. expectation)
  d_feat <- withr::with_seed(
    955, 
    tibble::tibble(
      .id  = 1:n_total,
      .trt = sample(c("A", "B"), n_total, replace = TRUE) %>% 
        magrittr::inset2(n_total, NA),
      cont  = c(rep(NA, n_remove), rnorm(n_total-n_remove))
    )
  )
  
  d_out <- tibble::tibble(
    .id  = 1:n_total,
    .out = sample(c("out1", "out2"), n_total, replace = TRUE),
  )
  
  res_out <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out
  ) 
  
  # observation deleted from prepped data set
  testthat::expect_equal(
    res_out %>% get_data(type = "prep") %>% nrow(),
    n_total - n_remove
  )
  
  # documented id in na_feature slot in prepare_ml output
  testthat::expect_equal(
    res_out$removed$rows$na_feature,
    res_out %>% 
      martini::get_data(type = "raw") %>% 
      dplyr::filter(is.na(.trt)) %>% 
      dplyr::pull(.id) 
  )
    
})

test_that("`strings_as_factors = TRUE` in `recipe` works as expected", {
  
  martini_feat_char <- martini_feat %>% 
    dplyr::mutate(.id = stringr::str_pad(.id, width = 5, pad = "0")) %>% 
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))
  
  martini_outc_idchar <- martini_outc_class %>% 
    dplyr::mutate(.id = stringr::str_pad(.id, width = 5, pad = "0"))
  
  ml_class <- prepare_ml(
    feature = martini_feat_char,
    outcome = martini_outc_idchar, 
    train_prop = 1
  )
  
  # .id should be the only column of type character
  expect_equal(
    ml_class$data$prep$train %>% 
      purrr::map_lgl(is.character) %>% 
      purrr::keep(isTRUE) %>%
      names(),
    ".id"
  )
  
})


test_that("repeated measurement implementation works", {
  #'repeated measurement implementation works'  ####
  
  ads_build <- martini_spec %>% 
    adjust_spec(entry = "adlb", filter = "") %>% 
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
  
  id_training <- unique(ml_regr$data$raw$train$.id)
  id_test     <- unique(ml_regr$data$raw$test$.id)
  
  expect_length(intersect(id_training, id_test), 0)
  
  expect_true(".rmtime" %in% colnames(ml_regr$data$raw$train))
  
})

test_that("prepare_ml(check_feature) works", {  
  # prepare_ml(check_feature) works ####
  
  expect_message(
    p <- prepare_ml(
      feature = martini_feat,
      outcome = martini_outc_class, 
      check_feature = TRUE
    ), 
    "Potential issues were identified"
  )
  
  expect_snapshot(
    prepare_ml(
      feature = martini_feat,
      outcome = martini_outc_class, 
      check_feature = FALSE,
      train_prop = 3/4
    ),
    transform = hide_cli_id
  )
  
})



# test_that("prepare_ml(custom_recipe) works", {  
#   
#   # prepare_ml(custom_recipe) ####
#   ml_custom <- prepare_ml(
#     feature = martini_feat, 
#     outcome = martini_outc_class, 
#     outcome_name = ".out", 
#     custom_recipe = martini_ml_class$recipe$raw %>% 
#       #recipes::step_pca(recipes::all_numeric_predictors())
#       recipes::step_sample(20)
#   )
#   #debugonce(prepare_ml)
#   
#   # print
#   expect_snapshot(
#     ml_custom
#   )
#   
#})


test_that("get_data(martini_ml) works", {  
  
  # get_data(martini_ml) works ####
  
  expect_s3_class(
    get_data(martini_ml_regr, type = "prep"),
    "tbl_df" 
  )
  expect_s3_class(
    get_data(martini_ml_regr, type = "raw"),
    "tbl_df" 
  )
  
  # test split_id argument
  d_prep       <- get_data(martini_ml_regr)
  ncol_no_id   <- ncol(d_prep) 
  d_prep_id    <- get_data(martini_ml_regr, split_id = "type") 
  ncol_with_id <- ncol(d_prep_id)
  
  expect_true(
    ncol_no_id + 1 == ncol_with_id
  )

  expect_setequal(
    d_prep_id$type %>% unique(), 
    c("train", "test")
  )
    
})
  
test_that("prepare_ml() snapshots content/print", {
  # prepare_ml snapshots content/print ####                 
  
  #skip_on_ci()
  withr::local_options(width = 80)
  
  skip_if_not_installed("jsonlite")
  
  ads_path  <- test_path("sas/")
  ads_build <- ads_path %>% 
    adam_spec(
      filter = c(
        "AVISIT == 'Baseline'",
        "ADSNAME == 'ADLB' & AVISIT == 'Visit 1'",
        "ABLFL == 'Y'"
       #, "MHOCCUR == 'Y' | is.na(MHOCCUR)"
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
    seed                = 2231,
    train_prop          = 3/4
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_class$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$recipe$raw   <- NULL
  ads_ml_class$recipe$prep  <- NULL
  
  expect_snapshot(
    ads_ml_class %>% 
      magrittr::set_attr('class', 'list') %>% # snapshot object not print
      purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
  # # pkg data # todo: modify before snapshotting
  # expect_snapshot(martini_ml_class)
  # expect_snapshot(martini_ml_regr)
  # expect_snapshot(martini_ml_surv)
  # 
  expect_visible(
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
    seed                = 2231,
    train_prop          = 3/4
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_regr$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$recipe$raw   <- NULL
  ads_ml_class$recipe$prep  <- NULL
  
  expect_snapshot(
    ads_ml_regr %>% 
      magrittr::set_attr('class', 'list') %>% # snapshot object not print
      purrr::modify_tree(leaf = tibble_to_JSON)
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
    seed                = 2231, 
    train_prop          = 3/4
  )
  
  # remove file path information in console output (will be a different tmp file path each time the test is run)
  ads_ml_surv$source <- NULL
  # recipe will have different step and environment ids in each run
  ads_ml_class$recipe$raw   <- NULL
  ads_ml_class$recipe$prep  <- NULL
  
  expect_snapshot(
    ads_ml_surv %>% 
      magrittr::set_attr('class', 'list') %>% # snapshot object not print
      purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
})


