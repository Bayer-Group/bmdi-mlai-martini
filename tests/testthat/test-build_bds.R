
test_that("build_bds/pivot_prepare_bds duplicate messaging", {
  
  file_adlb     <- test_path("sas/adlb.sas7bdat")
  ads_spec_adlb <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  # test duplicate messaging ####
  
  # ... from build_bds (top level) ####
  # adlb.sas7bdat contains duplicates
  expect_message(build_bds(spec = ads_spec_adlb))
  
  # remove duplicates and retest
  spec_nodupes      <- ads_spec_adlb
  spec_nodupes$data <- spec_nodupes$data %>% 
    dplyr::distinct(SUBJID, PARAMCD, AVISIT, .keep_all = TRUE)
  
  expect_silent(build_bds(spec = spec_nodupes))
  
  # ... from pivot_prepare_bds() (throwing the message)####
  expect_message(
    pivot_prepare_bds(
      bds_full = ads_spec_adlb$data,
      spec     = ads_spec_adlb
    )
  )
  
  expect_silent(
    pivot_prepare_bds(
      bds_full = spec_nodupes$data,
      spec     = spec_nodupes
    )
  )
  
})

test_that("pivot_prepare_bds - values_fn deduced correctly", {

  # values_fn test ####
  
  file_adlb <- test_path("sas/adlb.sas7bdat")
  
  # ... values_fn is specified ####
  
  spec <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  spec$values_fn <- mean

  target_fn <- median
  
  actual_fn <- pivot_prepare_bds(
    bds_full  = spec$data,
    spec      = spec,
    values_fn = target_fn
  )[["values_fn"]]
  
  expect_equal(target_fn, actual_fn)
  
  # ... values_fn is NULL ####
  
  spec <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  values_fn_out <- pivot_prepare_bds(
    bds_full = spec$data,
    spec     = spec
  )[["values_fn"]]
  
  expect_equal(
    body(values_fn_out),
    body(values_fn_default)
  )
  
})

test_that("pivot_prepare_bds - names_from argument deduced correctly", {
  
  file_adlb <- test_path("sas/adlb.sas7bdat")
  spec      <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  # data in adlb.sas7bdat contains multiple time points (Visit 1-3)
  # -> names_from should be length 2, param and time
  # create data set with single time point 
  # -> names_from should be length 1, param only
  time_single <- spec$data[[spec$time]] %>% head(1)
  
  spec_single <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE,
    filter      = c(paste0(spec$time, " == '", time_single, "'"))
  )
  spec_multi  <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE
  )
  
  pivot_single <- pivot_prepare_bds(
    bds_full  = spec_single$data,
    spec      = spec_single
  )
  
  pivot_multi <- pivot_prepare_bds(
    bds_full  = spec_multi$data,
    spec      = spec_multi
  )
  
  expect_setequal(
    pivot_single$names_from,
    spec$param
  )
  
  expect_setequal(
    pivot_multi$names_from,
    c(spec$param, spec$time)
  )
  
})

test_that("build_bds - dict matches data set", {
  # TODO colnames have to match dict entries 1:1
  file_adlb <- test_path("sas/adlb.sas7bdat")
  spec      <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  time_single <- spec$data[[spec$time]] %>% head(1)
  
  spec_single <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE,
    filter      = c(paste0(spec$time, " == '", time_single, "'"))
  )
  spec_multi  <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE
  )
  
  bds_wide_single <- build_bds(spec_single)
  bds_wide_multi  <- build_bds(spec_multi)
  
  expect_setequal(
    bds_wide_single$data %>% names() %>% setdiff(c(".id")),
    bds_wide_single$dict$column
  )
  
  expect_setequal(
    bds_wide_multi$data %>% names() %>% setdiff(c(".id")),
    bds_wide_multi$dict$column
  )
  
})


test_that("build_bds conversion to factor/numeric from AVALC", {
  file_adlb <- testthat::test_path("sas/adlb.sas7bdat")
  spec_adlb <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  spec_adlb$data <- spec_adlb$data %>% 
    dplyr::mutate(AVALC = as.character(AVAL)) %>% 
    dplyr::mutate(AVALC = dplyr::case_when(
      PARAMCD == 'LAB1' ~ LETTERS[AVAL],
      TRUE ~ AVALC
    ))
  
  spec_adlb$value <- 'AVALC'
  build_adlb      <- build_bds(spec = spec_adlb)  
  expect_true(
    all(c('factor', 'numeric') %in%
      (build_adlb$data %>% dplyr::select(-.id) %>% purrr::map_chr(class))))
    
})
