test_that("adam_spec_bds works", {
  
  # SETUP ####
  
  file_adlb        <- test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- test_path("sas/adlb_miss.sas7bdat")
  file_adlb_rename <- test_path("sas/adlb_rename.sas7bdat")
  
  
  # # TEST key columns ####
  # # ... provide non-existing id column name ####
  # testthat::expect_error(
  #   ads_spec_adlb <- adam_spec_bds(
  #     file  = file_adlb,
  #     id    = 'USUBJID'         
  #   ),
  #   regexp = '`id`' 
  # )
  # 
  # # ... column cannot be guessed ####
  # testthat::expect_null(
  #   adam_spec_bds(file = file_adlb_rename)
  # )
  
  # ... provide non-standard column name ####
  ns_param <- 'MARTINI'
  ns_spec  <- adam_spec_bds(
    file  = file_adlb_rename,
    param = ns_param,
    value = "LBSTRESC"
  )
  testthat::expect_equal(
    ns_spec[['param']],
    ns_param
  )
  
  # TEST 'data' parameter ####
  
  spec_bds_file <- adam_spec_bds(file = file_adlb)
  
  spec_bds_data <- adam_spec_bds(
    data   = haven::read_sas(file_adlb),
    param  = "PARAMCD",
    value  = "AVAL",
    time   = "AVISIT",
    unit   = "AVALU",
    label  = "PARAM",
    domain = "adlb"
  )
  
  testthat::expect_equal(
    spec_bds_file[!names(spec_bds_file) %in% c("file", "md5", "size")],
    spec_bds_data[!names(spec_bds_data) %in% c("file", "md5", "size")]
  )
  
  
})
# test area  ####
if(FALSE){
  
  require(tidyverse)
  require(haven)
  require(labelled)
  
  file = '../adegf.sas7bdat'
  id = 'SUBJID'
  param  =  NULL
  label  = NULL
  unit   = NULL # AVALU, xxSTRESU, ORESSU
  time   = NULL 
  value  = NULL #c(AVAL, CHG)
  filter = NULL
  
  # basic function call
  spec_res <- adam_spec_bds(file = file, id = id)
  spec_res %>%  str()
  
  # specify filter that is partially not applicable
  filter_test <- c("AVISIT == 'BASELINE'", "LBTESTCD == 'RHYNOS'")
  spec_res <- adam_spec_bds(file = file, id = id, filter = filter_test)
  spec_res$filter
  
  # specify value column that is not in the data
  spec_res <- adam_spec_bds(file = file, id = id, value = "VALUE")
  
}

test_that("check_role() works with bds data", {
  
  adlb <- test_path("sas/adlb.sas7bdat") %>% haven::read_sas()
  
  # guess param column name
  expect_equal(
    check_role(data = adlb, role = "param", type = "bds"),
    list(role = "param", column = "PARAMCD", required = TRUE, check_passed = TRUE)
  )
  
  # provide wrong column name
  expect_equal(
    check_role(data = adlb, role = "param", column_spec = "param", type = "bds"),
    list(role = "param", column = NULL, required = TRUE, check_passed = FALSE)
  )
  expect_warning(
    check_role(data = adlb, role = "param", column_spec = "param", type = "bds")
  )
  
})

test_that("check_role() works with occds data", {
  
  admh <- test_path("sas/admh.sas7bdat") %>% haven::read_sas()
  
  # guess label column name
  expect_equal(
    check_role(data = admh, role = "label", type = "occds"),
    list(role = "label", column = "MHHLGT", required = TRUE, check_passed = TRUE)
  )
  
  # provide wrong column name
  expect_equal(
    check_role(data = admh, role = "label", column_spec = "wrong_label", type = "occds"),
    list(role = "label", column = NULL, required = TRUE, check_passed = FALSE)
  )
  
  expect_warning(
    check_role(data = admh, role = "label", column_spec = "wrong_label", type = "occds")
  )
  
})

