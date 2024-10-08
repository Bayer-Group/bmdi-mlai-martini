test_that("adam_spec_bds works", {
  
  # SETUP ####
  
  file_adlb        <- test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- test_path("sas/adlb_miss.sas7bdat")
  file_adlb_rename <- test_path("sas/adlb_rename.sas7bdat")
  
  
  # TEST key columns ####
  # ... provide non-existing id column name ####
  testthat::expect_error(
    ads_spec_adlb <- adam_spec_bds(
      file  = file_adlb,
      id    = 'USUBJID'         
    ),
    regexp = '`id`' 
  )
  
  # ... column cannot be guessed ####
  testthat::expect_null(
    adam_spec_bds(file = file_adlb_rename)
  )
  
  # ... provide non-standard column name ####
  ns_param <- 'MARTINI'
  ns_spec  <- adam_spec_bds(
    file  = file_adlb_rename,
    param = ns_param
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
    domain = "LB"
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