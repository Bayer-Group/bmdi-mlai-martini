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
    regexp = 'id' 
  )
  
  # ... column cannot be guessed ####
  testthat::expect_null(
    adam_spec_bds(file = file_adlb_rename)
  )
  
  # ... provide non-standard column name ####
  testthat::expect_type(
    adam_spec_bds(
      file  = file_adlb_rename,
      param = 'MARTINI'
    ),
    'list'
  )
  
  # TEST 'data' parameter ####
  
  spec_bds_file <- adam_spec_bds(
    file  = file_adlb
  )
  
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
    spec_bds_file[!names(spec_bds_file) %in% c("file")],
    spec_bds_data[!names(spec_bds_data) %in% c("file")]
  )
  
  
})
