test_that("adam_spec_bds works", {
  
  # SETUP ####
  
  file_adlb        <- test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- test_path("sas/adlb_miss.sas7bdat")
  file_adlb_rename <- test_path("sas/adlb_rename.sas7bdat")
  
  
  #  create prepare specification ####
  ads_spec_adlb <- martini::adam_spec_bds(
    file_adlb         
  )
  
  # TEST key columns ####
  # ... provide non-existing column name as key column but can be guessed ####
  testthat::expect_message(
    ads_spec_adlb <- martini::adam_spec_bds(
      file_adlb,
      param = 'PRMCD'         
    ),
    regexp = 'param' 
  )
  
  # ... column cannot be guessed ####
  testthat::expect_null(
    martini::adam_spec_bds(file_adlb_rename)
  )
  
  # ... provide non-standard column name ####
  testthat::expect_type(
    martini::adam_spec_bds(
      file_adlb_rename,
      param = 'MARTINI'
    ),
    'list'
  )
  
  # 
  
})
