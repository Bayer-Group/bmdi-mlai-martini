test_that("adam_spec_bds works", {
  
  # SETUP ####
  
  # use system.file() to identify folder location after pkg is installed (system dependent)
  file_adlb        <- system.file("sas/adlb.sas7bdat", package = "martini")
  file_adlb_miss   <- system.file("sas/adlb_miss.sas7bdat", package = "martini")
  file_adlb_rename <- system.file("sas/adlb_rename.sas7bdat", package = "martini")
  # direct path specification if tests are run outside of pkg build process
  # (in this case 'system.file()' gives an empty string)
  if (file_adlb == "") {
    file_adlb        <- "inst/sas/adlb.sas7bdat"
    file_adlb_miss   <- "inst/sas/adlb_miss.sas7bdat"
    file_adlb_rename <- "inst/sas/adlb_rename.sas7bdat"
  }
  
  
   
   
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
