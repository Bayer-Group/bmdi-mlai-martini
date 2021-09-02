test_that("adam_spec_adsl", {
  
  # SETUP ####
  
  file_adsl <- test_path("sas/adsl.sas7bdat")
  
  # create prepare specification
  ads_spec_adsl <- martini:::adam_spec_adsl(file_adsl)
  
  # TEST adsl prepare specification ####
  
  # ... column selection ####
  testthat::expect_setequal(
    ads_spec_adsl$select,
    c("SUBJID", "TRT01A", "AGEGR01", "SEX", "RACE", "AGE")
  )
  
  # ... drop list ####
  
  # ... ... date time ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$datetime,
    c("RANDDT")
  )
  
  # ... ... numeric codes ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$numcode,
    c("AGEGR01N", "SEXN", "RACEN")
  )
  
  # ... ... redundancies ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$redundancy,
    c("UASR", "USUBJID", "RANDDT", "TRT01P")
  )
  
  
})
