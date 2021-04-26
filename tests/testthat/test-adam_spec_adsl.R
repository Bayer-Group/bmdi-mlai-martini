test_that("adam_spec_adsl", {
  
  # SETUP ####
  
  # use system.file() to identify folder location after pkg is installed (system dependent)
  file_adsl <- system.file("sas/adsl.sas7bdat", package = "martini")
  # direct path specification if tests are run outside of pkg build process
  # (in this case 'system.file()' gives an empty string)
  if (file_adsl == "") file_adsl <- "inst/sas/adsl.sas7bdat" 
  
  # create prepare specification
  ads_spec_adsl <- martini::adam_spec_adsl(file_adsl)
  
  # TEST adsl prepare specification ####
  
  # ... column selection ####
  testthat::expect_setequal(
    ads_spec_adsl$select,
    c("SUBJID", "TRT01A", "AGEGR01", "SEX", "RACE", "AGE")
  )
  
  # ... drop list ####
  
  # ... ... date time ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$datetimes,
    c("RANDDT")
  )
  
  # ... ... numeric codes ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$numcodes,
    c("AGEGR01N", "SEXN", "RACEN")
  )
  
  # ... ... redundancies ####
  testthat::expect_equal(
    ads_spec_adsl$drop_list$redundancies,
    c("UASR", "USUBJID", "RANDDT", "TRT01P")
  )
  
  
})
