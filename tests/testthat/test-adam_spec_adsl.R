test_that("adam_spec_adsl", {
  
  # SETUP ####
  
  file_adsl <- test_path("sas/adsl.sas7bdat")
  
  # create prepare specification
  ads_spec_adsl <- adam_spec_adsl(file_adsl)
  
  # TEST adsl prepare specification ####
  
  # TODO: write list of expectations along with data set creation, save to fixtures/adam_spec_expects.rds
  # TODO data creation script move from dev to somewhere more sensible 
  # ... column selection ####
  expect_setequal(
    ads_spec_adsl$select,
    c("SUBJID", "TRT01A", "AGEGR01", "SEX", "RACE", "AGE", "BMI")
  )
  
  # ... drop list ####
  
  # ... ... date time ####
  expect_setequal(
    ads_spec_adsl$drop_list$datetime,
    c("RANDDT")
  )
  
  # ... ... numeric codes ####
  expect_setequal(
    ads_spec_adsl$drop_list$numcode,
    c("TRT01PN", "TRT01AN", "AGEGR01N", "SEXN", "RACEN")
  )
  
  # ... ... redundancies ####
  expect_setequal(
    ads_spec_adsl$drop_list$redundancy,
    c("UASR", "USUBJID", "RANDDT", "TRT01P")
  )
  
  
})
