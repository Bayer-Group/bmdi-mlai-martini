test_that("adam_spec", {
  
  # TEST SETUP ####
  
  # use system.file() to identify folder location after pkg is installed (system dependent)
  ads_path <- system.file("sas/adsl.sas7bdat", package = "martini") %>% 
    stringr::str_remove("adsl.sas7bdat")
  # direct path specification if tests are run outside of pkg build process
  # (in this case 'system.file()' gives an empty string)
  if (ads_path == "") ads_path <- "inst/sas/" 
  
  # create prepare specification
  ads_spec <- martini::adam_spec(ads_path)

  # TEST object structure ####
  
  testthat::expect_length(
    ads_spec,
    1
  )
  
  testthat::expect_named(
    ads_spec,
    c("adsl")
  )
  
})
