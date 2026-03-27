test_that("build_adsl()", {
 
  adsl_path   <- testthat::test_path('sas')
  #filter      <- c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
  
  spec <- adam_spec(
    adsl_path,
    keep = "adsl", 
    attach_data = TRUE
    #, 
    #filter      = filter
  )

  build_adsl(spec[[1]])  
   
})
  