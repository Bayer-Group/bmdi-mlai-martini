test_that("adam_spec", {
  
  # TEST SETUP ####
  
  ads_path <- test_path('sas/')
  
  # create prepare specification
  ads_spec <- martini::adam_spec(ads_path)
  
  # TEST object structure ####
  
  testthat::expect_length(
    ads_spec,
    2
  )
  
  testthat::expect_named(
    ads_spec,
    c("adsl", "adlb")
  )
  
  # TEST add_bds argument ####
  
  ads_spec_add <- adam_spec(ads_path, add_bds = "adlb_miss")
  
  # TODO finish test (check list names?)
  
})
