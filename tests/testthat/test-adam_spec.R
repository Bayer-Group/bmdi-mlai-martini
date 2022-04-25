test_that("adam_spec", {
  
  # TEST SETUP ####
  
  ads_path <- test_path('sas/')
  
  # create prepare specification
  ads_spec <- adam_spec(ads_path)
  
  # TEST object structure ####
  
  testthat::expect_length(
    ads_spec,
    3
  )
  
  testthat::expect_named(
    ads_spec,
    c("adsl", "adlb", "advs")
  )
  
  # TEST add_bds argument ####
  
  ads_spec_add <- adam_spec(ads_path, add_bds = "adlb_miss")
  
  testthat::expect_setequal(
    names(ads_spec) %>% c("adlb_miss"),
    names(ads_spec_add)
  )

})
