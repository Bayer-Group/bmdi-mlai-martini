test_that("adam_spec", {
  
  # TEST SETUP ####
  
  ads_path <- test_path('sas/')
  
  # create prep specification
  ads_spec <- adam_spec(ads_path)
  
  # TEST object structure ####
  
  testthat::expect_length(
    ads_spec,
    3
    #TODO WS remove hard-coding list.files(ads_path), filter look up table, nrow
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

test_that("adam_spec snapshots", {
  
  ads_path <- test_path('sas/')
  
  expect_snapshot(
    adam_spec(ads_path)
  )
  
})
