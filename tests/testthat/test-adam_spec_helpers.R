test_that("create_dict works", {
  
  
  # TEST SETUP ####
  
  ads_path <- test_path('sas/')
  
  # create prep specification
  ads_spec <- adam_spec(ads_path, attach_data = TRUE)
  
  # no unit column available
  ads_spec_bds         <- ads_spec$adlb
  ads_spec_bds['unit'] <- list(NULL) # currently NA values of col select set to NULL in adam_spec_bds (if no column could be guessed)

  spec_entry <- ads_spec_bds
  
  expect_true(
    create_dict(ads_spec_bds) %>% tibble::is_tibble()
  )
  
})
