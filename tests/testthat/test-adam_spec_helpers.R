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

test_that("import_info() works", {
  
  adsl_file <- test_path('sas/adsl.sas7bdat')
  
  # no column attributes are removed
  data_haven <- haven::read_sas(adsl_file)
  data_import <- import_info(adsl_file)
  
  expect_equal(
    purrr::map(data_haven,  ~{attributes(.x)}),
    purrr::map(data_import, ~{attributes(.x)})
  )
  
})
