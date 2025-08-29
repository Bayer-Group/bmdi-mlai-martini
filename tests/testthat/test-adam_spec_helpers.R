test_that("create_dict() works", {
  
  # create_dict() works ####
  
  # create prep specification
  ads_spec <- martini_spec #adam_spec(ads_path, attach_data = TRUE)
  # ERROR adam_guess() for occds, 
  #role == value -> pattern is NULL
  # -> str_subset exits
  
  # no unit column available
  ads_spec_bds         <- ads_spec$adlb
  ads_spec_bds["unit"] <- list(NULL) 
  # currently NA values of col select set to NULL in adam_spec_bds (if no column could be guessed)

  spec_entry <- ads_spec_bds
  
  expect_true(
    create_dict(ads_spec_bds) %>% tibble::is_tibble()
  )
  
})

test_that("import_info() works", {
  # import_info() works ####
  
  adsl_file <- test_path("sas/adsl.sas7bdat")
  
  # no column attributes are removed
  data_haven <- haven::read_sas(adsl_file)
  data_import <- import_info(adsl_file)
  
  expect_equal(
    purrr::map(data_haven,  ~{attributes(.x)}),
    purrr::map(data_import$data, ~{attributes(.x)})
  )
  
})


test_that("check_occds_occur() works", {
  # check_occds_occur() works ####
  
  n <- 5
  data_occds <- tibble::tibble(
    col1 = 1:n,
    OKoccur  = c(NA, "Y", NA, "Y", NA),
    OKoccurN = c(NA,  1 , NA,  1 , NA),
    ERoccur  = c("N", "Y", NA, "Y", NA),
    ERoccurN = c( 0 ,  1 , NA,  1 , NA)
  )
  
  # N/0 occurrence issue in ERoccur and ERoccurN
  expect_message(
    res1 <- check_occds_occur(
     data_occds, 
     domain = NULL, 
     filters = NULL
    ), 
    "ERoccur and ERoccurN"
  )
  expect_setequal(
    res1,  
    c("ERoccur", "ERoccurN")
  )
  
  # no issue: filtered correctly, no remaining 0/N values
  expect_no_message(
    res2 <- check_occds_occur(
      data_occds, 
      domain = NULL, 
      filters = "ERoccur == 'Y' | is.na(ERoccur)"
    )
  )
  expect_length(res2, 0)
  
  # no colname matches '--OCCUR(N)'
  expect_no_message(
    res3 <- check_occds_occur(
      data = mtcars
    )
  )
  expect_length(res3, 0)
  
  
  # no OCCUR colname matches has N or 0
  expect_no_message(
    res4 <- check_occds_occur(
      data = data_occds %>% 
        dplyr::select(-tidyselect::starts_with("ERoccur"))
    )
  )
  expect_length(res4, 0)
  
  # domain used correctly to prefix message
  dom_name <- 'ADXY'
  expect_message(
    res5 <- check_occds_occur(
      data = data_occds,
      domain = dom_name
    ), 
    stringr::str_to_lower(dom_name)
  )
  expect_length(res5, 2)
  
  # pluralization used correctly
  expect_message(
    check_occds_occur(
      data = data_occds %>% dplyr::select(-ERoccurN)
    ), 
    "column ERoccur contains"
  )
  expect_message(
    res6 <- check_occds_occur(
      data = data_occds
    ), 
    "columns ERoccur and ERoccurN contain"
  )
  expect_length(res6, 2)
  
  # pkg data
  expect_message(
    res7 <- check_occds_occur(
      data = martini_spec$admh$data, 
      filters = NULL 
    ), 
    "columns MHOCCUR and MHOCCURN"
  )
  expect_length(res7, 2)
  
  expect_no_message(
    res8 <- check_occds_occur(
      data = martini_spec$admh$data, 
      filters = "MHOCCUR == 'Y' | is.na(MHOCCUR)"
    )
  )
  expect_length(res8, 0)
  
})
