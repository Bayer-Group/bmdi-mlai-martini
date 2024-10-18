testthat::test_that("adam_spec_adsl works", {
  
  adsl_file   <- file.path(test_path('sas', 'adsl.sas7bdat'))
  filter      <- c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
   
  spec <- adam_spec_adsl(
    file        = adsl_file, 
    filter      = filter
  )
  
  expect_type(spec, 'list')
  
})

testthat::test_that("adsl_identify works", {
  
  adsl_data <- haven::read_sas(file.path(test_path('sas', 'adsl.sas7bdat')))
  adsl_data <- adsl_data %>% labelled::remove_var_label()
  type_sel      <- c(
    # adsl only
    'dttm',
    # using dict
    'combined', 
    # using flag results
    'factor'
  )
  
  test <- adsl_identify(adsl_data, type = type_sel)
  expect_true(all(test$to_remove %>% unlist() %>%  {.%in% colnames(adsl_data)}))
  
  test <- adsl_identify(adsl_data)
  expect_true(all(test$to_remove %>% unlist() %>%  {.%in% colnames(adsl_data)}))
  
  
})
  

testthat::test_that("adsl_identify_dttm works", {
  # currently adsl_identify_dttm is only helper that directly uses var_labels of adsl
  # others use dictionary
  
  # now works for labelled (ADaM standard) and unlabelled data sets
  adsl        <- haven::read_sas(file.path(test_path('sas', 'adsl.sas7bdat')))
  
  dttm_name       <- 'RANDDT'
  adsl_dttm       <- adsl %>% dplyr::select(ADSNAME, SUBJID, tidyselect::any_of(dttm_name))
  adsl_nodttm     <- adsl_dttm %>% dplyr::select(-tidyselect::any_of(dttm_name))
  
  adsl_from_label <- adsl %>% dplyr::select(ADSNAME, SUBJID) %>% 
    dplyr::mutate(test = 1 : dplyr::n()) %>% 
    labelled::set_variable_labels(.labels = list(test = 'date'))
  
  # no dttm in adsl  
  expect_equal(
    adsl_identify_dttm(adsl_nodttm),
    character()
  )
  
  # no dttm in adsl WITHOUT labels 
  expect_equal(
    adsl_identify_dttm(adsl_nodttm %>% labelled::remove_labels()),
    character()
  )
  
  # dttm in adsl WITHOUT labels 
  expect_equal(
    adsl_identify_dttm(adsl_dttm %>% labelled::remove_labels()),
    dttm_name
  )
  
  
  # dttm from label   
  expect_equal(
    adsl_identify_dttm(adsl_from_label),
    'test'
  )
  
})

test_that("catalog_file argument of adam_spec_adsl() works", {
  
  file    <- test_path('sas', 'hadley.sas7bdat')
  catalog <- test_path('sas', 'formats.sas7bcat')
  
  data <- haven::read_sas(file, catalog_file = catalog)
  
  purrr::map(data, ~{
    labels <- attr(.x, "labels") 
    # TODO for now only numeric (labels are levels)
    # later character: labels are factor labels, levels dont change
    if (!is.null(labels) && isTRUE(is.numeric(labels))) {
      labels %>% sort() %>% names()
    }
  }) %>% 
    purrr::compact()
  
})
# NOTE covered by snapshot test for 'adam_spec()'
