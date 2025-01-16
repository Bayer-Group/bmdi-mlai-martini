test_that("adam_spec_adsl works", {
  
  adsl_file   <- file.path(testthat::test_path('sas', 'adsl.sas7bdat'))
  filter      <- c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
   
  spec <- adam_spec_adsl(
    file        = adsl_file, 
    filter      = filter
  )
  
  expect_type(spec, 'list')
  
})

test_that("adsl_identify works", {
  
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
  

test_that("adsl_identify_dttm works", {
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
  
  # extract labels attribute from imported data
  data <- haven::read_sas(file, catalog_file = catalog)
  expect_equal(
    purrr::map(data, ~{
      labels <- attr(.x, "labels") 
      # TODO for now only numeric (labels are levels)
      # later character: labels are factor labels, levels dont change
      if (!is.null(labels) && isTRUE(is.numeric(labels))) {
        labels %>% sort() 
      }
    }) %>% 
      purrr::compact(),
    list(workshop = c(R = 1, SAS = 2))
  )
  
  
  
})

test_that("fct_levels argument of adam_spec_adsl() works", {
  
  file    <- test_path('sas', 'hadley.sas7bdat')
  catalog <- test_path('sas', 'formats.sas7bcat')
  
  data <- haven::read_sas(file, catalog_file = catalog)
  
  fct_levels <- purrr::map(c(paste0("q", 1:4)) %>% purrr::set_names(), ~{
    purrr::set_names(1:5, LETTERS[1:5])
  }) 
  
  spec_adsl <- adam_spec_adsl(
    file        = file, 
    id          = "id",
    fct_levels  = fct_levels, 
    catalog     = catalog
  )
  
  expect_setequal(
    names(spec_adsl$factor_levels),
    c(
      "q1", "q2", "q3", "q4",
      # only level labels of integer columns are extracted 
      # from the catalog file
      "workshop"
    )
  )
  
  fct_levels <- list(
    q1           = purrr::set_names(1:6, LETTERS[1:6]),
    gender       = c(girl = "f", boy = "m"),
    workshop     = c(R = 1, Python = 2),
    non_existent = c(old = 65, young = 25)
  )
  
  spec_adsl <- adam_spec_adsl(
    file        = file, 
    id          = "id",
    fct_levels  = fct_levels, 
    catalog     = catalog
  )
  
  expect_setequal(
    names(spec_adsl$factor_levels),
    c("q1", "workshop", "gender")
  )
  
  expect_equal(
    names(spec_adsl$factor_levels$workshop),
    c("R", "Python")
  )
  
})

