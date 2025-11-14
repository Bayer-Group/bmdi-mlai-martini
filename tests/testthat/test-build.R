test_that("build() correctly passes parameters to build_bds()", {
  
  path_test <- test_path("sas")
  
  spec_dupl <- adam_spec(
    path_test,
    keep = c('adsl', 'adlb'), 
    attach_data = TRUE,
    id = "SUBJID"
    )
  
  # introduce duplicates (select by Date):
  # spec_dupl$adlb$data values are all integers
  spec_dupl$adlb$data <- spec_dupl$adlb$data %>%
    dplyr::mutate(AVAL = AVAL + .5) %>% 
    dplyr::mutate(Date = as.Date('2021-01-01')) %>% 
    dplyr::bind_rows(spec_dupl$adlb$data %>% dplyr::mutate(Date = as.Date('2021-06-01')))

  # check if 'values_fn' is correctly passed ####
  
  spec_arrange <- spec_dupl
  
  # change default for values_fn (mean -> last)
  spec_arrange$adlb$dupl_ctrl <- list(
    values_fn = dplyr::last, 
    arrange   = NULL
  )
  
  data_build_bds <- build_bds(spec_arrange$adlb)$data
  
  data_build <- build(
    spec_arrange, 
    join = dplyr::inner_join
  )
  
  expect_equal(
    data_build %>% dplyr::select(tidyselect::all_of(data_build_bds %>% names())),
    data_build_bds,
    ignore_attr = TRUE
  )
  
  # check if 'arrange' is correctly passed from build to build_bds() ####
  
  spec_arrange <- spec_dupl
  
  # change default for arrange (later to first Date)
  spec_arrange$adlb$dupl_ctrl <- list(
    values_fn = dplyr::last, 
    arrange   = c("desc(Date)")
  )
  
  data_build_bds <- build_bds(spec_arrange$adlb)$data
  
  data_build <- build(
    spec_arrange, 
    join = dplyr::inner_join
  )
  
  expect_equal(
    data_build %>% dplyr::select(tidyselect::all_of(data_build_bds %>% names())),
    data_build_bds,
    ignore_attr = TRUE
  )
  
})

test_that("build() correctly builds the dictionary", {
  
  path_test <- testthat::test_path("sas")
  
  data_build <- path_test %>% 
    adam_spec(
      keep        = c('adsl', 'advs'),
      filter      = c("AVISIT == 'Baseline'"),
      attach_data = TRUE,
      id          = "SUBJID"
    ) %>% 
    build()
  
  # all columns of feature matrix (minus '.id') included in dictionary and vice versa
  expect_setequal(
    attr(data_build, "dict")[["column"]],
    data_build %>% names()
  )
  
  # no duplicated entries in the dictionary
  expect_false(
    any(duplicated(attr(data_build, "dict")[["column"]]))
  )
  
})


test_that("build snapshots", {
  
  #skip_on_ci()
  withr::local_options(width = 80)
  
  ads_path  <- test_path('sas/')
  ads_build <- ads_path %>% 
    adam_spec(
      filter = c(
        "ITTFL == 'Y'",
        "AVISIT == 'Baseline'",
        "ADSNAME == 'ADLB' & AVISIT == 'Visit 1'",
        "ABLFL == 'Y'"
      ),
      attach_data = TRUE,
      id          = "SUBJID"
    ) %>% 
    build(join = "adsl")
  
  # remove source path info for snapshot
  # (will be a different temp path, each time the test is run)
  attr(ads_build, "source") <- NULL
  
  # snapshot output value (in json2)
  expect_snapshot_value(
    ads_build, style = "json2"
  )
  
})


# test_that("factor levels correct", {
#   
#   # factor levels from catalog file ####
#   if(FALSE){
#   data_haven <- haven::read_sas(
#     test_path('sas/hadley.sas7bdat'),
#     catalog_file = test_path('sas/formats.sas7bcat')
#   )
#   
#   
#   levels_haven <- data_haven %>% 
#     haven::as_factor() %>% 
#     dplyr::select(dplyr::where(is.factor)) %>% 
#     purrr::map(levels)
#   
#   labels_haven <- data_haven %>% 
#     purrr::map(~attr(.x, 'label'))
#   
#   expect_equal(
#     purrr::map(data_haven,  ~{attributes(.x)}),
#     purrr::map(data_import$data, ~{attributes(.x)})
#   )
#   }
# 
# })