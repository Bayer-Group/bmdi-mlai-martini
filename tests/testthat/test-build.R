
library(dplyr)
library(purrr)
library(tibble)

test_that("build() correctly passes parameters to build_bds()", {
  
  path_test <- testthat::test_path("sas")
  
  spec_dupl <- adam_spec(path_test, keep = c('adsl', 'adlb'), attach_data = TRUE)
  
  # introduce duplicates (select by Date):
  # spec_dupl$adlb$data values are all integers
  spec_dupl$adlb$data <- spec_dupl$adlb$data %>%
    mutate(AVAL = AVAL + .5) %>% 
    mutate(Date = as.Date('2021-01-01')) %>% 
    bind_rows(spec_dupl$adlb$data %>% mutate(Date = as.Date('2021-06-01')))

  # check if 'values_fn' is correctly passed ####
  
  spec_arrange <- spec_dupl
  
  # change default for values_fn (mean -> last)
  spec_arrange$adlb$dupl_ctrl <- list(
    values_fn = dplyr::last, 
    arrange   = NULL
  )
  
  data_build_bds <- martini:::build_bds(spec_arrange$adlb)$data
  
  data_build <- build(spec_arrange)
  
  expect_equivalent(
    data_build %>% select(all_of(data_build_bds %>% names())),
    data_build_bds
  )
  
  # check if 'arrange' is correctly passed ####
  
  spec_arrange <- spec_dupl
  
  # change default for arrange (later to first Date)
  spec_arrange$adlb$dupl_ctrl <- list(
    values_fn = dplyr::last, 
    arrange   = c("desc(Date)")
  )
  
  data_build_bds <- martini:::build_bds(spec_arrange$adlb)$data
  
  data_build <- build(spec_arrange)
  
  expect_equivalent(
    data_build %>% select(all_of(data_build_bds %>% names())),
    data_build_bds
  )
  
})

