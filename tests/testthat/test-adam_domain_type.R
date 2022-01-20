test_that("adam_domain_type works", {
  
  # test setup  ####
  library(tibble)
  ads_path <- test_path('sas/')
  
  
  # print look-up table ####
  testthat::expect_true(
    adam_domain_type() %>% is_tibble()
  )
  
  testthat::expect_setequal(
    adam_domain_type() %>% names(),
    c('domain', 'adam_spec_*')
  )
  
  # mapped types tibble ####
  expect_setequal(
    names(adam_domain_type(ads_path)),
    c('domain', 'type', 'file')
  )
  
  expect_equal(
    nrow(adam_domain_type(ads_path)),
    list.files(ads_path) %>% length()
  )
  
  
  
})



# test area ####
if(FALSE){
  
  paths <- paste0('../../../',
                  c('', 'adcm.sas7bdat'))
  
  # '../../..//adxa.sas7bdat'
  
  # process path with unknown domains (adpr)
  adam_domain_type(path = paths[1])
  adam_domain_type(path = paths[1], quiet = TRUE)
  adam_domain_type(path = paths[1], quiet = TRUE) %>%  attr('unknown_domains')
  
  # process single file
  adam_domain_type(path = paths[2])
  
  # keep: files actually in path
  adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs'))  
  adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs.sas7bdat'))  
  
  # keep: files not found in path (typo, missing file selected)
  adam_domain_type(path = paths[1], keep = c('adqs'))  
  
  # keep/drop: keep  
  # info: Please specify only one of 'keep' or 'drop'. Only 'keep' will be used for subsetting here.
  adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs'), drop = 'advs')  
  
  # path: path doesn't exist
  # Error: The provided path does not exist
  adam_domain_type(path = str_remove(paths[1], 'Original/') )  
  
}