# test_that("adam_guess works", {
#   
#   ads_path <- test_path('sas/')
#   
#   #adam_guess(ads_path)
#   
# 
#   })

# test area ####
#TODO WS write tests in test area in formal tests in test file adam_guess
if(FALSE){
  file <- "../../../adcm.sas7bdat"
  
  # works
  adam_guess(file)
  
  # file doesn't exist
  adam_guess(stringr::str_remove(file, 'Original/'))
  #testthat::expect_failure()
  
  # nothing guessed in adam_domain_type
  adam_guess(stringr::str_replace(file, 'prod/adcm', 'prod/adpr'))
  #testthat::expect_failure()
  
  # nothing guessed for label and time
  adam_guess(stringr::str_replace(file, 'prod/adcm', 'prod/advs'))
  
}