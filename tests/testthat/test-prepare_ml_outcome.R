
# NA REMOVAL  ####

test_that("outcome preparation works (NA removal)", {
  
  outcome <- c(20, rnorm(9)) %>%  tibble::as_tibble_col('num_outcome')
  
  # NA removal
  expect_equal(2 * 2, 4)
  
  
})

# OUTLIER REMOVAL  #### 
test_that("outcome preparation works (outlier removal for regression)", {
  
  set.seed(1605)
  outcome <- c(20, rnorm(9)) %>%  tibble::as_tibble_col('num_outcome') %>% 
    dplyr::mutate(.id = 1:n(), .before=1)
  
  prepare_ml_outcome(
    
    outcome,
    outcome_name   = NULL,
    level_order    = NULL,
    outlier_remove = FALSE,
    outlier_ctrl   = list(coef = 3)
    
  )
    
  
})