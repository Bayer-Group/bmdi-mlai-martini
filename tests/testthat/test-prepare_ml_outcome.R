
# NA REMOVAL  ####

test_that("outcome preparation works (NA removal)", {
  
  set.seed(1605)
  outcome <- c(NA,  rnorm(9)) %>%  tibble::as_tibble_col('num_outcome') %>% 
    dplyr::mutate(.id = 1:dplyr::n(), .before = 1)
  
  prep_res <- prepare_ml_outcome(
    outcome
  )
  
  # NA removal
  testthat::expect_equal(prep_res$outcome %>%  nrow, 9)
  
})


# OUTLIER REMOVAL  #### 
test_that("outcome preparation works (outlier removal for regression)", {
  
  set.seed(1605)
  outcome <- c(20,  rnorm(9)) %>%  tibble::as_tibble_col('num_outcome') %>% 
    dplyr::mutate(.id = 1:dplyr::n(), .before=1)
  
  prep_res <- prepare_ml_outcome(
    outcome,
    outlier_remove = TRUE,
    outlier_ctrl   = list(coef = 3)
  )
  
  testthat::expect_equal(
    prep_res$id_outlier,
    1
  )
  
  testthat::expect_equal(
    nrow(prep_res$outcome),
    9
  )
  
  testthat::expect_length(
    prepare_ml_outcome(
      outcome,
      outlier_remove = TRUE,
      outlier_ctrl   = list(coef = 10)
    )$id_outlier,
    0
  )
})

