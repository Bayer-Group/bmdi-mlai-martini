
# MISSING OUTCOME LABEL  ####

test_that("prepare_ml_outcome handles missing outcome label", {
  
  # classification ####
  set.seed(1719)
  n <- 10
  target_label_in_dict <- 'outcome'
  outcome <- sample(c("A", "B"), size = n, replace = TRUE) %>%
    tibble::as_tibble_col(target_label_in_dict) %>% 
    dplyr::mutate(.id = 1:dplyr::n(), .before = 1)
  
  prep_res <- prepare_ml_outcome(
    outcome
  )
  
  # label in dictionary = column name
  expect_equal(prep_res$outcome_label[['.out']], target_label_in_dict)
  
})

# NA REMOVAL  ####

test_that("outcome preparation works (NA removal)", {
  
  set.seed(1605)
  outcome <- c(NA,  rnorm(9)) %>%  tibble::as_tibble_col('num_outcome') %>% 
    dplyr::mutate(.id = 1:dplyr::n(), .before = 1)
  
  prep_res <- prepare_ml_outcome(
    outcome
  )
  
  # NA removal
  expect_equal(prep_res$outcome %>%  nrow, 9)
  
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
  
  # TODO remove hardcoding from test prepare_ml_outcome(), 1 and 9
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

