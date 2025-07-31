test_that("step_log_skew() works", {
  
  withr::with_seed(2116,{
    
    n <- 250
    X <- tibble::tibble(
      sym1 = rnorm(n, mean = 10, sd =1),
      sym2 = rnorm(n, mean = 100, sd =5),
      skw1 = exp(rnorm(n, mean = 1, sd = 2)),
      skw2 = exp(rnorm(n, mean = 0, sd = 1))
    )
  })
  
  rec <- recipes::recipe(~ ., data = X)
  
  rec_log <- rec %>% 
    step_log_skew(
      recipes::all_numeric_predictors(), 
      skewness = 2
    )
  
  rec_log_prep <- recipes::prep(rec_log)
  
  expect_setequal(
    unname(rec_log_prep$steps[[1]]$columns),
    c("skw1", "skw2")
  )
  
  X_baked  <- recipes::bake(rec_log_prep, new_data = NULL)
  X_mutate <- X %>% 
    dplyr::mutate(dplyr::across(skw1:skw2, log))
  
  expect_equal(X_baked, X_mutate, ignore_attr = TRUE)
  
  # also works, when no skewed variables are present
  rec_noskew_prep <- recipes::recipe(
    ~ ., 
    data = dplyr::select(X, tidyselect::starts_with("sym"))
  ) %>% 
    step_log_skew(
      recipes::all_numeric_predictors(), 
      skewness = 2
    ) %>% 
    prep()
  
  expect_length(rec_noskew_prep$steps[[1]]$columns, 0)
  
})
