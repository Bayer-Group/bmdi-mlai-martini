test_that("step_log_skewness() works", {
  # step_log_skewness ####
  withr::with_seed(2116,{
    
    n <- 250
    X <- tibble::tibble(
      sym1 = rnorm(n, mean = 10, sd =1),
      sym2 = rnorm(n, mean = 100, sd =5),
      skw1 = exp(rnorm(n, mean = 1, sd = 2)),
      skw2 = exp(rnorm(n, mean = 0, sd = 1)),
      # add a constant (skewness = NaN)
      const = rep(1, n)
    )
  })
  
  rec <- recipes::recipe(~ ., data = X)
  
  # step_log_skewness() with skewed vars ####
  rec_log <- rec %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(), 
      skewness = 2
    ) 
  
  rec_log_prep <- recipes::prep(rec_log)
  
  expect_setequal(
    unname(rec_log_prep$steps[[1]]$columns),
    c("skw1", "skw2")
  )

  expect_equal(
    recipes::bake(rec_log_prep, new_data = NULL), 
    dplyr::mutate(X, dplyr::across(skw1:skw2, log)), 
    ignore_attr = TRUE
  )
  
  # step_log_skewness_undo() with skewed vars ####
  rec_log_undo <- rec_log %>% 
    step_log_skewness_undo(recipes::all_numeric_predictors())
  
  rec_log_undo_prep <- recipes::prep(rec_log_undo)
  
  expect_equal(
    recipes::bake(rec_log_undo_prep, new_data = NULL), 
    X,
    ignore_attr = TRUE
  )
  
  ## no skewed variables are present ####
  X_sym <-  dplyr::select(X, tidyselect::starts_with("sym"))
  rec_noskew_prep <- recipes::recipe(
    ~ ., 
    data = X_sym
  ) %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(), 
      skewness = 2
    ) %>% 
    prep()
  
  expect_length(rec_noskew_prep$steps[[1]]$columns, 0)
  
  expect_equal(
    recipes::bake(rec_noskew_prep, new_data = X_sym), 
    X_sym, 
    ignore_attr = TRUE
  )
  
  # step_log_skewness_undo() picks correct step to undo ####
  var_for_backtrafo <- "skw1"
  rec_multiple_log_specified <- rec %>% 
    step_log_skewness(
      tidyselect::all_of(var_for_backtrafo), 
      skewness = 1, id = "still_skewed"
    ) %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(),
      -tidyselect::all_of(var_for_backtrafo)
    ) %>% 
    step_log_skewness_undo(id_undo = "still_skewed")
  
  expect_setequal(
    var_for_backtrafo,
    rec_multiple_log_specified %>% 
      prep() %>% 
      tidy(3) %>% # number of step_log_skewness_undo in recipe
      dplyr::pull(terms)
  )
  
  # --- 
  var_for_backtrafo <- "skw2"
  rec_multiple_log_last <- rec %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(),
      -tidyselect::all_of(var_for_backtrafo)
    ) %>% 
    step_log_skewness(
      tidyselect::all_of(var_for_backtrafo),
      skewness = 1
    ) %>% 
    step_log_skewness_undo(id_undo = NULL)
  
  expect_setequal(
    var_for_backtrafo,
    rec_multiple_log_last %>% 
      prep() %>% 
      tidy(3) %>% # number of step_log_skewness_undo in recipe
      dplyr::pull(terms)
  )  
  
  #--- try reversing regular step_log should fail
  var_for_backtrafo <- "skw2"
  expect_error(
    rec %>% 
      step_log_skewness(skewness = Inf) %>% 
      recipes::step_log(var_for_backtrafo, id = "cannot_be_undone") %>% 
      step_log_skewness_undo(id_undo = "cannot_be_undone"),
    "log_skewness"
  )
  
  
  
  # log-transformed column is removed before the undo-step
  var_gone_missing <- "skw1"
  rec_var_gone_missing <- rec %>% 
    step_log_skewness(tidyselect::any_of(var_gone_missing), skewness = 0) %>% 
    recipes::step_rm(tidyselect::any_of(var_gone_missing)) %>% 
    step_log_skewness_undo(id_undo = NULL)
  expect_length(
    rec_var_gone_missing %>% 
      prep() %>% 
      tidy(3) %>%  # number of step_log_skewness_undo in recipe
      dplyr::pull(terms),
    0
  )
  
  # basic check: tidy gives tibble result for raw and prepped ----
  expect_s3_class(
    rec_log %>% tidy(),
    "tbl_df"
  )
  
  expect_s3_class(
    rec_log_prep %>% tidy(),
    "tbl_df"
  )
  
  expect_s3_class(
    rec_log_undo %>% tidy(),
    "tbl_df"
  )
  
  expect_s3_class(
    rec_log_undo_prep %>% tidy(),
    "tbl_df"
  )
  
  expect_s3_class(
    rec_noskew_prep %>% tidy(),
    "tbl_df"
  )
  
})

test_that("step_log_skewness_undo() works", {
  
  withr::with_seed(1653,{
    
    n <- 250
    X <- tibble::tibble(
      sym1 = rnorm(n, mean = 10, sd =1),
      sym2 = rnorm(n, mean = 100, sd =5),
      skw1 = exp(rnorm(n, mean = 1, sd = 2)),
      skw2 = exp(rnorm(n, mean = 0, sd = 1)),
      # add a constant (skewness = NaN)
      const = rep(1,n)
    )
  })
  
  rec <- recipes::recipe(~ ., data = X)
  
  rec_log_undo <- rec %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(), 
      skewness = 2
    ) %>% 
    step_log_skewness_undo(
      recipes::all_numeric_predictors()
    )
  
  rec_log_undo_prep <- recipes::prep(rec_log_undo)
  
  expect_equal(
    recipes::bake(rec_log_undo_prep, new_data = NULL), 
    X,
    ignore_attr = TRUE
  )
  
  rec_log_undo_base <- rec %>% 
    step_log_skewness(
      recipes::all_numeric_predictors(), 
      skewness = 2,
      base = 2, offset = .1
    ) %>% 
    step_log_skewness_undo(
      recipes::all_numeric_predictors()
    )
  
  rec_log_undo_base_prep <- recipes::prep(rec_log_undo)
  
  expect_equal(
    recipes::bake(rec_log_undo_base_prep, new_data = NULL), 
    X,
    ignore_attr = TRUE
  )
  
})

