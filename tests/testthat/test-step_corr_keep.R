test_that("step_corr_keep() works", {
  
  withr::with_seed(1717, {
    
    p <- 5
    corrm <- matrix(numeric(p^2), ncol = p, nrow = p)
    # variable 2 has a higher average correlation 
    # than all other variables
    corrm[,2] <- corrm[2,] <- .2
    # variable 1 and 2 have high correlation
    corrm[1,2] <- corrm[2,1] <- .9
    diag(corrm) <- 1
    
    X <- MASS::mvrnorm(n = 100, mu = rep(0, p), Sigma = corrm) %>% 
      tibble::as_tibble(.name_repair = ~paste0("V", 1:p))
    
    corrX <- list(pearson = cor(X), spearman = cor(X, method = "spearman"));
    cor_V1V2 <- purrr::map_dbl(corrX, ~.x[1, 2])
    
  })
  
  rec_base <- recipes::recipe(~., data = X) 
  
  # no `keep`, V2 should be discarded (larger avg correlation)
  rec_prep <- rec_base %>% 
    step_corr_keep(
      recipes::all_numeric_predictors(),
      threshold = .8
    ) %>% 
    recipes::prep()
  
  expect_equal(rec_prep$steps[[1]]$removals, "V2")
  
  expect_setequal(
    recipes::bake(rec_prep, new_data = NULL) %>% colnames(),
    setdiff(colnames(X), "V2")
  )
 
  # `keep = "V2"`, V1 should be discarded
  rec_keep_prep <- rec_base %>% 
    step_corr_keep(
      recipes::all_numeric_predictors(),
      threshold = .8,
      keep = c("V2")
    ) %>% 
    recipes::prep()
  
  expect_equal(rec_keep_prep$steps[[1]]$removals, "V1")
  
  expect_setequal(
    recipes::bake(rec_keep_prep, new_data = NULL) %>% colnames(),
    setdiff(colnames(X), "V1")
  )
  
  # `keep = c("V1", "V2")`, V2 should be discarded 
  # (larger avg correlation) and a message should be printed
  expect_message(
    rec_keep2_prep <- rec_base %>% 
      step_corr_keep(
        recipes::all_numeric_predictors(),
        threshold = .8,
        keep = c("V1", "V2")
      ) %>% 
      recipes::prep()
  )
  
  expect_equal(rec_keep2_prep$steps[[1]]$removals, "V2")
  
  expect_s3_class(rec_keep2_prep$steps[[1]]$high_corr, "tbl_df")
  
  # test that method argument works, should default to spearman
  # threshold set between cor spearman < pearson
  # -> one method should discard one variable, one should keep all
  vars_kept <- c("pearson", "spearman") %>% 
    purrr::set_names() %>% 
    purrr::map(
      ~ rec_base %>% 
        step_corr_keep(
          recipes::all_numeric_predictors(),
          threshold = mean(cor_V1V2),
          method = .x,
          keep = c("V2")
        ) %>% 
        recipes::prep() %>%
        bake(new_data = NULL) %>% 
        names()
    )
  
  # for the larger correlation, variable is removed
  expect_lt(
    vars_kept[[cor_V1V2 %>% which.max() %>% names()]] %>% length(),
    vars_kept[[cor_V1V2 %>% which.min() %>% names()]] %>% length()
  )
  
  #... more specifically, V2 is retained, V1 subject to removal
  expect_equal(
    sum(c(
      "V1" %in% vars_kept[[cor_V1V2 %>% which.max() %>% names()]],
      "V1" %in% vars_kept[[cor_V1V2 %>% which.min() %>% names()]]
    )),
    1
  )
  
  
})
