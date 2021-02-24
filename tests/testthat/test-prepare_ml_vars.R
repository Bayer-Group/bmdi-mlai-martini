testthat::test_that("prepare_ml_vars() works", {
 
  # require(tidyverse)
  n <- 27
  set.seed(1909)
  
  data <- tibble::tibble(
    count   = sample(0:3, size = n, replace = TRUE),
    log     = rnorm(n) %>% exp(),
    nolump  = sample(c("a", "b"), size = n-1, replace = TRUE) %>% c("c") %>% factor(),
    imp     = rnorm(n-1) %>% c(NA),
    exclude = rep(NA, n-2) %>% c("a", "b") %>% factor(),
    .out    = log
  )
  
  testthat::expect_equal(
    
    prepare_ml_vars(
      data,
      thres_count = 4,
      thres_log   = 1.5,
      thres_lump  = 0.05,
      thres_imp   = 0.9
    ),
    
    list(
      count   = "count", 
      log     = "log", 
      nolump  = "nolump", 
      imp     = "imp", 
      exclude = "exclude"
    )
    
  )
})
