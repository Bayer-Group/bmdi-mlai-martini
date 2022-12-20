testthat::test_that("prepare_ml_vars() works", {
 
  n <- 27
  set.seed(1909)
  
  data <- tibble::tibble(
    count     = sample(0:3, size = n, replace = TRUE),
    count_dbl = as.double(count),
    log       = rnorm(n-1) %>% exp() %>% c(NA),
    nolump    = sample(c("a", "b"), size = n-1, replace = TRUE) %>% c("c") %>% factor(),
    imp       = rnorm(n-1) %>% c(NA),
    exclude   = rep(NA, n-2) %>% c("a", "b") %>% factor(),
    .out      = log
  )
  
  thres_log <- skw(data$log, na.rm = TRUE)/2
  
  testthat::expect_equal(
    
    prepare_ml_vars(
      data,
      thres_count = 4,
      thres_log   = thres_log,
      thres_lump  = 0.05,
      thres_imp   = 0.9
    ),
    
    list(
      count   = c("count", "count_dbl"), 
      log     = "log", 
      nolump  = "nolump", 
      imp     = c( "log", "imp"),
      exclude = "exclude"
    )
    
  )
})
