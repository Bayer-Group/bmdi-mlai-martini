
test_that("check_freq() works", {
  # check_freq() works ####
  n     <- 100
  thres <- n/10  
  
  x <- tibble::tibble(
    num       = 1:n,
    fct_safe  = rep(c("A", "B"), each = n/2) %>% factor(),
    fct_risky = c(rep("A", n - thres/2), rep("B", thres/2)) %>% factor()
  )
  
  # details on factor level frequencies are returned invisibly
  expect_invisible(
    check_freq(x, thres = thres, quiet = TRUE)
  )
  
  # case 1: no factors: all empty/NA
  expect_equal(
   res1 <- x %>% 
      dplyr::select(num) %>% 
      check_freq(thres = thres, quiet = TRUE),
    list(
      vars        = character(0),
      counts      = structure(list(), names = character(0)), 
      overall_min = NA_integer_,
      finding     = FALSE,
      threshold   = 10,
      check       = "check_freq()"
    )
  )
  
  # case 2: factors, no risk: vars & counts empty, overall min of fct_safe
  expect_snapshot(
    res2 <- x %>% 
      dplyr::select(-fct_risky) %>%
      check_freq(thres = thres)
  )
  
  # case 3: at least one factor with min class size below thres
  expect_message(
    res3 <- check_freq(x, thres = thres),
    "fct_risky"
  )
  
  expect_snapshot(
    check_freq(x, thres = thres)
  )
  
  # case 4: pkg data
  # ... martini_feat 
  expect_message(
    res4 <- check_freq(martini_feat, thres = 50),
    "The following factors have low frequencies"
  )
  # ... martini_ml output object for automated slot selection
  # TODO move to check_feature
  # expect_message(
  #   res4 <- check_freq(martini_ml_class, thres = 50),
  #   "The following factors have low frequencies"
  # )
  
})
