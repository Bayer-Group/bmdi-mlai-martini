
test_that("check_freq works", {
  
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
      check_freq(thres = thres),
    list(
      vars        = character(0),
      counts      = structure(list(), names = character(0)), 
      overall_min = NA_integer_
    )
  )
  
  # case 2: factors, no risk: vars & counts empty, overall min of fct_safe
  expect_equal(
    res2 <- x %>% 
      dplyr::select(-fct_risky) %>%
      check_freq(thres = thres),
    list(
      vars = character(0), 
      counts = structure(list(), names = character(0)), 
      overall_min = c(fct_safe = 50L)
    )
  )
  
  # case 3: at least one factor with min class size below thres
  expect_message(
    res3 <- check_freq(x, thres = thres),
    "fct_risky"
  )
  
  expect_equal(
    res3,
    list(
      vars = "fct_risky",
      counts = list(
        fct_risky = structure(
          list(
            fct = structure(1:2, levels = c("A", "B"), class = "factor"), 
            n = c(95L, 5L)
          ), 
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = c(NA, -2L))
        ), 
      overall_min = c(fct_risky = 5L))
  )
  
  # case 4: pkg data
  # ... martini_feat 
  expect_message(
    res4 <- check_freq(martini_feat, thres = 50),
    "The following factors have low frequencies"
  )
  # ... martini_ml output object for automated slot selection
  expect_message(
    res4 <- check_freq(martini_ml_class, thres = 50),
    "The following factors have low frequencies"
  )
  
  
  
})
