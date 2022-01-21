
test_that("check freq works", {
  
  n     <- 100
  thres <- n/10  
  
  x <- tibble::tibble(
    num       = 1:n,
    fac_safe  = rep(c("A", "B"), each = n/2) %>% factor(),
    fac_risky = c(rep("A", n - thres/2), rep("B", thres/2)) %>% factor()
  )
  
  # case 1: no factors
  expect_message(
    x %>% dplyr::select(num) %>% check_freq(thres = thres),
    "Data does not contain any factors."
  )
  
  # case 2: factors, no risk
  expect_message(
    x %>% dplyr::select(-fac_risky) %>% check_freq(thres = thres),
    regexp = 'Minimum observed class size is '
  )
  
  # case 3: at least one factor with min class size below thres
  expect_named(
    check_freq(x, thres = thres),
    'fac_risky'
  )
  
  # try prepare_ml output object for automated slot selection
  expect_message(check_freq(martini_ml_regr, thres = thres))
  
  
})
