

testthat::test_that("check_filter() works", {
  
  library(purrr)
  
  test_filter <- c('mpg > 0', 'DISP < 200', 'cyl > 10')
  
  testthat::expect_equal(
    martini:::check_filter(mtcars, test_filter)$individual %>% 
      purrr::map_lgl("keep") %>% 
      as.logical(),
    c(TRUE, FALSE, FALSE)
  )
  
})
