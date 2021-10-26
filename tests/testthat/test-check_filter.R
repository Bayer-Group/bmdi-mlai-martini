library(purrr)

testthat::test_that("check_filter() works", {
  
  
  test_filter <- c('mpg > 0', 'DISP < 200', 'cyl > 10')
  
  testthat::expect_equal(
    martini:::check_filter(mtcars, test_filter)$individual %>% 
      map_lgl("keep") %>% 
      as.logical(),
    c(TRUE, FALSE, FALSE)
  )
  
})
