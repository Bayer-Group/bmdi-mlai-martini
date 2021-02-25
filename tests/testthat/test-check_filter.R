
testthat::test_that("check_filter() works", {
  
  
  test_filter <- c('mpg > 0', 'DISP < 200', 'cyl > 10')
  
  testthat::expect_equal(
    check_filter(mtcars, test_filter),
    c(TRUE, FALSE, FALSE)
  )
  
})
