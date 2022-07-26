

testthat::test_that("check_filter() works", {
  
  test_filter <- c('mpg > 0', 'DISP < 200', 'cyl > 10')
  
  expect_equal(
    check_filter(mtcars, test_filter)$individual %>% 
      purrr::map_lgl("keep") %>% 
      as.logical(),
    c(TRUE, FALSE, FALSE)
  )
  
})
