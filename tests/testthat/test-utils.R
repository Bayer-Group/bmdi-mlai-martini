test_that("corrr_mini works", {
  
  skip_if_not_installed("corrr")

  cor1 <- corrr::correlate(martini_feat, method = "pearson", quiet = TRUE) %>% 
    corrr::stretch(na.rm = TRUE) %>% 
    dplyr::arrange(x, y)
  
  cor2 <- corrr_mini(martini_feat) %>% 
    dplyr::arrange(x, y)
  
  expect_equal(cor1, cor2)
  
  # test edge case of just 2 numeric features
  expect_no_error(
    martini_feat %>% 
      dplyr::select(BMI, WEIGHT) %>% 
      corrr_mini()
  )
  
})
