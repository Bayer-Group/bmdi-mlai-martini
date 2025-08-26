test_that("check_feature() works", {
  # check_feature() ####
  
  # on package data: martini_feat
  res_pkg_feat <- check_feature(martini_feat, thres_low_freq = 15)
  
  expect_setequal(
    res_pkg_feat %>% names(),
    c("low_freq", "other", "filter_missing", "count_vars")
  )
  
  #expect_invisible(check_feature(martini_feat, thres_low_freq = 15))
  
  expect_equal(
    res_pkg_feat,
    list(
      low_freq = list(
        vars = "angina_pectoris", 
        counts = list(
          angina_pectoris = structure(
            list(
              fct = structure(1:2, levels = c("no", "yes"), class = "factor"), 
                 n = c(275L, 14L)
            ), 
            class = c("tbl_df", "tbl", "data.frame"), 
            row.names = c(NA, -2L))), 
        overall_min = c(angina_pectoris = 14L)
      ), 
      other = character(0), 
      filter_missing = character(0), 
      count_vars = "AGE"
    )
  )
})
