test_that("check_feature() works", {
  # check_feature() ####
  
  # on package data: martini_feat
  res_pkg_feat <- check_feature(martini_feat, thres_low_freq = 15)
  
  #expect_invisible(check_feature(martini_feat, thres_low_freq = 15))
  
  expect_snapshot(
    check_feature(martini_feat, thres_low_freq = 15)
  )
})
