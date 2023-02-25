test_that("adam_spec add_bds ", {
  
  ads_path <- test_path('sas/')
  
  add_name <- "adlb_miss"
  # create prep specification
  ads_spec     <- adam_spec(ads_path)
  ads_spec_add <- adam_spec(ads_path, add_bds = add_name)
  
  expect_setequal(
    names(ads_spec) %>% c(add_name),
    names(ads_spec_add)
  )

})

test_that("adam_spec keep/drop hierarchy ", {
  
  ads_path <- test_path('sas/')
  
  domains <- adam_spec(ads_path) %>% names()
  
  # If both \code{keep} and \code{drop} are specified, only \code{keep} will be used. 
  
  domains_keep <- domains[1:2]
  domains_drop <- domains_keep[1]
  
  ads_spec <- adam_spec(ads_path, keep = domains_keep, drop = domains_drop)

  expect_setequal(
    names(ads_spec),
    domains_keep
  )
  
})


test_that("adam_spec snapshots", {
  
  # COMBAK figure out how snapshot tests work on gitlab
  skip_on_ci()
  
  ads_path <- test_path('sas/')
  ads_spec <- adam_spec(ads_path)
  
  # console output (print method)
  expect_snapshot(
    ads_spec
  )
  
  ads_spec_mod <- ads_spec
  # remove file path information (will be a different tmp folder each time the test is run)
  for (i in seq_along(ads_spec_mod)) ads_spec_mod[[i]]$file <- NULL 
  # remove class to avoid print method
  class(ads_spec_mod) <- NULL
  
  expect_snapshot(
    ads_spec_mod
  )
  
})
