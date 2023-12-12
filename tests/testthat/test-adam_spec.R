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
  
  #skip_on_ci()
  withr::local_options(width = 80)
  
  ads_path <- test_path('sas/')
  ads_spec <- adam_spec(ads_path)
  
  # console output (print method)
  expect_snapshot(
    ads_spec
  )
  
  ads_spec_mod <- ads_spec
  # remove class to avoid print method
  class(ads_spec_mod) <- NULL
  # remove file path information (will be a different tmp file path each time the test is run)
  hide_file_path <- function(x){
    ifelse(
      # contained in every temp file path
      stringr::str_detect(x, "tests/testthat/"),
      "<REDACTED>", x
    )
  }
  
  expect_snapshot(
    ads_spec_mod, transform = hide_file_path
  )


})


test_that("adam_spec rds/sas selection works", {
  
  ads_path <- test_path('sas/file_ext_test')
  
  # create prep specification
  spec_sas_only   <- adam_spec(ads_path, file_ext = 'sas7bdat')
  spec_rds_only   <- adam_spec(ads_path, file_ext = 'rds')
  
  
  expect_true(spec_rds_only %>% purrr::map_chr(~{.x$file %>% purrr::map_chr(tools::file_ext)}) %>% {. == 'rds'}      %>% all())
  expect_true(spec_sas_only %>% purrr::map_chr(~{.x$file %>% purrr::map_chr(tools::file_ext)}) %>% {. == 'sas7bdat'} %>% all())
  
  # selection does not affect resulting spec object
  spec_rds_sas     <- adam_spec(ads_path, file_ext = c('rds', 'sas7bdat'))
  spec_rds_sas %>% purrr::map_chr('file') %>% purrr::map_chr(tools::file_ext)
  
  spec_sas_rds     <- adam_spec(ads_path, file_ext = c('sas7bdat', 'rds'))
  
  purrr::map_dfc( list(sas_rds = spec_sas_rds, rds_sas = spec_rds_sas), ~{
    .x %>% purrr::map_chr('file') %>% basename()
  }) %>% 
    dplyr::mutate(domain = sas_rds %>% tools::file_path_sans_ext(), .before = 1) %>% 
    dplyr::mutate_at(c('sas_rds', 'rds_sas'), tools::file_ext)
  
  # no difference with different file ext preference 
  expect_equal(spec_rds_sas, spec_rds_sas)
  
  
})
  
  