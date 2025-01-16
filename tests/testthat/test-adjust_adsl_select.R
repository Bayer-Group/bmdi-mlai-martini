test_that("adjust_adsl", {
  
  select_orig <- martini_spec[["adsl"]][["select"]]
  
  add  <- "AGEGR01"
  drop <- "AGE"
  
  martini_spec_adj <- martini_spec %>% 
    adjust_adsl_select(
      add  = add,
      drop = drop
    ) 
  
  select_adj <- martini_spec_adj %>% 
    .[["adsl"]] %>% 
    .[["select"]]
  
  # check that spec's select slot is updated
  expect_setequal(
    c(select_orig, add) %>% unique() %>% setdiff(drop),
    select_adj
  )
  
  # check that dict column is updated
  expect_true(
    martini_spec_adj$adsl$dict %>%
      dplyr::select(param, selected) %>% 
      dplyr::filter(param %in% c(add, drop)) %>% 
      dplyr::mutate(expect = ifelse(param %in% add, TRUE, FALSE)) %>% 
      dplyr::mutate(match = (selected == expect)) %>% 
      dplyr::pull(match) %>% 
      all()
  )
  
})
