test_that("adjust_spec works", {
  
  # TODO WS one expectation pair may be discarded, both using the same code in adjust_spec
  # TODO WS testing our function or just modify_list, append and if/else?
  
  # value ####
  
  mod_md5 <- "martini"
  mod_id  <- "adlb"
  
  # ...overwrite ####
  
  expect_equal(
    martini_spec %>% 
      adjust_spec(mod_id, md5 = mod_md5) %>% 
      .[[mod_id]] %>% 
      .[["md5"]],
    mod_md5
  )
  
  # ... append ####
  
  expect_setequal(
    martini_spec %>% 
      adjust_spec(mod_id, md5 = mod_md5, append = TRUE) %>% 
      .[[mod_id]] %>% 
      .[["md5"]],
    c(martini_spec[[mod_id]][["md5"]], mod_md5)
  )
  
  # list ####
  
  mod_fct <- list(NEW = c("level 1", "level 2")) 
  mod_id  <- "adsl"
  
  names_factor <- martini_spec$adsl$factor_levels %>% names()
  
  # ...overwrite ####
  
  expect_equal(
    martini_spec %>% 
      adjust_spec(mod_id, factor_levels = mod_fct) %>% 
      .[[mod_id]] %>% 
      .[["factor_levels"]],
    mod_fct
  )
  
  # ...append ####
  expect_setequal(
    martini_spec %>% 
      adjust_spec(mod_id, factor_levels = mod_fct, append = TRUE) %>% 
      .[[mod_id]] %>% 
      .[["factor_levels"]] %>% 
      names(),
    c(names_factor, names(mod_fct)) %>% unique()
  )
  
})
