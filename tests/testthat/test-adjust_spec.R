test_that("check_adjust() works", {

  # md5 slot is protected, check message
  expect_message(
    check_adjust(
      spec = martini_spec, id = "adlb",
      list(md5 = 'fail')
    ), 
    'md5'
  )
  
  
    
})


test_that("adjust_spec works", {
  
  # TODO WS one expectation pair may be discarded, both using the same code in adjust_spec
  # TODO WS testing our function or just modify_list, append and if/else?
  
  # protected slot ####
  # return original object, when trying to change protected slot
  expect_equal(
    adjust_spec(
      spec = martini_spec, id = "adlb",
      md5 = "fail"
    ),
    martini_spec
  )
  
  # id not in spec ####
  missing_id <- "adlb"
  expect_error(
    adjust_spec(
      spec = martini_spec %>% magrittr::inset2(missing_id, NULL), 
      entry = missing_id,
      id = "USUBJIDN"
    ),
    missing_id
  )
  
  
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
  
  
})
