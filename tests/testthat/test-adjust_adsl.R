test_that("adjust_adsl", {
  
  library(martini)
  
  select_orig <- martini_spec[["adsl"]][["select"]]
  
  add  <- "AGEGR01"
  drop <- "AGE"
  
  select_adj <- martini_spec %>% 
    adjust_adsl(
      add  = add,
      drop = drop
    ) %>% 
    .[["adsl"]] %>% 
    .[["select"]]
  
  expect_setequal(
    c(select_orig, add) %>% unique() %>% setdiff(drop),
    select_adj
  )
  
})
