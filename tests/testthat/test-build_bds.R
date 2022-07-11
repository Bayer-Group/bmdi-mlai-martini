library(dplyr)

test_that("build_bds works", {
  
  # TEST setup ####
  
  file_adlb        <- testthat::test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- testthat::test_path("sas/adlb_miss.sas7bdat")
  file_adlb_rename <- testthat::test_path("sas/adlb_rename.sas7bdat")
  
  ads_spec_adlb <- martini:::adam_spec_bds(file_adlb, attach_data = TRUE)
  
  #  duplicate handling ####
  
  # reference data set with duplicates replaced by mean value
  ref <- ads_spec_adlb$data %>% 
    dplyr::group_by(SUBJID, PARAMCD, AVISIT) %>% 
    dplyr::filter(dplyr::n()>1) %>% 
    dplyr::summarise(REF = mean(AVAL, na.rm = TRUE), .groups = "drop") %>% 
    tidyr::unite(PARAMCD, PARAMCD, AVISIT) %>% 
    dplyr::mutate(PARAMCD = stringr::str_replace_all(PARAMCD, ' ', '_'))
    
  # parameters with duplicated values
  dupl <- ref %>% 
    dplyr::pull(PARAMCD) %>% 
    unique()
  
  # create comp for direct comparison of ref and test 
  comp <- martini:::build_bds(spec = ads_spec_adlb)$data %>% 
    dplyr::select(tidyselect::all_of(c(".id", dupl))) %>% 
    tidyr::pivot_longer(-.id, names_to = "PARAMCD", values_to = "AVAL") %>% 
    dplyr::left_join(ref, by = c(".id" = "SUBJID", "PARAMCD"))
  
  testthat::expect_equal( # expect_identical
    comp$AVAL,
    comp$REF
  )
  
  # test duplicate message ####
  
  expect_message(martini:::build_bds(spec = ads_spec_adlb))
  
  spec_nodupes <- ads_spec_adlb
  spec_nodupes$data <- spec_nodupes$data %>% dplyr::distinct(SUBJID, PARAMCD, AVISIT, .keep_all = TRUE)
  
  expect_silent(martini:::build_bds(spec = spec_nodupes))
  
  # test  values_fn and arrange ####
  spec_arrange <- martini:::adam_spec_bds(file_adlb, attach_data = TRUE)
  
  # create duplicated data set:
  # the records with later (original) Date are all integers,
  # corresponding records with earlier Date are copied with 0.5 added
  # -> values_fn default: all means end in .25 or .75
  # -> last: all .5
  # -> last and desc(Date): all integer 
  
  spec_arrange$data <- spec_arrange$data %>% 
    mutate(AVAL = AVAL + .5) %>% 
    mutate(Date = as.Date('2021-01-01')) %>% 
    bind_rows(spec_arrange$data %>% mutate(Date = as.Date('2021-06-01')))
  
  
  
  # ...test values_fn parameter ####
  
  lb_valuefn_def <- martini:::build_bds(
    spec_arrange
  )$data %>% 
    select(-.id) %>% 
    unlist()
  
  expect_true(
    all((lb_valuefn_def %% 1) %in% c(.25,.75))
  )
  
  lb_valuefn_custom <- build_bds(
    spec_arrange,
    dupl_ctrl = list( 
      values_fn = function(x){last(x)}
    )
    )$data%>% 
    select(-.id) %>% 
    unlist() 
    
  expect_true(
    all((lb_valuefn_custom %% 1) == 0)
  )
  
  # ...test arrange parameter  ####
  
  lb_valuefn_arrange <- build_bds(
    spec_arrange,
    dupl_ctrl = list( 
      values_fn = function(x){last(x)},
      arrange = c("desc(Date)")
    )
  )$data %>% 
    select(-.id) %>% 
    unlist() 
  
  expect_true(
    all((lb_valuefn_arrange %% 1) == .5)
  )
  
  # data dimensions ####
  ads_spec_adlb <- adam_spec_bds(
    file_adlb, 
    filter = "PARAMCD != 'LAB1'",
    attach_data = TRUE
    )
  
  target_nrow <- ads_spec_adlb$data %>%
    dplyr::filter(!! rlang::parse_expr(ads_spec_adlb$filter)) %>% 
    pull(ads_spec_adlb$id) %>% 
    n_distinct()
  
  target_ncol <- ads_spec_adlb$data %>%
    filter(!! rlang::parse_expr(ads_spec_adlb$filter)) %>% 
    select(any_of(c(ads_spec_adlb[c('time', 'param')] %>% unlist()))) %>% 
    distinct() %>% 
    nrow() %>% 
    {.+1} # subj id
    
  
  expect_equal(
    build_bds(ads_spec_adlb)$data %>% dim(),
    c(target_nrow, target_ncol)
  )
  
  # ... test conversion to factor/numeric
  spec_conv <- ads_spec_adlb
  spec_conv$data <- spec_conv$data %>% 
    mutate(AVALC = as.character(AVAL)) %>% 
    mutate(AVALC = case_when(
      PARAMCD == 'LAB1' ~ LETTERS[AVAL],
      TRUE ~ AVALC
    ))
  spec_conv$value <- 'AVALC'
  build_conv      <- martini:::build_bds(spec = spec_conv)  
  expect_true(all(c('factor', 'numeric') %in% (build_conv$data %>% select(-.id) %>% map_chr(class)) ))
    
})
