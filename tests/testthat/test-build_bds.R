
test_that("build_bds duplicate handling works", {
  file_adlb     <- test_path("sas/adlb.sas7bdat")
  ads_spec_adlb <- adam_spec_bds(file_adlb, attach_data = TRUE)
  

  # test duplicate message ####
  
  expect_message(build_bds(spec = ads_spec_adlb))
  
  spec_nodupes      <- ads_spec_adlb
  spec_nodupes$data <- spec_nodupes$data %>% 
    dplyr::distinct(SUBJID, PARAMCD, AVISIT, .keep_all = TRUE)
  
  # pivot_prepare_bds(, )
  expect_silent(build_bds(spec = spec_nodupes))
  
})

test_that("pivot_prepare_bds - values_fn deduced correctly", {
  
  file_adlb <- test_path("sas/adlb.sas7bdat")
  spec      <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  spec$values_fn <- mean
  
  target_fn <- median
  
  actual_fn <- pivot_prepare_bds(
    bds_full  = spec$data,
    spec      = spec,
    values_fn = target_fn
  )[["values_fn"]]
  
  expect_equal(target_fn, actual_fn)

})

test_that("pivot_prepare_bds - names_from argument deduced correctly", {
  
  file_adlb <- test_path("sas/adlb.sas7bdat")
  spec      <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  time_single <- spec$data[[spec$time]] %>% head(1)
  
  spec_single <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE,
    filter      = c(paste0(spec$time, " == '", time_single, "'"))
  )
  spec_multi  <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE
  )
  
  pivot_single <- pivot_prepare_bds(
    bds_full  = spec_single$data,
    spec      = spec_single
  )
  
  pivot_multi <- pivot_prepare_bds(
    bds_full  = spec_multi$data,
    spec      = spec_multi
  )
  
  expect_setequal(
    pivot_single$names_from,
    spec$param
  )
  
  expect_setequal(
    pivot_multi$names_from,
    c(spec$param, spec$time)
  )
  
})

test_that("build_bds - dict matches data set", {
  # TODO colnames have to match dict entries 1:1
  file_adlb <- test_path("sas/adlb.sas7bdat")
  spec      <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  time_single <- spec$data[[spec$time]] %>% head(1)
  
  spec_single <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE,
    filter      = c(paste0(spec$time, " == '", time_single, "'"))
  )
  spec_multi  <- adam_spec_bds(
    file_adlb,
    attach_data = TRUE
  )
  
  bds_wide_single <- build_bds(spec_single)
  bds_wide_multi  <- build_bds(spec_multi)
  
  expect_setequal(
    names(bds_wide_single$data) %>% setdiff(c(".id")),
    bds_wide_single$dict$column
  )
  
  expect_setequal(
    names(bds_wide_multi$data) %>% setdiff(c(".id")),
    bds_wide_multi$dict$column
  )
  
})

test_that("build_bds works", {
  
  # TODO structure build_bds expectations in different tests
  # TEST setup ####
  
  file_adlb        <- test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- test_path("sas/adlb_miss.sas7bdat")
  
  ads_spec_adlb <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  #  duplicate handling ####
  
  # reference data set with duplicates replaced by mean value?
  #   if values_fn is not specified, ie NULL, it is set to
  #   mean if all(is.numeric(x)), na.omit(x)[1] otherwise

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
  comp <- build_bds(spec = ads_spec_adlb)$data %>% 
    dplyr::select(tidyselect::all_of(c(".id", dupl))) %>% 
    tidyr::pivot_longer(-.id, names_to = "PARAMCD", values_to = "AVAL") %>% 
    dplyr::left_join(ref, by = c(".id" = "SUBJID", "PARAMCD"))
  
  expect_equal( # expect_identical
    comp$AVAL,
    comp$REF
  )
  
  # test  values_fn and arrange ####
  spec_arrange <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  # create duplicated data set:
  # the records with later (original) Date are all integers,
  # corresponding records with earlier Date are copied with 0.5 added
  # -> values_fn default: all means end in .25 or .75
  # -> last: all .5
  # -> last and desc(Date): all integer 
  
  spec_arrange$data <- spec_arrange$data %>% 
    dplyr::mutate(AVAL = AVAL + .5) %>% 
    dplyr::mutate(Date = as.Date('2021-01-01')) %>% 
    dplyr::bind_rows(spec_arrange$data %>% dplyr::mutate(Date = as.Date('2021-06-01')))
  
  
  
  # ...test values_fn parameter ####
  
  lb_valuefn_def <- build_bds(
    spec_arrange
  )$data %>% 
    dplyr::select(-.id) %>% 
    unlist()
  
  expect_true(
    all((lb_valuefn_def %% 1) %in% c(.25,.75))
  )
  
  lb_valuefn_custom <- build_bds(
    spec_arrange,
    dupl_ctrl = list( 
      values_fn = function(x){dplyr::last(x)}
    )
    )$data%>% 
    dplyr::select(-.id) %>% 
    unlist() 
    
  expect_true(
    all((lb_valuefn_custom %% 1) == 0)
  )
  
  # ...test arrange parameter  ####
  
  lb_valuefn_arrange <- build_bds(
    spec_arrange,
    dupl_ctrl = list( 
      values_fn = function(x){dplyr::last(x)},
      arrange = c("desc(Date)")
    )
  )$data %>% 
    dplyr::select(-.id) %>% 
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
    dplyr::pull(ads_spec_adlb$id) %>% 
    dplyr::n_distinct()
  
  target_ncol <- ads_spec_adlb$data %>%
    dplyr::filter(!! rlang::parse_expr(ads_spec_adlb$filter)) %>% 
    dplyr::select(tidyselect::any_of(c(ads_spec_adlb[c('time', 'param')] %>% unlist()))) %>% 
    dplyr::distinct() %>% 
    nrow() %>% 
    {.+1} # subj id
    
  
  expect_equal(
    build_bds(ads_spec_adlb)$data %>% dim(),
    c(target_nrow, target_ncol)
  )
})


test_that("build_bds conversion to factor/numeric from AVALC", {
  file_adlb <- testthat::test_path("sas/adlb.sas7bdat")
  spec_adlb <- adam_spec_bds(file_adlb, attach_data = TRUE)
  
  spec_adlb$data <- spec_adlb$data %>% 
    dplyr::mutate(AVALC = as.character(AVAL)) %>% 
    dplyr::mutate(AVALC = dplyr::case_when(
      PARAMCD == 'LAB1' ~ LETTERS[AVAL],
      TRUE ~ AVALC
    ))
  
  spec_adlb$value <- 'AVALC'
  build_adlb      <- build_bds(spec = spec_adlb)  
  expect_true(
    all(c('factor', 'numeric') %in%
      (build_adlb$data %>% dplyr::select(-.id) %>% purrr::map_chr(class))))
    
})
