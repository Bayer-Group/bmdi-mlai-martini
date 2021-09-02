test_that("build_bds works", {
  
  # TEST setup ####
  
  file_adlb        <- test_path("sas/adlb.sas7bdat")
  file_adlb_miss   <- test_path("sas/adlb_miss.sas7bdat")
  file_adlb_rename <- test_path("sas/adlb_rename.sas7bdat")
  
  ads_spec_adlb <- martini:::adam_spec_bds(file_adlb, attach_data = TRUE)
  
  #  duplicate handling ####
  ref <- ads_spec_adlb$data %>% 
    dplyr::group_by(SUBJID, PARAMCD, AVISIT) %>% 
    dplyr::filter(dplyr::n()>1) %>% 
    dplyr::summarise(REF = mean(AVAL, na.rm = TRUE), .groups = "drop") %>% 
    tidyr::unite(PARAMCD, PARAMCD, AVISIT) %>% 
    dplyr::mutate(PARAMCD = stringr::str_replace_all(PARAMCD, ' ', '_'))
    
  
  dupl <- ref %>% 
    dplyr::pull(PARAMCD) %>% 
    unique()
  
  comp <- martini:::build_bds(spec = ads_spec_adlb)$data %>% 
    dplyr::select(tidyselect::all_of(c(".id", dupl))) %>% 
    tidyr::pivot_longer(-.id, names_to = "PARAMCD", values_to = "AVAL") %>% 
    dplyr::left_join(ref, by = c(".id" = "SUBJID", "PARAMCD"))
  
  testthat::expect_equal( # expect_identical
    comp$AVAL,
    comp$REF
  )
  
  # data dimensions ####
  
  
})
