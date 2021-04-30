test_that("build_bds works", {
  
  # TEST setup ####
  
  # use system.file() to identify folder location after pkg is installed (system dependent)
  file_adlb        <- system.file("sas/adlb.sas7bdat", package = "martini")
  file_adlb_miss   <- system.file("sas/adlb_miss.sas7bdat", package = "martini")
  file_adlb_rename <- system.file("sas/adlb_rename.sas7bdat", package = "martini")
  # direct path specification if tests are run outside of pkg build process
  # (in this case 'system.file()' gives an empty string)
  if (file_adlb == "") {
    file_adlb        <- "inst/sas/adlb.sas7bdat"
    file_adlb_miss   <- "inst/sas/adlb_miss.sas7bdat"
    file_adlb_rename <- "inst/sas/adlb_rename.sas7bdat"
  }
  
  ads_spec_adlb <- martini::adam_spec_bds(file_adlb, attach_data = TRUE)
  
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
  
  comp <- martini::build_bds(spec = ads_spec_adlb)$data %>% 
    dplyr::select(tidyselect::all_of(c(".id", dupl))) %>% 
    tidyr::pivot_longer(-.id, names_to = "PARAMCD", values_to = "AVAL") %>% 
    dplyr::left_join(ref, by = c(".id" = "SUBJID", "PARAMCD"))
  
  testthat::expect_equal( # expect_identical
    comp$AVAL,
    comp$REF
  )
  
  # data dimensions ####
  
  
})
