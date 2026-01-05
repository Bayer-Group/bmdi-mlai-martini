test_that("prepare_ml_recipe() works", {
  
  martini_feat_char <- martini_feat %>% 
    dplyr::mutate(.id = stringr::str_pad(.id, width = 5, pad = "0")) %>% 
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))
  
  martini_outc_idchar <- martini_outc_class %>% 
    dplyr::mutate(.id = stringr::str_pad(.id, width = 5, pad = "0"))
  
  x <- dplyr::inner_join(
    martini_feat_char, 
    martini_outc_idchar,
    by = dplyr::join_by(.id)
  )
  
  the_formula <- as.formula(".out ~ .")
  
  rcp_true <- recipes::recipe(
    formula = the_formula, 
    data = x,
    # `strings_as_factors` only affects variables with role 'outcome' and 
    # 'predictor'. Role 'ID' is not affected, even though it is not defined yet 
    # (but in the next step)
    strings_as_factors = packageVersion("recipes") >= package_version("1.3.0")
  ) %>% 
    recipes::update_role(
      tidyselect::any_of(c(".id", ".rmtime")), 
      new_role = "ID"
    )
  
  rcp_false <- recipes::recipe(
    formula = the_formula, 
    data = x,
    strings_as_factors = !TRUE
  ) %>% 
    recipes::update_role(
      tidyselect::any_of(c(".id", ".rmtime")), 
      new_role = "ID"
    )
  
  # rcp_false %>% 
  #   prep(strings_as_factors = TRUE) %>% 
  #   bake(new_data = NULL) %>% 
  #   pull(.id) %>% 
  #   class() # fct
  # rcp_false %>% 
  #   prep(strings_as_factors = FALSE) %>% 
  #   bake(new_data = NULL) %>% 
  #   pull(.id) %>% 
  #   class() # chr
  # rcp_true %>%
  #   prep(strings_as_factors = TRUE) %>% 
  #   bake(new_data = NULL) %>% 
  #   pull(.id) %>% 
  #   class() # fct
  # rcp_true %>%
  #   prep(strings_as_factors = FALSE) %>% 
  #   bake(new_data = NULL) %>% 
  #   pull(.id) %>% 
  #   class()  # chr
  
  expect_type(
    prepare_ml_recipe(
      
      x, 
      
      thres_list = NULL,
      step_list  = NULL,
      
      one_hot = FALSE,
      log_base = 2
      
    ), 
    "list"
  )

  

  
})
