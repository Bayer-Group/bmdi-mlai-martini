test_that("step_other2() works", {
  
  n = 5
  threshold <- 2
  
  df <- tibble::tibble(
    # all classes large enough - should be kept unmodified
    no_mods = c(
      rep('large1', 2), 
      rep('large2', 3)
    ),
    # 'asis' should be kept by step_other2(), renamed by step_other() 
    single_low = c(
      rep('large', n-1), 
      rep('asis', 1)
    ),
    # no difference between step_other2() and step_other(): other_ml pools collapse1/2
    two_low = c(
      rep('large', n-2), 
      rep('collapse1', 1),
      rep('collapse2', 1)
    ),
    # other_ml pools collapse1/2 just like for two_low, yielding a constant column
    # note error for original recipes::step_other()
    clash = c(
      rep('other_ml', n-2), 
      rep('collapse1', 1),
      rep('collapse2', 1)
    )
    ) %>% 
    dplyr::mutate_all(factor)

  # df %>% map(~fct_count(.x, prop = TRUE))
  
  rcp_raw <- recipes::recipe(x = df) %>% 
    recipes::update_role(tidyselect::everything()) %>% 
    step_other2(
      recipes::all_predictors(),
      threshold = threshold
    )
  rcp_prepped <- prep(rcp_raw, training = df)
  rcp_prepped
  
df_baked  <- recipes::bake(rcp_prepped, new_data = df)
# df_baked %>% dput()
df_baked_reference <- structure(
  list(
    no_mods = structure(
      c(1L, 1L, 2L, 2L, 2L), 
      levels = c("large1", "large2"), 
      class = "factor"), 
    single_low = structure(
      c(2L, 2L, 2L, 2L, 1L), 
      levels = c("asis", "large"), 
      class = "factor"),
    two_low = structure(
      c(1L, 1L, 1L, 2L, 2L), 
      levels = c("large", "other_ml"), 
      class = "factor"),
    clash = structure(
      c(1L, 1L, 1L, 1L, 1L), 
      levels = "other_ml", 
      class = "factor")
    ), 
  class = c("tbl_df", "tbl", "data.frame"), 
  row.names = c(NA, -5L))

if(FALSE){
  # waldo::compare(df, df_baked_reference, x_arg = "original", y_arg = "baked")
  # see details on expected changes above in definition of df
  # no changes in variables 'no_mods' and 'single_low'
  
  # two_low: "collapse1" "collapse2" pooled into other_ml
  # `levels(original$two_low)`: "collapse1" "collapse2" "large"
  # `levels(baked$two_low)`:    "large"     "other_ml"         
  # 
  # `original$two_low`: "large    " "large    " "large    " "collapse1" "collapse2"
  # `baked$two_low`:    "large   "  "large   "  "large   "  "other_ml"  "other_ml" 
  #
  # clash: same as two_low
  # `levels(original$clash)`: "collapse1" "collapse2" "other_ml"
  # `levels(baked$clash)`:                            "other_ml"
  # 
  # `original$clash`: "other_ml " "other_ml " "other_ml " "collapse1" "collapse2"
  # `baked$clash`:    "other_ml"  "other_ml"  "other_ml"  "other_ml"  "other_ml" 
}

expect_equal(
  df_baked,
  df_baked_reference
)



if(FALSE){ # comparison with recipes::step_other()
  
  rcp_orig_step_other <- recipes::recipe(x = df) %>% 
    recipes::update_role(tidyselect::everything()) %>% 
    recipes::step_other(
      recipes::all_predictors(), -clash,
      threshold = threshold
      #, other = 'other_ml'
    )
  rcp_orig_step_other_prepped <- recipes::prep(rcp_orig_step_other, training = df)
  
  df_baked_orig_step_other <- recipes::bake(rcp_orig_step_other_prepped, new_data = df)
  df_baked_orig_step_other
  
  #if(rlang::is_installed("waldo")){
  #  waldo::compare(
  #    df_baked, df_baked_orig_step_other, 
  #    x_arg = "martini", y_arg = "recipes"
  #  )
  #}
}

})
