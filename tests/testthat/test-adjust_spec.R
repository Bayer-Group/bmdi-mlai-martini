test_that("check_adjust() works", {

  # md5 slot is protected, check message
  expect_message(
    check_adjust(
      spec = martini_spec, 
      entry = "adlb",
      list(md5 = 'fail')
    ), 
    "md5"
  )
    
})


test_that("adjust_spec works", {

  # TODO WS one expectation pair may be discarded, both using the same code in adjust_spec
  # TODO WS testing our function or just modify_list, append and if/else?

  # protected slot ####
  # return original object, when trying to change protected slot
  expect_equal(
    adjust_spec(
      spec = martini_spec,
      entry = "adlb",
      md5 = "fail"
    ),
    martini_spec
  )

# entry not in spec ####
missing_entry <- "adlb"
expect_error(
  adjust_spec(
    spec = martini_spec %>% magrittr::inset2(missing_entry, NULL),
    entry = missing_entry,
    id = "USUBJIDN"
  ),
  missing_entry
)

# refer to other funs for select, filter mods ####
# leave spec unmodified
# ... filter
expect_equal(
  adjust_spec(
    spec = martini_spec,
    entry = "adsl",
    filter = "SUBJID %% 2 == 0"
  ),
  martini_spec
)
expect_message(
  adjust_spec(
    spec = martini_spec,
    entry = "adsl",
    filter = "SUBJID %% 2 == 0"
  ),
  "adjust_spec_filter"
)


# ... select
expect_equal(
  adjust_spec(
    spec = martini_spec,
    entry = "adsl",
    select = c("SUBJID", "TRT01A", "SEX")
  ),
  martini_spec
)
expect_message(
  adjust_spec(
    spec = martini_spec,
    entry = "adsl",
    select = c("SUBJID", "TRT01A", "SEX")
  ),
  "adjust_adsl_select"
)

# factors ####
# set factors not yet recognized & modify factors labels
if(FALSE){
  martini_spec$adsl$factor_levels %>% names
  #[1] "TRT01A"  "AGEGR01" "SEX"     "RACE"  
  
}
pre_mod <- martini_spec$adsl$factor_levels

# reverse order for TRT01A
adjusted_trt_rev <- martini_spec %>% 
  adjust_spec(
    entry = "adsl",
    factor_levels = list(
      "TRT01A" = martini_spec$adsl$factor_levels$TRT01A %>% rev()
    )
  ) 
expect_equal(
  adjusted_trt_rev$adsl$factor_levels$TRT01A,
  pre_mod$TRT01A %>% rev()
)


# relabel TRT01A
adjusted_trt_relabel <- martini_spec %>% 
  adjust_spec(
    entry = "adsl",
    factor_levels = list(
      "TRT01A" = c("treated", "non-treated")
    )
  ) 


adjusted_add_var <- martini_spec %>% 
  adjust_adsl_select(add = "ITTFL") %>% 
  adjust_spec(
    entry = "adsl",
    factor_levels = list(
      "TRT01A" = c("treated", "non-treated")
    )
  ) 

})

test_that("adjust_adsl_select() works", {
# check_adjust_adsl_select

  # use select ####
  expect_snapshot(
    adjust_adsl_select(
      spec = martini_spec,
      select = c("SUBJID", "TRT01A", "SEX")
    )
  )
  # select wins over drop, add with message ####
  purrr::walk(c("ignored", "select"), ~{
    expect_message(
      adjust_adsl_select(
        spec = martini_spec,
        select = c("SUBJID", "TRT01A", "SEX"),
        add = "RANDDT",
        drop = "SEX"
      ),
      .x
    )
  })

  # add, drop ignored with select ####
  # work done by check_adjust_adsl_select
  expect_true(
    check_adjust_adsl_select(
      spec = martini_spec,
      select = c("SUBJID", "TRT01A", "SEX"),
      add = "RANDDT",
      drop = "SEX"
    ) %>%
      magrittr::extract(c("add", "drop")) %>%
      purrr::map_lgl(is.null) %>%
      all()
  )

  expect_equal(
    adjust_adsl_select(
      spec = martini_spec,
      select = c("SUBJID", "TRT01A", "SEX"),
      add = "RANDDT",
      drop = "SEX"
    ),
    adjust_adsl_select(
      spec = martini_spec,
      select = c("SUBJID", "TRT01A", "SEX"),
      add = NULL,
      drop = NULL
    )
  )

})

test_that("adjust_spec_filter() works", {
  
  expect_snapshot(
    adjust_spec_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0",
      append = TRUE
    )
  )
  expect_snapshot(
    adjust_spec_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0",
      append = FALSE
    )
  )
  
})

test_that("adjust_adsl_factors() works", {
  
  if(FALSE){
    # find variable in adsl not yet in factor_levels
    martini_spec$adsl$dict$param %>% 
      setdiff(names(martini_spec$adsl$factor_levels))
    
  }
  
  # for now: skip tests on spec class, entry name exists, factor_levels is named list
  
  # column does not exist in data ####
  # message and no modification 
  expect_message(
    check_adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "SX" = rev(martini_spec$adsl$factor_levels$SEX)
      ),
      entry = "adsl"
    ),
    "non-existing column"
  )
  expect_equal(
    adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "SX" = rev(martini_spec$adsl$factor_levels$SEX)
      ),
      entry = "adsl"
    ),
    martini_spec
  )
  
  # names are set (levels are values, labels are names)
  expect_equal(
    adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "ITTFL" = c("Y") %>% purrr::set_names()
      ), 
      entry = "adsl"
    ),
    adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "ITTFL" = c("Y")
      )
    )
  )
  
  # add factor one level missing  ####
  # warning and no modification
  expect_warning(
    check_adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "RACE" = martini_spec$adsl$factor_levels$RACE %>% 
          rev() %>% 
          tail(-1)
      )
    ),
    "missing the following existing level"
  )
  expect_equal(
    adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "RACE" = martini_spec$adsl$factor_levels$RACE %>% 
          rev() %>% 
          tail(-1)
      )
    ),
    martini_spec
  )
  
  
  # add factor additional level ####
  # message and modification
  RACE_plus <- martini_spec$adsl$factor_levels$RACE %>% 
    c("new_level") %>% purrr::set_names()
  expect_message(
    check_adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "RACE" = RACE_plus
      )
    ),
    "new level"
  )
  expect_equal(
    check_adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "RACE" = RACE_plus
      )
    )$RACE,
    RACE_plus
  )
  
  # add factor not yet in factor_levels ####
  # test redundant, is testing list_modify() 
  
  # see build_adsl() tests for usage of factors_levels 
})

