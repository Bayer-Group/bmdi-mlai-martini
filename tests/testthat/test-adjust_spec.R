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


test_that("adjust_spec correctly modifies use_for_build", {
  
  # use_for build FALSE at spec creation, 
  # check if set to TRUE after adding missing key columns
  spec_missing_key_columns <- martini_spec 
  
  # remove key column param (like it could not be guessed from the data)
  spec_missing_key_columns$adlb$use_for_build <- FALSE
  spec_missing_key_columns$adlb <- spec_missing_key_columns$adlb %>% 
    purrr::list_modify("param" = NULL)
  
  expect_true(
    spec_missing_key_columns %>% 
      adjust_spec("adlb", param = "PARAMCD") %>% 
      purrr::pluck("adlb", "use_for_build")
  )
  
  # direction TRUE to FALSE not tested, as it should not possible
  # messing up the spec as long as adjust_spec() is used
  # (only if data is not provided and columns can not be checked)
  
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
    "adjust_filter"
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
  
  expect_s3_class(
    adjust_spec(
      spec = martini_spec,
      entry = "adlb",
      md5 = "fail"
    ),
    "martini_spec"
  )
  
  # factors ####
  # set factors not yet recognized & modify factors labels
  if(FALSE){
    martini_spec$adsl$factor_levels %>% names
    #[1] "TRT01A"  "AGEGR01" "SEX"     "RACE"  
    
  }
  # ... factors
  # pointing to separate function, returning unmodified spec
  expect_equal(
    adjust_spec(
      spec = martini_spec,
      entry = "adsl",
      factor_levels = list(
        "TRT01A" = martini_spec$adsl$factor_levels$TRT01A %>% rev()
      )
    ),
    martini_spec
  )
  expect_message(
    adjust_spec(
      spec = martini_spec,
      entry = "adsl",
      factor_levels = list(
        "TRT01A" = martini_spec$adsl$factor_levels$TRT01A %>% rev()
      )
    ),
    "adjust_adsl_factors"
  )
  


})

test_that("adjust_adsl_select() works", {
# check_adjust_adsl_select
  
  select_orig <- martini_spec[["adsl"]][["select"]]
  
  add  <- "AGEGR01"
  drop <- "AGE"
  
  martini_spec_adj <- martini_spec %>% 
    adjust_adsl_select(
      add  = add,
      drop = drop
    ) 
  
  expect_s3_class(martini_spec_adj, "martini_spec")
  
  select_adj <- martini_spec_adj %>% 
    .[["adsl"]] %>% 
    .[["select"]]
  
  # check that spec's select slot is updated
  expect_setequal(
    c(select_orig, add) %>% unique() %>% setdiff(drop),
    select_adj
  )
  
  # check that dict column is updated
  expect_true(
    martini_spec_adj$adsl$dict %>%
      dplyr::select(param, selected) %>% 
      dplyr::filter(param %in% c(add, drop)) %>% 
      dplyr::mutate(expect = ifelse(param %in% add, TRUE, FALSE)) %>% 
      dplyr::mutate(match = (selected == expect)) %>% 
      dplyr::pull(match) %>% 
      all()
  )
  # TODO maybe later: check that data_info_ok is FALSE if data not attached
  
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

test_that("adjust_filter() works", {
  
  expect_snapshot(
    adjust_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0",
      append = TRUE
    )
  )
  expect_snapshot(
    adjust_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0",
      append = FALSE
    )
  )
  
  expect_s3_class(
    adjust_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0"
    ),
    "martini_spec"
  )
  
})

test_that("adjust_adsl_factors() works", {
  
  if(FALSE){
    # find variable in adsl not yet in factor_levels
    martini_spec$adsl$dict$param %>% 
      setdiff(names(martini_spec$adsl$factor_levels))
    
  }
  
  expect_s3_class(
    adjust_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0",
      append = FALSE
    ),
    "martini_spec"
  )
  
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
  expect_message(
    martini_spec_sx <- adjust_adsl_factors(
      spec = martini_spec,
      fctrs = list(
        "SX" = rev(martini_spec$adsl$factor_levels$SEX)
      )
    )
  )
  expect_equal(
    martini_spec_sx,
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
    purrr::quietly(adjust_adsl_factors)(
      spec = martini_spec,
      fctrs = list(
        "RACE" = martini_spec$adsl$factor_levels$RACE %>% 
          rev() %>% 
          tail(-1)
      )
    )$result,
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
