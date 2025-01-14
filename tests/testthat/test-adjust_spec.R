test_that("check_adjust() works", {

  # md5 slot is protected, check message
  expect_message(
    check_adjust(
      spec = martini_spec, 
      entry = "adlb",
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




# list ####

# mod_fct <- list(NEW = c("level 1", "level 2"))
# mod_id  <- "adsl"
#
# names_factor <- martini_spec$adsl$factor_levels %>% names()
#
# # ...overwrite ####
#
# expect_equal(
#   martini_spec %>%
#     adjust_spec(mod_id, factor_levels = mod_fct) %>%
#     .[[mod_id]] %>%
#     .[["factor_levels"]],
#   mod_fct
# )


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
  # adjust_spec_filter() works ####

  expect_snapshot(
    adjust_spec_filter(
      spec = martini_spec,
      filter = "SUBJID %% 2 == 0"
    )
  )

})

