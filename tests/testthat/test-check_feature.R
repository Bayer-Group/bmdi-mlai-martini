test_that("check_feature() works", {
  # check_feature() ####
  
  # on package data: martini_feat
  res_pkg_feat <- check_feature(martini_feat, thres_low_freq = 15)
  
  #expect_invisible(check_feature(martini_feat, thres_low_freq = 15))
  
  expect_snapshot(
    check_feature(martini_feat, thres_low_freq = 15) %>% 
      purrr::modify_tree(leaf = tibble_to_JSON)
  )
})


test_that("check_freq() works", {
  # check_freq() works ####
  n     <- 100
  thres <- n/10  
  
  x <- tibble::tibble(
    num       = 1:n,
    fct_safe  = rep(c("A", "B"), each = n/2) %>% factor(),
    fct_risky = c(rep("A", n - thres/2), rep("B", thres/2)) %>% factor()
  )
  
  # details on factor level frequencies are returned invisibly
  expect_invisible(
    check_freq(x, thres = thres, quiet = TRUE)
  )
  
  # case 1: no factors: all empty/NA
  expect_equal(
    res1 <- x %>% 
      dplyr::select(num) %>% 
      check_freq(thres = thres, quiet = TRUE),
    list(
      vars        = character(0),
      counts      = structure(list(), names = character(0)), 
      overall_min = NA_integer_,
      finding     = FALSE,
      threshold   = 10,
      check       = "check_freq()"
    )
  )
  
  # case 2: factors, no risk: vars & counts empty, overall min of fct_safe
  expect_snapshot(
    res2 <- x %>% 
      dplyr::select(-fct_risky) %>%
      check_freq(thres = thres)
  )
  
  # case 3: at least one factor with min class size below thres
  expect_message(
    res3 <- check_freq(x, thres = thres),
    "fct_risky"
  )
  
  expect_snapshot(
    check_freq(x, thres = thres)
  )
  
  # case 4: pkg data
  # ... martini_feat 
  expect_message(
    res4 <- check_freq(martini_feat, thres = 50),
    "The following factors have low frequencies"
  )
  # ... martini_ml output object for automated slot selection
  # TODO move to check_feature
  # expect_message(
  #   res4 <- check_freq(martini_ml_class, thres = 50),
  #   "The following factors have low frequencies"
  # )
  
})



test_that("check_other_class() works", {
  # check_other_class() works ####
  # see also use_test("step_other2")
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
    # 'collapse' should be incorporated into the 'other_ml' class
    incorporate = c(
      rep('large', n-2), 
      rep('other_ml', 1),
      rep('collapse', 1)
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
  
  
  expect_message(
    x <- check_other_class(df), 
    "columns incorporate and clash"
  )
  
  expect_snapshot(
    x %>% purrr::modify_tree(leaf = tibble_to_JSON)
  )
  
})

test_that("check_non_missing() works", {
  # check_non_missing() works ####

  n <- 10
  p <- get_default(prepare_ml, "thres_imp")
  df <- tibble::tibble(
    all_miss = rep(NA, n), 
    only_one_kept = c(NA, 1:(n-1)),
    threshold = c(NA, NA, 1:(n*p)) 
  )
  # p <- 0.8; rep(NA, (n*(1-p))); rep(NA, n*(1-0.8)); (1-0.8); rep(NA, (n*.2))
  
  expect_message(
    res_check <- check_non_missing(
      df,
      thres = NULL,
      quiet = FALSE
    )
  )
  
  rcp_raw <- recipes::recipe(x = df) %>% 
    recipes::update_role(tidyselect::everything()) %>% 
    recipes::step_filter_missing(
      recipes::all_predictors()
    )
  rcp_prepped <- prep(rcp_raw, training = df)
  #rcp_prepped
  df_baked <- recipes::bake(rcp_prepped, new_data = df)
  
  # for reference: step_filter_missing() works as expected
  expect_equal(
    df_baked,
    df["only_one_kept"]
  )
  
  # verify that check_non_missing selects same columns as recipe step
  expect_setequal(
    res_check$vars,
    tidy(rcp_prepped, 1)$terms
  )
  
})


test_that("check_count() works", {
  # check_count() works ####
  
  n <- 20
  threshold <- 5
  df <- tibble::tibble(
    rownumber = 1:n,
    ints_with_neg = c(-1, 0, withr::with_seed(1433, sample(1:2, n-2, replace = TRUE))),
    guess_fct   = withr::with_seed(1433, sample(1:2, n, replace = TRUE)),
    guess_count = withr::with_seed(1344, sample(1:threshold, n, replace = TRUE))
  )
  
  expect_message(
    res_check <- check_count(
      df,
      thres   = threshold,
      non_neg = TRUE,
      quiet   = FALSE
    )
  )
  
  expect_setequal(
    res_check$vars,
    c("guess_fct", "guess_count")
  )
  
  expect_setequal(
    check_count(
      df,
      thres   = threshold,
      non_neg = FALSE,
      quiet   = FALSE
    )$vars,
    c("guess_fct", "guess_count", "ints_with_neg")
  )
})

test_that("check_nzv() works", {
  
  n <- 1000
  thres_freq   <- get_default(prepare_ml, "thres_nzv_freq")
  thres_unique <- get_default(prepare_ml, "thres_nzv_unique")
  
  df <- tibble::tibble(
    var_const = rep(1, n), 
    var_nzv = c(1, rep(2, n-1)), 
    keep = 1:n
  )
  
  expect_message(
    res_check <- check_nzv(
      df,
      thres_freq = thres_freq,
      thres_unique = thres_unique,
      quiet = FALSE
    ),
    "var_const and var_nzv"
  )
  
  removed <- recipes::recipe(df) %>% 
    recipes::update_role(
      tidyselect::everything(), 
      new_role = 'predictor'
    ) %>% 
    recipes::step_nzv(recipes::all_predictors(),
      freq_cut = thres_freq,
      unique_cut = thres_unique
    ) %>% 
    recipes::prep() %>% 
    recipes::tidy(number = 1) %>% 
    dplyr::pull(terms)
  
  
  # for reference: step_nzv() works as expected
  expect_equal(
    removed,
    c("var_const", "var_nzv")
  )
  
})
