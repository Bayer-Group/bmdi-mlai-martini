
require(martini)
require(purrr)
require(dplyr)

# reference object
d_ml <- prepare_ml(
  feature = martini_feat,
  outcome = martini_outc_class, 
  seed    = 1314
)

# split object
split_by   <- "RACE"
d_ml_split <- prepare_ml_split(d_ml, by = split_by)


test_that("prepare_ml_split creates the same structure than prepare_ml", {
  
  d_ml_split <- prepare_ml_split(d_ml)
  
  for (i in length(d_ml_split)){
    
    expect_equal(
      names(d_ml_split[[i]]),
      names(d_ml)
    )
    
  }
  
})

test_that("prepared data in split objects contains all variables except for the split variable", {

  # one-hot encoding (default) ####
  
  split_levs <- d_ml$data_raw$train[[split_by]] %>% levels()
  
  var_ref <- d_ml$data_prep$train %>% 
    names()
  
  var_split <- map(d_ml_split, ~{
    
    .x$data_prep$train %>% 
      names()
    
  })
  
  for (i in length(var_split)){
    
    expect_equal(
      var_split[[i]],
      setdiff(var_ref, c(split_by, paste0(split_by, "_", split_levs)))
    )
    
  }
  
  # no dummy coding ####
  
  d_ml_nodummy <- prepare_ml(
    feature         = martini_feat,
    outcome         = martini_outc_class,
    prep_step_dummy = FALSE,
    seed            = 1314
  )
  
  d_ml_nodummy_split <- prepare_ml_split(d_ml_nodummy, by = split_by)
  
  var_ref <- d_ml_nodummy$data_prep$train %>% 
    names()
  
  var_split <- map(d_ml_nodummy_split, ~{
    
    .x$data_prep$train %>% 
      names()
    
  })
  
  for (i in length(var_split)){
    
    expect_equal(
      var_split[[i]],
      setdiff(var_ref, c(split_by, paste0(split_by, "_", split_levs)))
    )
    
  }
  
})

test_that("by variable removed from formulae", {
  
  split_by_regex <- c(split_by, paste0(split_by, "_", split_levs)) %>% 
    paste0('\\b', ., '\\b') %>% 
    paste(collapse = '|')
  
  expect_false(
    map(d_ml_split, 'prep_recipe') %>% 
      map(formula) %>% 
      map(~stringr::str_detect(rlang::f_text(.x), split_by_regex)) %>% 
      unlist() %>% 
      any()
  )
    
}
)

test_that("subjects split is correct", {
  
  subj_split <- d_ml_split %>% 
    imap(~{
      .x[["data_raw"]] %>% 
        imap_dfr(~{.x %>% mutate(.split_split = .y)}) %>% 
        mutate(.group_split = .y) %>% 
        select(.id, .group_split, .split_split)
    }) %>% 
    reduce(bind_rows)
    
  subj_orig <- d_ml$data_raw %>% 
    imap_dfr(~{.x %>% mutate(.split_orig = .y)}) %>% 
    select(all_of(c(".id", ".group_orig" = split_by, ".split_orig"))) %>% 
    mutate(.group_orig = as.character(.group_orig))
    
  subj_comp <- subj_orig %>% 
    left_join(subj_split, by = ".id")
  
  expect_equal(
    subj_comp$.group_split,
    subj_comp$.group_orig
  )
  
  # exclude NAs of the splitting variable in the original data
  subj_comp_nona <- subj_comp %>% filter(!is.na(.group_split))
  
  expect_equal(
    subj_comp_nona$.split_split,
    subj_comp_nona$.split_orig
  )
  
})
