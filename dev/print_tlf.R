# rec <- martini_ml_regr$prep_recipe

dict_full <- martini_spec %>% 
  purrr::map('dict') %>% 
  list_rbind(names_to = 'spec_entry')

collect_info <- function(
    rec
){
  
  stopifnot('recipe' %in% class(rec))
  
  if(is.null(rec$steps)) return(NULL) # TODO error handling
  
  rec$steps %>% 
    purrr::set_names(., purrr::map_chr(., 'id')) %>% 
    purrr::map(~{
      
      suffix <- class(.x) %>% setdiff('step') %>% stringr::str_remove('^step_')
      
      cols_available <- c('rm', 'nzv', 'impute_knn', 'corr', 'other', 'log', 'normalize', 'naomit') 
      if(! suffix %in% cols_available) return(NULL)
      
      res <- do.call(
        paste0('cols_', suffix),
        list(.x)
      )
      
    }) 
}

preprocess_steps <- function(
  rec
){
  
  full_info <- collect_info(rec) 
  
  settings <- full_info %>%  
    purrr::map('settings') %>% 
    purrr::compact() %>%  
    purrr::map(~unlist(.x) %>% 
      tibble::enframe() %>% 
      tidyr::unite(set, tidyselect::everything(), sep = ' = ') %>% 
      dplyr::pull() %>% 
      paste(collapse = ', ')
    ) %>% 
    unlist() %>%
    tibble::enframe(name = 'id', value = 'settings')
  
  affected <- full_info %>% 
    purrr::map('columns') %>% 
    purrr::compact() %>% 
    purrr::map(~paste(sort(.x), collapse = ', ')) %>% 
    unlist() %>%
    tibble::enframe(name = 'id', value = 'related variables')
  
  
  recipes::tidy(rec) %>% 
    dplyr::select(any_of(c('number', 'type', 'id'))) %>% 
    dplyr::left_join(settings, by = dplyr::join_by(id)) %>% 
    dplyr::left_join(affected, by = dplyr::join_by(id)) %>% 
    dplyr::select(-tidyselect::any_of('id'))
}

# extract settings/general parameters ####
# e.g. log base, thresholds
# step_settings <- function(
#     rec
# ){
#   collect_info(rec) %>%  map('settings')
#   
# }



# extract affected columns  ####
# cols_affected <- function(
#     rec
# ){
#   
#   collect_info(rec) %>%
#     map('columns') %>% 
#     unname() 
#   
# }




# removals ####
# ...column ####
cols_rm <- function(
  x
){
  
  list(
    columns  = x$removals,
    id       = x$id 
  )
  
}

cols_nzv <- function(
    x
){
  list(
    columns  = x$removals,
    settings = x[c('freq_cut', 'unique_cut')],
    id       = x$id
  )
}

cols_zv <- function(
    x
){
  list(
    columns  = x$removals,
    settings = x[c('freq_cut', 'unique_cut')],
    id       = x$id
  )
}


cols_corr <- function(
  x
){
  list(
    columns  = x$removals,
    settings = x[c('method', 'use', 'threshold')],
    id       = x$id
  )
}

## ... rows ####
cols_naomit <- function(
    x
){
  list(
    columns  = x$columns,
    id       = x$id
  )
}

# modifications ####
# ... imputation ####
cols_impute_knn <- function(
    x
){
  # $columns is unnamed nested list where each entry represents one imputed variable
  # each entry is named list of length 2 where
  #  - y is character of length 1 giving the column imputed
  #  - x character vector giving columns used for imputation (supposedly)
  list(
    columns  = x$columns %>% purrr::map_chr('y'),
    settings = x[c('neighbors')],
    derived  = list(used = x$columns %>% purrr::map('x') %>% unlist() %>% unique()), 
    id       = x$id     
  )
}

cols_impute_median <- function(
    x
){
  
  meds <- x$medians
  
  list(
    columns  = names(meds),
    derived  = lst(meds),
    id       = x$id     
  )
}

cols_impute_mean <- function(
    x
){
  
  means <- x$means
  
  list(
    columns  = names(means),
    derived  = tibble::lst(means),
    settings = x[c('trim')],
    id       = x$id     
  )
}

cols_impute_mode <- function(
    x
){
  
  modes <- x$modes
  
  list(
    columns  = names(modes),
    derived  = tibble::lst(modes),
    id       = x$id     
  )
}

if(FALSE){
  data("credit_data", package = "modeldata")
  
  ## missing data per column
  vapply(credit_data, function(x) mean(is.na(x)), c(num = 0))
  
  set.seed(342)
  in_training <- sample(1:nrow(credit_data), 2000)
  
  credit_tr <- credit_data[in_training, ]
  credit_te <- credit_data[-in_training, ]
  missing_examples <- c(14, 394, 565)
  
  rec <- recipe(Price ~ ., data = credit_tr)
  
  # MODE
  impute_rec_mode <- rec %>%
    step_impute_mode(Status, Home, Marital)
  imp_models_mode   <- prep(impute_rec_mode,   training = credit_tr)
  
  # MEDIAN
  impute_rec_median <- rec %>%
    step_impute_median(Income, Assets, Debt)
  imp_models_median <- prep(impute_rec_median, training = credit_tr)
  
  # MEAN
  impute_rec_mean <- rec %>%
    step_impute_mean(Income, Assets, Debt)
  imp_models_mean <- prep(impute_rec_mean, training = credit_tr)
}

# ... factors ####

cols_dummy <- function(
    x
){
  
  list(
    columns  = x$columns,
    settings = x[c('one_hot')], #, 'preserve'
    id       = x$id
  )
  
}

if(FALSE){
  
  data(Sacramento, package = "modeldata")
  
  # factor city with 37 levels
  rec <- recipe(~ city + sqft + price, data = Sacramento)
  
  # Default dummy coding: 36 dummy variables
  dummies <- rec %>%
    step_dummy(city) %>%
    prep(training = Sacramento)
  
  dummies$steps[[1]]
}

cols_ordinalscore <- function(
    x
){
  
  list(
    columns  = x$columns,
    id       = x$id
  )
  
}

if(FALSE){
  
fail_lvls <- c("meh", "annoying", "really_bad")

ord_data <-
  data.frame(
    item = c("paperclip", "twitter", "airbag"),
    fail_severity = factor(fail_lvls,
                           levels = fail_lvls,
                           ordered = TRUE
    )
  )

model.matrix(~fail_severity, data = ord_data)

linear_values <- recipe(~ item + fail_severity, data = ord_data) %>%
  step_dummy(item) %>%
  step_ordinalscore(fail_severity)

linear_values <- prep(linear_values, training = ord_data)

}

# ... misc ####
cols_log <- function(
  x
){
  
  list(
    columns  = x$columns,
    settings = x[c('base', 'offset')], #, 'signed'
    id       = x$id
  )
  
}

cols_other <- function(
  x
){
  # $steps$<other>$objects is list of available factors at this point: 
  # columns removed in earlier step (e.g. nzv) not listed
  
  cols <- x$objects %>% 
    tibble::enframe() %>% 
    tidyr::unnest_wider(value) %>% 
    dplyr::filter(collapse) %>% 
    pull(name)
  
  list(
    columns  = cols,
    settings = x['threshold'],
    derived  = NULL,   # TODO
    id       = x$id 
  )
  
}

# also for step_center() (means only), and step_scale()
cols_normalize <- function(
    x
){
  # $steps$<other>$objects is list of available factors at this point: 
  # columns removed in earlier step (e.g. nzv) not listed
  vars <- intersect(names(x), c('means', 'sds'))
  
  info <- x[vars] %>%
    purrr::map(tibble::enframe) %>% 
    purrr::list_rbind()
  
  cols <- info %>% 
    pull(name) %>% 
    unique() %>% 
    sort()
  
  list(
    columns  = cols,
    derived  = info,
    id       = x$id
  )
  
}


cols_mutate_at <- function(
    x
){
  list(
    columns = x$inputs,
    id      = x$id 
  )
}



# refs ####
if(FALSE){
  require(martini)
  require(tidyverse)
  
  rec <- martini_ml_regr$prep_recipe
  
  recipes::print_step()
  recipes:::print.step_normalize()
  #https://github.com/tidymodels/recipes/blob/36a1307656ea7804eff127fd2294cc1f2dd9421e/R/printing.R
  
  martini_feat %>% apply(2, function(x)sum(is.na(x))) %>%  keep(~ .x>0) %>%  length
  

}