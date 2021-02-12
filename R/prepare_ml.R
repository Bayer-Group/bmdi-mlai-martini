#' Prepare ML ready data set from outcome and predictor data
#'
#' @param feature feature matrix in wide format, e.g. output object of \code{build()}, i.e. containing \code{.id} column and predictors 
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name if NULL (default), the first column that's not .id is chosen for outcome_name and the outcome_type is guessed to be either classification or regression.
#' One may also provide a single character giving the name of the outcome column OR a vector of length two giving the column names for the 'time' and 'status' data in survival analysis,
#' where .time is numeric and .status is binary with 0 coding for censored, and 1 coding for event. Currently, only right-censoring is supported. Please note, that survival will never be guessed
#' @param level_order  = NULL (only used for classification)
#' @param prep_recipe  = NULL,
#' @param train_prop   = 3/4
#' @param seed         = NULL,
#' @param prep_step_normalize = TRUE,
#' @param prep_step_knnimpute = TRUE,
#' @param prep_step_log       = TRUE,
#' @param prep_step_corr      = TRUE,
#' @param prep_step_dummy FALSE converted  variables to be 
#' @param thres_log           = 2,
#' @param thres_count    = 10
#' @param thres_corr          = .9,
#' @param thres_lump = 0.05
#' @param thres_imp = 0.8
#' @param thres_nzv_freq =95/5
#' @param thres_nzv_unique = 10
#' @param vars_imp_ignore = NULL
#' @param vars_fct_expl_na = NULL
#' @param vars_ordinalscore  = NULL, 
#' @param one_hot = TRUE
#' @param log_base            = exp(1),
#' @param outlier_remove      = FALSE,
#' @param outlier_ctrl        = list(coef = 3)
#' @param quiet =FALSE
#'
#'


prepare_ml <- function(
  feature,
  outcome,
  outcome_name = NULL,
  level_order  = NULL,
  prep_recipe  = NULL,
  train_prop   = 3/4,
  seed         = NULL,
  
  prep_step_normalize = TRUE,
  prep_step_knnimpute = TRUE,
  prep_step_log       = TRUE,
  prep_step_corr      = TRUE,
  prep_step_dummy     = TRUE,
  
  thres_log           = 2,
  thres_count         = 10,
  thres_corr          = 0.9,
  thres_lump          = 0.05,
  thres_imp           = 0.8,
  thres_nzv_freq      = 95/5, 
  thres_nzv_unique    = 10,
  
  vars_imp_ignore     = NULL,
  vars_fct_expl_na    = NULL,
  vars_ordinalscore   = NULL,
  
  one_hot             = TRUE,
  
  log_base            = exp(1),
  outlier_remove      = FALSE,
  outlier_ctrl        = list(coef = 3),
  
  quiet               = FALSE
  
){
  
  # OUTCOME ####
  
  outcome_prep <- prepare_ml_outcome(
    outcome        = outcome,
    outcome_name   = outcome_name,
    level_order    = level_order,
    outlier_remove = outlier_remove,
    outlier_ctrl   = outlier_ctrl
  )
  
  # (for code readability)
  outcome       <- outcome_prep$outcome
  outcome_name  <- outcome_prep$outcome_name
  outcome_label <- outcome_prep$outcome_label
  outcome_mode  <- outcome_prep$outcome_mode
  outcome_dict  <- outcome_prep$outcome_dict
  na_outcome    <- outcome_prep$na_outcome
  id_outlier    <- outcome_prep$id_outlier
  
  if (length(id_outlier)>0 && !quiet){
    usethis::ui_info(paste0(
      "Based on the outcome distribution, ", length(id_outlier),
      ifelse(length(id_outlier)>1, " observations were "," observation was "),
      "identified as outlier and removed from the outcome data prior to data splitting and preprocessing.\n"
    ))
  }
  
  if (length(na_outcome)>0 && !quiet){
    usethis::ui_info(paste0(
      length(na_outcome),
      ifelse(length(na_outcome)>1, " observations were "," observation was "),
      "removed from the outcome data prior to data splitting and preprocessing due to missingness.\n"
    ))
  }
  
  # FEATURE ####
  
  # ... define renaming vector ####
  # order matters!
  renaming <- c('<= |<=' = 'less_than_',  
                '> '  = 'over_',
                '< '  = 'under_',
                ' - ' = '_to_',  
                '>= |>=' = 'at_least_', 
                '<'   = 'under_' ,
                '>'   = 'over_',
                ' years|years' = '_y',
                '%'   = 'pct', 
                #' '  ='_',
                '[[:punct:]]|[[:space:]]' = '_',
                '_+'  = '_',
                '_$' = ''
  )
  
  
  # ... intersect 'vars_fct_expl_na' with factor columns ####
  if (!is.null(vars_fct_expl_na)){
    vars_fct_expl_na <- feature %>% 
      select_if(is.factor) %>% 
      colnames() %>% 
      intersect(vars_fct_expl_na)
    # catch special case 'no factors in feature'
    if (length(vars_fct_expl_na) == 0) vars_fct_expl_na <- NULL
  }
  
  # ... transform all character columns into factors (strips labels) ####
  feature <- feature %>% 
    dplyr::mutate_if(is.character, factor) %>% 
    dplyr::mutate_if(is.factor, ~ forcats::fct_relabel(., ~stringr::str_replace_all(., renaming) )) %>% 
    # add explicit NAs to selected factor variables (optional)
    {if(!is.null(vars_fct_expl_na)){
      dplyr::mutate_at(., vars_fct_expl_na, ~forcats::fct_explicit_na(.x, na_level = "missing"))
    }else{.}
    }
  
  # ... identify columns with 'Other' level ####
  level_other <- "other"
  
  vars_with_other <- feature %>% 
    purrr::map_lgl(~{any(stringr::str_to_lower(.) == stringr::str_to_lower(level_other))}) %>% 
    which() %>% 
    names()
  
  if(length(vars_with_other) > 0){
    feature <- feature %>% 
      dplyr::mutate_at(vars_with_other, ~{
        if (stringr::str_to_title(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_title(level_other))
        if (stringr::str_to_upper(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_upper(level_other))
        if (stringr::str_to_lower(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_lower(level_other))
      })
  }
  
  # MERGE OUTCOME AND FEATURE  ####
  
  d_raw <- outcome %>%
    dplyr::inner_join(feature, by = ".id")
  
  # DATA SPLIT ####
  
  if(!is.null(seed))  set.seed(seed)
  
  strata <- NULL
  if(outcome_mode == "classification") strata <- '.out'
  if(outcome_mode == "survival")       strata <- '.status'
  
  train_prop_valid <- c(0.5, 1)
  if (!dplyr::between(train_prop, train_prop_valid[1], train_prop_valid[2])){
    usethis::ui_stop(paste0(
      "The provided training proportion 'train_prop' is outside [",
      train_prop_valid[1], ", ", train_prop_valid[2], "]. Please check!"
    ))
  } 
  
  if (train_prop < 1){
    d_split     <- d_raw %>% rsample::initial_split(strata = tidyselect::all_of(strata), prop = train_prop)
    d_train_raw <- training(d_split)
    d_test_raw  <- testing( d_split)
  } else {
    d_split     <- NULL
    d_train_raw <- d_raw
    d_test_raw  <- NULL
  }
  
  #  RECIPE PREP ####
  # derive variable lists for steps
  
  # ... vars_count: identify integers with only a limited number of values ####
  vars_count <- NULL
  if (any(purrr::map_lgl(d_train_raw, is.integer))){
    vars_count <- d_train_raw %>% 
      dplyr::select_if(is.integer) %>% 
      tidyr::pivot_longer(-tidyselect::any_of(c(".id", ".out", ".status", ".time")), 
                          names_to = "PARAMCD", values_to = "AVAL") %>% 
      dplyr::group_by(PARAMCD) %>% 
      dplyr::summarise(NDIST = dplyr::n_distinct(AVAL)) %>% 
      dplyr::filter(NDIST <= thres_count) %>% 
      dplyr::pull(PARAMCD)
  }
  
  # ... vars_logtr: identify skewed parameters -> logtrafo later in recipe  ####
  vars_logtr <- NULL
  if (any(purrr::map_lgl(d_train_raw, is.numeric))){
    vars_logtr <- d_train_raw %>% 
      dplyr::select_if(is.numeric) %>% 
      tidyr::pivot_longer(-tidyselect::any_of(c(".id", ".out", ".status", ".time")), 
                          names_to = "PARAMCD", values_to = "AVAL") %>% 
      dplyr::group_by(PARAMCD) %>% 
      dplyr::mutate(MINAVAL = min(AVAL)) %>% 
      dplyr::filter(MINAVAL > 0) %>% 
      dplyr::summarise(skew = e1071::skewness(AVAL, na.rm = TRUE), .groups = "drop") %>% 
      dplyr::filter(skew > thres_log ) %>% 
      dplyr::pull(PARAMCD) %>% 
      setdiff(vars_count)
  }

  
  # ... prop_available: calculate proportion of missing values per column ####
  prop_available <- d_train_raw %>% 
    purrr::map_dbl(~ mean(!is.na(.))) %>% 
    tibble::enframe()
  
  # ... vars_nolump: factors to skip from step_other ####
  # if a single class falls below the threshold thres_lump, the class would be renamed to 'other'
  vars_nolump <- d_train_raw %>% 
    dplyr::select_if(is.factor) %>% 
    map_lgl( ~ { freqs <- table(.x)/ length(.x); sum(freqs < thres_lump) == 1  } )  %>% 
    which(.) %>% 
    names()
  
  # ... vars_imp: missing values will be knn imputed ####
  # var %in% 'vars_imp_ignore' : rows with missing values are dropped
  # else if thres_imp is not met vars are dropped
  vars_imp <- prop_available %>% 
    dplyr::filter(value >= thres_imp) %>% 
    dplyr::pull(name) %>% 
    setdiff(vars_imp_ignore) %>% 
    setdiff((c(".out", ".id", ".status", ".time")))
  
  # ... vars_exclude: variables with a large number of missing values are excluded ####
  vars_exclude <- prop_available %>% 
    dplyr::filter(value < thres_imp) %>% 
    dplyr::pull(name) %>% 
    setdiff((c(".out", ".id", ".status", ".time")))
    
    
  # RECIPE ####
  
  if (is.null(prep_recipe)){
    
    # ... formula ####
    if(outcome_mode %in% c('regression', 'classification')){
      the_formula <- as.formula(".out ~ .")
    }else{
      #the_formula <- as.formula("Surv(time = .time, event = .status, type = 'right') ~ .")
      the_formula <-  as.formula('.time + .status ~ .') # best guess...
    }  
   
    # ... write recipe ####
    # Note that order is important when building the recipe,
    # e.g. exclude variable before imputation, nzv and log before normalize 
    rcp <- recipes::recipe(the_formula, data = d_train_raw) %>% 
      recipes::step_rm(tidyselect::any_of(vars_exclude)) %>% 
      recipes::update_role(.id, new_role = "ID") %>% 
      
      # ... ... omit observations with missing endpoint ####
      recipes::step_naomit(recipes::all_outcomes()) %>% 
      
      # ... ... imputation ####
      {if(prep_step_knnimpute){
        recipes::step_knnimpute(., tidyselect::any_of(vars_imp)) }else{.}
      } %>% 
      
      # ... ... omit observations with missing data in variables ignored in imputation ####
      recipes::step_naomit(recipes::all_predictors()) %>% 
      
      # ... ... near zero variance ####
      recipes::step_nzv(recipes::all_predictors(),
                        freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique
      ) %>% 
      
      # ... ... log transformation ####
      {if(prep_step_log && length(vars_logtr)>0){
        recipes::step_log(., tidyselect::any_of(vars_logtr), base = log_base) 
      }else{.}
      }  %>%
      
      # ... ... normalization ####
      {if(prep_step_normalize){
        recipes::step_normalize(., 
          recipes::all_numeric(), -recipes::all_outcomes(), -recipes::has_role("ID"),
          # exclude vars identified as counts (previously excluded from logtrafo as well)
          -tidyselect::any_of(vars_count),
          )
        }else{.}
      }  %>% 
      
      # ... ... remove highly correlated variables ####
      {if(prep_step_corr){
        recipes::step_corr(., recipes::all_numeric(), -recipes::all_outcomes(), 
                           threshold = thres_corr, method = "pearson",
                           use = "pairwise.complete.obs")
      }else{.}} %>%  
      
      # ... ... lump factors ####
      recipes::step_other(., 
                          recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"),
                          -any_of(vars_nolump),
                          threshold = thres_lump, other = level_other) %>%  
      
      # ... ... factor handling ####
      {if(! is.null(vars_ordinalscore)){
        recipes::step_ordinalscore(.,  tidyselect::any_of(!! vars_ordinalscore ) )}else{.}
      } %>%  
      
      #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
      # ... .. dummy coding ####
      {if(prep_step_dummy){
        recipes::step_dummy(.,  recipes::all_nominal(), - recipes::all_outcomes(), - recipes::has_role("ID")  , 
                            one_hot = one_hot) }else{.} 
      }
    
    
  } else {
    rcp <- prep_recipe
  }
  
  # ... prep recipe ####
  rcp_prep <- rcp %>% 
    {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE)} %>% 
    pluck("result")
  
  d_train <- rcp_prep %>%  recipes::juice()
  
  if (train_prop < 1){
    d_test  <- rcp_prep %>% 
      {purrr::quietly(recipes::bake)(., d_test_raw)} %>% 
      pluck("result")
  } else {
    d_test  <- NULL
  }

  
  # CLEAN UP ####
  
  for (i in 1:ncol(d_train)){
    attr(d_train[[i]], "format.sas") <- NULL
    attr( d_test[[i]], "format.sas") <- NULL
    attr(d_train[[i]], "label"     ) <- NULL
    attr( d_test[[i]], "label"     ) <- NULL
  }
  attr(d_train, "label") <- NULL
  attr(d_test,  "label") <- NULL
  
  
  # DOCUMENT EXCLUDED ROWS AND COLUMNS ####
  
  # ... rows ####
  
  # ... ... na_feature ####
  na_feature <- d_raw$.id %>% 
    setdiff(na_outcome) %>% 
    setdiff(dplyr::bind_rows(d_train, d_test)$.id)
  
  if (length(na_feature) == 0) na_feature <- NULL

  # ... ... removed_rows: add outlier ids and NA outcome ids ####
  removed_rows <- list(
    outlier_outcome = id_outlier,
    na_outcome      = na_outcome,
    na_feature      = na_feature
  )
  
  # ... columns ####
  
  # extract prep step information
  prep_steps <- rcp_prep$steps
  
  # set names
  names(prep_steps) <- prep_steps %>% 
    purrr::map_chr(~{
      attr(.x, "class")[[1]][1] %>% 
        stringr::str_remove("^step_") %>% 
        # keep naming consistent with prep_params object
        stringr::str_replace("^rm$", "imp_ignore")
    })
  
  # create list of removed columns per step for output object
  removed_columns <- prep_steps %>% 
    purrr::map(~{.x$removal}) %>% 
    # keep all steps with a 'removal' slot
    purrr::keep(~{!is.null(.x)}) %>% 
    # set empty 'removal' slots (=vector of length 0) to NULL
    purrr::map(~{if(length(.x) > 0) .x})
  
  
  # DOCUMENT PREP PARAMETER SETTINGS ####
  # NOTE TEMP text slots will be removed once documentation is fully available
  # TODO  documentation of pre-processing parameters    
  prep_params <- list(
    
    # ... log trafo  ####
    thres_log  = list(
      value = ifelse(prep_step_log, thres_log, NA),
      text  = ifelse(prep_step_log,
                     paste0('Variables were log transformed (base ', 
                            ifelse(near(log_base, exp(1)), 'e', log_base),
                            ') if e1071::skewness() > ',  thres_log,
                            '. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details).'),
                     'No variables were logtransformed.')
    ),
    
    # ... log trafo excluded (integer with low number of values) ####
    thres_count  = list(
      value = ifelse( length(vars_logtr) > 0 && length(vars_count) > 0, 
                      thres_count, NA_real_),
      text  = ifelse(length(vars_logtr) > 0 && length(vars_count) > 0,
                     paste0('Variables were excluded from log transformation if they are integer coded 
                             and have ', thres_count, 'distinct values.'),
                            'Not applicable.')
    ),
    
    
    # ... correlated variables ####
    thres_corr  = list(
      value = ifelse(prep_step_corr, thres_corr, NA),
      text  = ifelse(prep_step_corr,
                     paste0('The applied cutoff for removal of variables due to high correlations was ',  thres_corr,'.'),
                     'No variables were removed for reasons of high correlation.')
    ),  
    
    # ... lump factor levels (always applied) ####
    thres_lump = list(
      value = thres_lump,
      text  = paste0('Low frequency factor levels were lumped using recipes::step_other(threshold = ', thres_lump, '). ')  
    ),
    
    # ... imputation/missing values  ####
    ## imputation/dropping of variables based on available probability
    imp_ignore = list(
      value = ifelse(prep_step_knnimpute, thres_imp, NA),
      text  = ifelse(prep_step_knnimpute,
                     paste0('Variables were dropped if the proportion of available data was less than ', 
                            thres_imp*100, '%.')  ,
                     'No imputation was done on the feature matrix.')
    ),

    # ... nzv ####
    nzv = list(
      value = list(freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique),
      text  = paste0('Highly sparse and unbalanced variables were dropped using ',  
                     'recipes::step_nzv(freq_cut = ', round(thres_nzv_freq) ,
                     ', unique_cut = ', thres_nzv_unique, ').'
      )
    )
    
  )    
  
  # ... outlier_remove ####
  # NOTE adjust to output of 'prepare_ml_outcome()'
  
  if(outcome_mode == 'regression' ){
    prep_params <- append(
      prep_params, 
      list(
        value = ifelse(outlier_remove,
                       unlist(outlier_ctrl), NA ),
        text  = ifelse(outlier_remove,
                       paste0("Based on the outcome distribution, observations outside the interval ",
                       '[q25 - ', outlier_ctrl$coef, '*iqr; ',  
                        'q75 + ', outlier_ctrl$coef, '*iqr] were removed prior to data splitting and preprocessing.'),
                       NA)
     )
    )
  } 
  

  # OUTPUT #### 
  
  prep_output <- list(
    data_raw = list(
      train = d_train_raw,
      test  = d_test_raw
    ),
    data_prep = list(
      train = d_train,
      test  = d_test 
    ),
    
    split = d_split,
    
    outcome = list(
      name = list('regression'     = '.out', 
                  'classification' = '.out', 
                  'survival'       = c('.time', '.status')
                  )[[outcome_mode]],
      mode = outcome_mode
    ),
    
    prep_recipe = rcp_prep,
    
    dict = dplyr::bind_rows(
      outcome_dict,
      attr(feature, "dict")  
      ) %>% 
      left_join(., 
          tibble::tibble(
            param = vars_logtr,
            logtr = "Y"
          ),
          by = c("param")
      ),
    
    prep_params = prep_params,
    
    removed = list(
      rows = removed_rows,
      cols = removed_columns
    )
    
  )
  
  prep_output
  
}




# dev
if(FALSE){
 # feature 
 # outcome 
  outcome_name = NULL
  level_order  = NULL
  prep_recipe  = NULL
  seed         = NULL
  
  prep_step_normalize = TRUE
  prep_step_knnimpute = TRUE
  prep_step_log       = TRUE
  prep_step_corr      = TRUE
  prep_step_dummy     = TRUE
  
  thres_log           = 2
  thres_count    = 10
  thres_corr          = 0.9
  thres_lump          = 0.05
  thres_imp           = 0.8
  thres_nzv_freq      = 95/5
  thres_nzv_unique    = 10
  
  vars_imp_ignore     = NULL
  vars_fct_expl_na    = NULL
  vars_ordinalscore   = NULL
  
  one_hot             = TRUE
  
  outlier_remove      = FALSE
  outlier_ctrl        = list(coef = 3)
}
