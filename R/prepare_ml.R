#'
#'
#' @param feature feature matrix in wide format, e.g. output object of \code{build()}, i.e. containing \code{.id} column and predictors 
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name if NULL (default), the first column that's not .id is chosen for outcome_name and the outcome_type is guessed to be either classification or regression.
#' One may also provide a single character giving the name of the outcome column OR a vector of length two giving the column names for the 'time' and 'status' data in survival analysis,
#' where .time is numeric and .status is binary with 0 coding for censored, and 1 coding for event. Currently, only right-censoring is supported. Please note, that survival will never be guessed
#' @param level_order  = NULL (only used for classification)
#' @param prep_recipe  = NULL,
#' @param seed         = NULL,
#' @param prep_step_normalize = TRUE,
#' @param prep_step_knnimpute = TRUE,
#' @param prep_step_log       = TRUE,
#' @param prep_step_corr      = TRUE,
#' @param prep_step_dummy FALSE converted  variables to be 
#' @param thres_log           = 2,
#' @param thres_corr          = .9,
#' @param thres_lump = 0.05
#' @param thres_imp = 0.8
#' @param thres_nzv_freq =95/5
#' @param thres_nzv_unique = 10
#' @param vars_imp_ignore = NULL
#' @param vars_fct_expl_na = NULL
#' @param vars_ordinalscore  = NULL, 
#' @param one_hot = TRUE
#'
#'
#'


prepare_ml <- function(
  feature,
  outcome,
  outcome_name = NULL,
  level_order  = NULL,
  prep_recipe  = NULL,
  seed         = NULL,
  
  prep_step_normalize = TRUE,
  prep_step_knnimpute = TRUE,
  prep_step_log       = TRUE,
  prep_step_corr      = TRUE,
  prep_step_dummy     = TRUE,
  
  thres_log           = 2,
  thres_corr          = 0.9,
  thres_lump          = 0.05,
  thres_imp           = 0.8,
  thres_nzv_freq      = 95/5, 
  thres_nzv_unique    = 10,
  
  vars_imp_ignore     = NULL,
  vars_fct_expl_na    = NULL,
  vars_ordinalscore   = NULL,
  
  one_hot             = TRUE,
  
  outlier_remove      = FALSE,
  outlier_ctrl        = list(coef = 3)
  
){
  
  # OUTCOME ####
  
  # ... outcome_name ####
  # guess outcome column, if outcome_name = NULL and outcome has more than 2 columns: use first that's not '.id'
  
  if(is.null(outcome_name)){
    
    outcome_options <- setdiff(colnames(outcome), '.id')
    needs_guessing  <- length(outcome_options) > 1
    outcome_name    <- outcome_options[1]
    
    if(needs_guessing){
      usethis::ui_info( paste0(
        crayon::silver('The outcome object you provided has multiple options. The following option was chosen: \n'), # MARTINI chose: \n'), 
        '\t' , crayon::magenta( outcome_name)     , '\n'
      ))
    } 
    
  } else { # outcome_name is provided
    
    # do columns exist?  
    walk( outcome_name, ~  if(! .x %in% colnames(outcome) ){ 
      usethis::ui_stop( paste0(
        'The column ', .x, ' is not present in the outcome data set. ', 
        'Please correct input of column_name or let the function choose from existing columns (regression and classification only).\n'))
    } )
    
    # check number of provided outcome columns
    if(length(outcome_name) > 2 ){ 
      usethis::ui_stop('Please check input for outcome_name. No more than two columns might be selected.')
    }else if( length(outcome_name) == 2 ){
      # check column names and types for survival  
      
      names_valid  <- {sort(names(outcome_name)) == c('.status', '.time')} %>%  all()
      if(!names_valid)  usethis::ui_stop('For survival analysis, please provide vector with names .status and .time for outcome_name.')
      
      status_valid <- outcome[, outcome_name['.status']] %>% pull() %>%  { . %in% c(0,1) } %>%  all() 
      if(!status_valid) usethis::ui_stop('status may only contain values 0 and 1.')
      # stops if NAs are present
      
      time_valid   <- outcome[, outcome_name['.time'  ]] %>% pull() %>%  is.numeric()
      if(!time_valid)   usethis::ui_stop('Please check type of time column.')
      
      # sort by name
      outcome_name <- outcome_name[ c('.time', '.status')]
    }  
  } # -> outcome_name is set, either of length one or two
  
  
  # ... outcome_mode ####
  if(length(outcome_name) == 2){
    outcome_mode <- 'survival'
  }else{ 
    outcome_mode <- ifelse(
      is.numeric(outcome[[outcome_name]])
      && n_distinct(outcome[[outcome_name]]) > 5,
      "regression", 
      "classification"
    )
  }  
  
  # for consistency, add name if mode != survival
  if(outcome_mode != 'survival'){
    names(outcome_name) <- '.out'
  }
  
  # ... outcome_label ####
  # extract label(s) of outcome before potentially mutating to factor (classification)
  # for consistency, outcome label is a named vector.
  outcome_label <- outcome_name 
  iwalk(outcome_name, ~ {
    the_label <- labelled::var_label(outcome)[.x] %>% unlist()
    outcome_label[.y] <<- the_label
  })
  
  
  # ... outcome dict ####
  outcome_dict <- tibble::tibble(
    param  = names(outcome_name)) %>% 
    mutate(
      column = param,
      source = "user_outcome",
      label  = outcome_label[param]
    )
  
  
  # ... outcome data ####
  
  # ... ... standardize outcome name ####
  outcome <- outcome %>% 
    dplyr::select(all_of('.id'), tidyselect::all_of(outcome_name))
  
  # ... ... classification -> factor(), fct_relevel() ####
  if (outcome_mode == "classification"){
    
    outcome <- outcome %>% dplyr::mutate_at(".out", factor) # strips labels
    outcome_level <- outcome[[".out"]] %>% levels()
    
    if (!is.null(level_order)){
      level_order <- intersect(level_order, outcome_level)
      if (length(level_order) > 0){
        outcome <- outcome %>% 
          mutate_at(".out", ~ fct_relevel(., level_order))
      }
    }
    
  }
  
  # ... ... regression -> outlier_removal
  id_outlier <- NULL
  if(outcome_mode == "regression" && outlier_remove){
    
    # with c = outlier_ctrl$coef, exclude observations outside [q25 - c*iqr;  q75 + c*iqr]
    q   <- quantile(outcome$.out, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
    loq <- q + c(-1,1) * abs(outlier_ctrl$coef[1]) * diff(q)
    is_outlier <- !between(outcome$.out, loq[1], loq[2])
    
    outcome    <- outcome %>% dplyr::filter( ! is_outlier) # !is.na(.out) NAs will be removed and tracked in the recipe
    
    if (any(is_outlier)){
      usethis::ui_info(paste0(
        "Based on the outcome distribution, ", sum(is_outlier),
        ifelse(sum(is_outlier)>1, " observations were "," observation was "),
        "identified as outlier and removed from the input data prior to data splitting and preprocessing.\n"
      ))
    }
    
    id_outlier <- outcome$.id[is_outlier]
    
  }
  
  
  # FEATURE ####
  
  # RENAMING VECTOR ####
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
  
  
  
  if (!is.null(vars_fct_expl_na)){
    vars_fct_expl_na <- feature %>% 
      select_if(is.factor) %>% 
      colnames() %>% 
      intersect(vars_fct_expl_na)
    # catch special case 'no factors in feature'
    if (length(vars_fct_expl_na) == 0) vars_fct_expl_na <- NULL
  }
  
  # transform all character columns into factors (strips labels)
  feature <- feature %>% 
    dplyr::mutate_if(is.character, factor) %>% 
    dplyr::mutate_if(is.factor, ~ forcats::fct_relabel(., ~stringr::str_replace_all(., renaming) )) %>% 
    # add explicit NAs to selected factor variables (optional)
    {if(!is.null(vars_fct_expl_na)){
      dplyr::mutate_at(., vars_fct_expl_na, ~forcats::fct_explicit_na(.x, na_level = "missing"))
    }else{.}
    }
  
  # identify columns with 'Other' level
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
  
  # MERGE  ####
  
  d_raw <- outcome %>%
    dplyr::inner_join(feature, by = ".id")
  
  # DATA SPLIT ####
  
  if(!is.null(seed))  set.seed(seed)
  
  strata <- NULL
  if(outcome_mode == "classification") strata <- '.out'
  if(outcome_mode == "survival")       strata <- '.status'
  
  d_split <- d_raw %>% rsample::initial_split(strata = tidyselect::all_of(strata))
  
  d_train_raw <- training(d_split)
  d_valid_raw <- testing( d_split)
  
  
  #  PREPROCESSING PREP    ####
  
  # derive variable lists for steps ####
  # ...identify skewed parameters -> logtrafo later in recipe  ####
  vars_logtr <- d_train_raw %>% 
    dplyr::select_if(is.numeric) %>% 
    tidyr::pivot_longer(-any_of(c(".id", ".out", ".status", ".time")), 
                        names_to = "PARAMCD", values_to = "AVAL") %>% 
    dplyr::group_by(PARAMCD) %>% 
    dplyr::mutate(MINAVAL = min(AVAL)) %>% 
    dplyr::filter(MINAVAL > 0) %>% 
    dplyr::summarise(skew = e1071::skewness(AVAL, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::filter(skew > thres_log ) %>% 
    dplyr::pull(PARAMCD)
  
  # ...calculate proportion of missing values per column ####
  prop_available <- d_train_raw %>% 
    purrr::map_dbl(~ mean(!is.na(.))) %>% 
    tibble::enframe()
  
  # ...factors to skip from step_other ####
  # if a single class falls below the threshold thres_lump, the class would be renamed to 'other'
  vars_nolump <- d_train_raw %>% 
    dplyr::select_if(is.factor) %>% 
    map_lgl( ~ { freqs <- table(.x)/ length(.x); sum(freqs < thres_lump) == 1  } )  %>% 
    which(.) %>% 
    names()
  
  # var %in% 'vars_imp_ignore' : rows with missing values are dropped
  # else if thres_imp is not met vars are dropped
  
  vars_imp <- prop_available %>% 
    dplyr::filter(value >= thres_imp) %>% 
    dplyr::pull(name) %>% 
    setdiff(vars_imp_ignore) %>% 
    setdiff((c(".out", ".id", ".status", ".time")))
  
  vars_exclude <- prop_available %>% 
    dplyr::filter(value < thres_imp) %>% 
    dplyr::pull(name) %>% 
    setdiff((c(".out", ".id", ".status", ".time")))
    
    
  
  # RECIPE ####
  
  if (is.null(prep_recipe)){
    
    # ... formula
    if(outcome_mode %in% c('regression', 'classification')){
      the_formula <- as.formula(".out ~ .")
    }else{
      #the_formula <- as.formula("Surv(time = .time, event = .status, type = 'right') ~ .")
      the_formula <-  as.formula('.time + .status ~ .') # best guess...
    }  
   
     
    # Note that order is important when building the recipe,
    # e.g. exclude variable before imputation, nzv and log before normalize 
    rcp <- recipes::recipe(the_formula, data = d_train_raw) %>% 
      recipes::step_rm(tidyselect::any_of(vars_exclude)) %>% 
      recipes::update_role(.id, new_role = "ID") %>% 
      
      # ...omit observations with missing endpoint ####
      recipes::step_naomit(recipes::all_outcomes()) %>% 
      
      # ...imputation ####
      {if(prep_step_knnimpute){
        recipes::step_knnimpute(., tidyselect::any_of(vars_imp)) }else{.}
      } %>% 
      
      # ...omit observations with missing data in variables ignored in imputation ####
      recipes::step_naomit(recipes::all_predictors()) %>% 
      
      # ...near zero variance ####
      recipes::step_nzv(recipes::all_predictors(),
                        freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique
      ) %>% 
      
      # ...log transformation ####
      {if(prep_step_log && length(vars_logtr)>0){
        recipes::step_log(., tidyselect::any_of(vars_logtr)) 
      }else{.}
      }  %>%
      
      # ...normalization ####
      {if(prep_step_normalize){
        recipes::step_normalize(., recipes::all_numeric(), -recipes::all_outcomes(), - recipes::has_role("ID")) }else{.}
      }  %>% 
      
      # ...remove highly correlated variables ####
      {if(prep_step_corr){
        recipes::step_corr(., recipes::all_numeric(), -recipes::all_outcomes(), 
                           threshold = thres_corr, method = "pearson",
                           use = "pairwise.complete.obs")
      }else{.}} %>%  
      
      # ...lump factors ####
      recipes::step_other(., 
                          recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"),
                          -any_of(vars_nolump),
                          threshold = thres_lump, other = level_other) %>%  
      
      # ...factor handling ####
      {if(! is.null(vars_ordinalscore)){
        recipes::step_ordinalscore(.,  tidyselect::any_of(!! vars_ordinalscore ) )}else{.}
      } %>%  
      
      #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
      {if(prep_step_dummy){
        recipes::step_dummy(.,  recipes::all_nominal(), - recipes::all_outcomes(), - recipes::has_role("ID")  , 
                            one_hot = one_hot) }else{.} 
      }
    
    
  } else {
    rcp <- prep_recipe
  }
  
  rcp_prep <- rcp %>% 
    {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE)} %>% 
    pluck("result")
  
  d_train <- rcp_prep %>%  recipes::juice()
  
  d_valid <- rcp_prep %>% 
    {purrr::quietly(recipes::bake)(., d_valid_raw)} %>% 
    pluck("result")
  
  # CLEAN UP ####
  
  for (i in 1:ncol(d_train)){
    attr(d_train[[i]], "format.sas") <- NULL
    attr(d_valid[[i]], "format.sas") <- NULL
    attr(d_train[[i]], "label"     ) <- NULL
    attr(d_valid[[i]], "label"     ) <- NULL
  }
  attr(d_train, "label") <- NULL
  attr(d_valid, "label") <- NULL
  
  
  # excluded rows and columns ####
  
  ## ...rows #### 
  # na_outcome <- d_train_raw %>% 
  #   dplyr::filter(
  #     !{d_train_raw %>% 
  #         dplyr::select(tidyselect::all_of(outcome_name)) %>% 
  #         stats::complete.cases()}
  #   ) %>% 
  #   dplyr::pull(.id)
  
  na_outcome <- d_train_raw %>% 
    dplyr::select(tidyselect::any_of(c(".id", ".out", ".status", ".time"))) %>% 
    mutate_all(is.na) %>%  
    dplyr::rowwise() %>% 
    dplyr::mutate(ANYNA  = dplyr::c_across() %>% any()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(ANYNA) %>% 
    dplyr::pull(.id)

  attributes(na_outcome) <- NULL
  if (length(na_outcome) == 0) na_outcome <- NULL
  
  # na_feature <- d_train_raw %>% 
  #   dplyr::select(.id) %>% 
  #   {if (!is.null(na_outcome)){
  #     dplyr::filter(., !.id %in% na_outcome)
  #   }else{.}} %>% 
  #   anti_join(d_train %>% dplyr::select(.id), by = ".id") %>% 
  #   dplyr::pull(.id)
  
  na_feature <- d_train_raw$.id %>% 
    setdiff(na_outcome) %>% 
    setdiff(d_train$.id)
  
  # attributes(na_feature) <- NULL
  if (length(na_feature) == 0) na_feature <- NULL

  ## ...columns ####
  
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
  
  # document preparation parameter setting ####
  # NOTE TEMP text slots will be removed once documentation is fully available
  # TODO  documentation of pre-processing parameters    
  prep_params <- list(
    
    # log trafo
    thres_log  = list(
      value = ifelse(prep_step_log,  thres_log, NA),
      text  = ifelse(prep_step_log,
                     paste0('Variables were logtransformed if e1071::skewness() >',  thres_log,'.'),
                     'No variables were logtransformed.')
    ),
    
    # correlated variables
    thres_corr  = list(
      value = ifelse(prep_step_corr, thres_corr, NA),
      text  = ifelse(prep_step_corr,
                     paste0('The applied cutoff for removal of variables due to high correlations was ',  thres_corr,'.'),
                     'No variables were removed for reasons of high correlation.')
    ),  
    
    # lump factor levels (always applied)
    thres_lump = list(
      value = thres_lump,
      text  = paste0('Low frequency factor levels were lumped using recipes::step_other(threshold = ', thres_lump, '). ')  
    ),
    
    # imputation/missing values
    ## imputation/dropping of variables based on available probability
    imp_ignore = list(
      value = ifelse(prep_step_knnimpute, thres_imp, NA),
      text  = ifelse(prep_step_knnimpute,
                     paste0('Variables were dropped if the proportion of available data was less than ', 
                            thres_imp*100, '%.')  ,
                     'No imputation was done on the feature matrix.')
    ),

    # nzv 
    nzv = list(
      value = list(freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique),
      text  = paste0('Highly sparse and unbalanced variables were dropped using ',  
                     'recipes::step_nzv(freq_cut = ', round(thres_nzv_freq) ,
                     ', unique_cut = ', thres_nzv_unique, ').'
      )
    )
    
  )    
  
  if(outcome_mode == 'regression' ){
    prep_params <- append(prep_params, 
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
      test  = d_valid_raw
    ),
    data_prep = list(
      train = d_train,
      test  = d_valid
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
      ),
    
    prep_params = prep_params,
    
    removed = list(
      rows = list(
        outlier_outcome = id_outlier,
        na_outcome      = na_outcome,
        na_feature      = na_feature
      ),
      cols = removed_columns
    )
    
  )
  
  #saveRDS(prep_output, file = paste0("data/prep_output_",outcome_mode,".rds"))
  
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
