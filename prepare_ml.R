#'
#'
#' @param feature feature matrix in wide format, e.g. output object of \code{build()}, i.e. containing \code{.id} column and predictors 
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name ,
#' @param level_order  = NULL (only used for classification)
#' @param prep_recipe  = NULL,
#' @param seed         = NULL,
#' @param prep_step_normalize = TRUE,
#' @param prep_step_knnimpute = TRUE,
#' @param prep_step_log       = TRUE,
#' @param prep_step_corr      = TRUE,
#' @param prep_step_dummy FALSE converted  variables to be 
#' @param thres_log           = 2,
#' @param thres_corr           = .9,
#' @param thres_lump
#' @param thres_imp
#' @param thres_nzv_freq
#' @param thres_nzv_unique
#' @param vars_imp_ignore
#' @param vars_fct_expl_na
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
    
    # guess outcome column, if outcome has more than 2 columns use first that's not '.id'
  
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
        
    } else { # outcome name is provided
      if(! outcome_name %in% colnames(outcome) ){ 
         usethis::ui_stop( paste0('The column ', outcome_name, 
                                  ' is not present in the outcome data set. Please correct input of column_name or let the function choose from existing columns.\n'))
      } 
    } 
   
    # extract label of outcome before potentially mutating to factor (classification)
    outcome_label <- labelled::var_label(outcome)[[outcome_name]]
    if(is.null(outcome_label)) outcome_label <- outcome_name
  
    outcome_mode <- ifelse(is.numeric(outcome[, outcome_name, drop = TRUE]), "regression", "classification")
    
    outcome <- outcome %>% 
      dplyr::select(all_of('.id'), .out = tidyselect::all_of(outcome_name))

    if (outcome_mode == "classification"){
      
      outcome <- outcome %>% dplyr::mutate_at(".out", factor) # strips labels
      outcome_level <- outcome[[".out"]] %>% levels()
      
      if (!is.null(level_order)){
        level_order <- intersect(level_order, outcome_level)
        if (length(level_order) > 0){
          outcome <- outcome %>% 
            mutate_at(".out", ~fct_relevel(., level_order))
        }
      }
      
    }

    if(outcome_mode == "regression" && outlier_remove){
      
      # with c=outlier_ctrl$coef, exclude observations outside [q25 - c*iqr;  q75 + c*iqr]
      q   <- quantile(outcome$.out, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
      loq <- q + c(-1,1) * abs(outlier_ctrl$coef[1]) * diff(q)
      is_outlier <- !between(.out, loq[1], loq[2])
      
      outcome    <- outcome %>% dplyr::filter(is.na(.out) | !!is_outlier)
      
      if (any(is_outlier)){
        usethis::ui_info(paste0(
          "Based on the outcome distribution, ", sum(is_outlier),
          ifelse(sum(is_outlier)>1, " observations were "," observation was "),
          "identified as outlier and removed from the input data prior to data splitting and preprocessing.\n"
        ))
      }
      
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
    
    d_split <- d_raw %>% rsample::initial_split(strata = tidyselect::all_of(strata))
    
    d_train_raw <- training(d_split)
    d_valid_raw <- testing( d_split)
    
    
    #  PREPROCESSING PREP    ####
    
    # derive variable lists for steps ####
    # ...identify skewed parameters -> logtrafo   ####
    vars_logtr <- d_train_raw %>% 
      dplyr::select_if(is.numeric) %>% 
      tidyr::pivot_longer(-any_of(c(".id", ".out")), 
                          names_to = "PARAMCD", values_to = "AVAL") %>% 
      dplyr::group_by(PARAMCD) %>% 
      dplyr::mutate(MINAVAL = min(AVAL)) %>% 
      dplyr::filter(MINAVAL > 0) %>% 
      dplyr::summarise(skew = e1071::skewness(AVAL, na.rm = TRUE), .groups = "drop") %>% 
      dplyr::filter(skew > thres_log ) %>% 
      dplyr::pull(PARAMCD)
    
    # ...calculate proportion of missing values per column ####
    prop_available <- d_train_raw %>% 
      purrr::map_dbl(~mean(!is.na(.))) %>% 
      tibble::enframe()
    
    # ...factors to skip from step_other ####
    # if a single class falls below the threshold thres_lump, the class would be renamed to 'other'
    vars_nolump <- d_train_raw %>% 
      dplyr::select_if(is.factor) %>% 
      map_lgl( ~ { freqs <- table(.x)/ length(.x); sum(freqs < thres_lump) == 1  } )  %>% 
      which(.) %>% 
      names()
      
    # variables to impute: predictors with sufficient information, i.e. meeting thres_imp
    # variables are dropped if they a) don't meet the threshold OR b) shall not be imputed explicitly (vars_imp_ignore) 
    vars_imp <- prop_available %>% 
      dplyr::filter(value >= thres_imp) %>% 
      dplyr::pull(name) %>% 
      setdiff(vars_imp_ignore) %>% 
      setdiff((c(".out", ".id")))
    
    vars_exclude <- c(
      
      d_train_raw %>% 
        dplyr::select_if(~any(is.na(.))) %>% 
        colnames() %>% 
        intersect(vars_imp_ignore),
      
      prop_available %>% 
        dplyr::filter(value < thres_imp) %>% 
        dplyr::pull(name)
      
    ) %>% unique()
    
    # RECIPE ####
    
    if (is.null(prep_recipe)){
      # Note that order is important when building the recipe, e.g. nzv and log before normalize 
      rcp <- as.formula(".out ~ .") %>%  
        recipes::recipe(data = d_train_raw  ) %>% 
        recipes::step_rm(tidyselect::any_of(vars_exclude)) %>% 
        recipes::update_role(.id, new_role = "ID") %>% 
        
        # ...omit observations with missing endpoint ####
        recipes::step_naomit(recipes::all_outcomes()) %>% 
        
        # ...imputation ####
        {if(prep_step_knnimpute){
          recipes::step_knnimpute(., tidyselect::any_of(vars_imp)) }else{.}
        } %>% 

        # ...near zero variance ####
        recipes::step_nzv(recipes::all_predictors(),
                          freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique
                          ) %>% 
        
        # ...log transformation ####
        {if(prep_step_log && length(vars_logtr)>0){
          recipes::step_log(., tidyselect::any_of(vars_logtr)) }else{.}
        }  %>%
        
        # normalization
        {if(prep_step_normalize){
          recipes::step_normalize(., recipes::all_numeric(), -recipes::all_outcomes(), - recipes::has_role("ID")) }else{.}
        }  %>% 
        
        # remove highly correlated variables
        {if(prep_step_corr){
            recipes::step_corr(., recipes::all_numeric(), -recipes::all_outcomes(), 
                               threshold = thres_corr, method = "pearson",
                               use = "pairwise.complete.obs")
        }else{.}} %>%  
            
        # lump factors
        recipes::step_other(., 
             recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"),
             -any_of(vars_nolump),
             threshold = thres_lump, other = level_other) %>%  
            
        # factor handling
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
      attr(d_train[[i]], "label") <- NULL
      attr(d_valid[[i]], "label") <- NULL
    }
    attr(d_train, "label") <- NULL
    attr(d_valid, "label") <- NULL
    
    
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
        
        # imputation/dropping of variables based on available probability
        imp_ignore = list(
          value = ifelse(prep_step_knnimpute, thres_imp,       NA),
          text  = ifelse(prep_step_knnimpute,
                         paste0('Variables were dropped if the proportion of available data was less than ', 
                               thres_imp*100, '% or if they were specified in vars_imp_ignore.')  ,
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
                                  outlier = list(
                                    value = ifelse(outlier_remove, unlist(outlier_ctrl), NA ),
                                    text  = paste0("Based on the outcome distribution, observations outside the interval ",
                                                   '[q25 - ', outlier_ctrl$coef, '*iqr; ',  
                         'q75 + ', outlier_ctrl$coef, '*iqr] were removed prior to data splitting and preprocessing.'))
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
        name = '.out', 
        mode = outcome_mode
      ),
      
      prep_recipe = rcp_prep,
      
      dict = attr(feature, "dict") %>% 
        dplyr::bind_rows(
          tibble::tibble(
            param  = outcome_name,
            column = ".out",
            source = "user_outcome"
          ) %>%
            mutate(label = outcome_label)
        ),
      
      prep_params = prep_params
      
    )
    
    #saveRDS(prep_output, file = paste0("data/prep_output_",outcome_mode,".rds"))
    
    prep_output
    
}



# dev
if(FALSE){
 # feature 
 #  outcome 
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
  
  vars_imp_ignore     = NULL 
  vars_fct_expl_na    = NULL 
  vars_ordinalscore   = NULL 
  
  one_hot             = TRUE 
  
  outlier_remove      = FALSE 
  outlier_ctrl        = list(coef = 3)
}
