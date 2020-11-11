#'
#'
#' @param feature feature matrix in wide format, e.g. output object of \code{build()}, i.e. containing \code{.id} column and predictors 
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name ,
#' @param outcome_order = NULL (only used for classification)
#' @param prep_recipe  = NULL,
#' @param seed         = NULL,
#' @param prep_step_normalize = TRUE,
#' @param prep_step_knnimpute = TRUE,
#' @param prep_step_log       = TRUE,
#' @param prep_step_corr      = TRUE,
#' @param prep_step_dummy FALSE converted  variables to be 
#' @param thres_log           = 2,
#' @param thres_cor           = .9,
#' @param thres_lump
#' @param thres_imp
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
   prep_recipe  = NULL,
   seed         = NULL,
   
   prep_step_normalize = TRUE,
   prep_step_knnimpute = TRUE,
   prep_step_log       = TRUE,
   prep_step_corr      = TRUE,
   prep_step_dummy     = TRUE,
   
   thres_log           = 2,
   thres_cor           = 0.9,
   thres_lump          = 0.05,
   thres_imp           = 0.8,
   
   vars_imp_ignore     = NULL,
   vars_fct_expl_na    = NULL,
   vars_ordinalscore   = NULL,
   
   one_hot             = TRUE
   
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
          '\t' , crayon::magenta( outcome_name)     , '\n'   )
        )
      } 
      
   } else { # outcome name is provided
     if(! outcome_name %in% colnames(outcome) ){ 
       usethis::ui_stop( paste0('The column ', outcome_name, 
                                ' is not present in the outcome data set. Please correct input of column_name or let the function choose from existing columns.\n'))
     } 
   } 
  
    outcome_mode <- ifelse(is.numeric(outcome[, outcome_name, drop = TRUE]), "regression", "classification")
    
    
    
    
    # MERGE  ####
    
   # clean_char <- c('<' = 'l', '<=' = 'leq', '>'= 'g', '>=' = 'geq' )
    
    
    # ORDER MATTERS
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

    # First merge preds and (selected) outcome by .id -> d_raw
    if (outcome_mode == "classification"){
      level_order <- intersect(level_order, outcome[[outcome_name]])
      if (length(level_order) > 0){
        outcome <- outcome %>% 
          mutate_at(outcome_name, ~fct_relevel(., level_order))
      }
    }

    
    
    d_raw <- outcome %>%
      dplyr::select(all_of('.id'), .out = tidyselect::all_of(outcome_name)) %>% 
      dplyr::inner_join( feature %>%  
                          # stringr::str_replace_all(x, clean_char)
                              dplyr::mutate_if(is.factor,
                                               ~  forcats::fct_relabel(., ~str_replace_all(., renaming) )) 
                         , by = ".id") %>% 
                # stringr::str_replace_all(x, clean_char) %>%
                #    stringr::str_trim() %>%  
                #    stringr::str_to_lower() } )
                #  dplyr::mutate_if(is.character,  stringr::str_to_lower)  %>% 
                #dplyr::mutate_if(is.factor,  forcats::fct_relabel, stringr::str_to_lower)  %>% 
                #stringr::str_trim ) %>%  
                #dplyr::mutate_if(is.character, ~  str_replace_all(.x, clean_char)) %>% 
                
      {if(outcome_mode == "classification"){
        dplyr::mutate(., .out = factor(.out))
      }else{.}
      } %>% 
      # add explicit NAs to selected factor variables (optional)
      {if(!is.null(vars_fct_expl_na)){
        dplyr::mutate_at(., vars_fct_expl_na, ~forcats::fct_explicit_na(., na_level = "missing"))
      }else{.}
      }
       #%>% 
            # recode factors to numeric if should be used as ordinal
            #mutate_at( any_of(ordinals, 
            #         ~  as.numeric(factor(.x)) )
    

    
    if(!is.null(seed))  set.seed(seed)
    
    
    strata <- NULL
    if(outcome_mode == "classification") strata <- '.out'
    
    d_split <- d_raw  %>% 
        rsample::initial_split(strata = tidyselect::all_of(strata))
    
    d_train_raw <- training(d_split)
    d_valid_raw <- testing( d_split)
    
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
    
    d_train_raw <- d_train_raw %>% dplyr::select(-tidyselect::any_of(vars_exclude))
    d_valid_raw <- d_valid_raw %>% dplyr::select(-tidyselect::any_of(vars_exclude))
    
    # recipe ...####
    if (is.null(prep_recipe)){
      # Note that order is important when building the recipe, e.g. nzv and log before normalize, corr before 
      rcp <- as.formula(".out ~ .") %>%  
        recipes::recipe(data = d_train_raw  ) %>% 
        recipes::update_role(.id, new_role = "ID") %>% 
        
        # ...omit observations with missing endpoint ####
        recipes::step_naomit(recipes::all_outcomes()) %>% 
        
        # ...imputation ####
        {if(prep_step_knnimpute){
          recipes::step_knnimpute(., tidyselect::any_of(vars_imp)) }else{.}
        } %>% 

        # ...near zero variance ####
        recipes::step_nzv(recipes::all_predictors()) %>% 
        
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
          suppressMessages(
            recipes::step_corr(., recipes::all_numeric(), -recipes::all_outcomes(), 
                               threshold = thres_cor)
          )     }else{.}
        } %>%  
            
        # lump factors
        recipes::step_other(., recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"),
                   threshold = thres_lump) %>%  
            
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
      recipes::prep(strings_as_factors = FALSE)
    
    d_train <- rcp_prep %>%  recipes::juice()
    d_valid <- rcp_prep %>%  recipes::bake(d_valid_raw)
    
    for (i in 1:ncol(d_train)){
      attr(d_train[[i]], "format.sas") <- NULL
      attr(d_valid[[i]], "format.sas") <- NULL
      attr(d_train[[i]], "label") <- NULL
      attr(d_valid[[i]], "label") <- NULL
    }
    attr(d_train, "label") <- NULL
    attr(d_valid, "label") <- NULL
    
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
            mutate(label = labelled::var_label(outcome)[[outcome_name]])
        )
      
    )
    
    #saveRDS(prep_output, file = paste0("data/prep_output_",outcome_mode,".rds"))
    
    prep_output
    
}
