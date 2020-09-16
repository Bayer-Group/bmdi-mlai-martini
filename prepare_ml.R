#'
#'
#' @param feature feature matrix in wide format, e.g. output object of \code{build()}, i.e. containing \code{.id} column and predictors 
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name ,
#' @param prep_recipe  = NULL,
#' @param seed         = NULL,
#' @param prep_step_normalize = TRUE,
#' @param prep_step_knnimpute = TRUE,
#' @param prep_step_log       = TRUE,
#' @param prep_step_corr      = TRUE,
#' @param prep_step_dummy FALSE converted  variables to be 
#' @param thres_log           = 2,
#' @param thres_cor           = .9,
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
   thres_cor           = .9,
   
   # encoding control
   vars_ordinalscore  = NULL, 
   one_hot            = TRUE
   
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
    
    # First merge preds and (selected) outcome by .id -> d_raw
    d_raw <- outcome %>%
      select(all_of('.id'), .out = all_of(outcome_name)) %>% 
      inner_join( feature %>%  
                    mutate_if(is.factor, ~ fct_relabel(., make.names) ), by = ".id") %>% 
      {if(outcome_mode == "classification"){
        mutate(., .out = factor(.out))
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
        rsample::initial_split(strata = all_of(strata))
    
    d_train_raw <- training(d_split)
    d_valid_raw <- testing( d_split)
    
    # identify skewed parameters -> logtrafo
    prms_logtr <- d_train_raw %>% 
      select_if(is.numeric) %>% 
      pivot_longer(-any_of(c(".id", ".out")), names_to = "PARAMCD", values_to = "AVAL") %>% 
      group_by(PARAMCD) %>% 
      mutate(MINAVAL = min(AVAL)) %>% 
      filter(MINAVAL > 0) %>% 
      summarise(skew = e1071::skewness(AVAL, na.rm = TRUE), .groups = "drop") %>% 
      filter(skew > thres_log ) %>% 
      pull(PARAMCD)
    
    if (is.null(prep_recipe)){
      rcp <- as.formula(".out ~ .") %>%  
        recipe(data = d_train_raw  ) %>% 
        update_role(.id, new_role = "ID") %>% 
        step_naomit(all_outcomes()) %>% 
        step_zv(all_predictors())   %>% 
        {if(prep_step_knnimpute){
           step_knnimpute(., all_predictors()) }else{.}
        } %>% 
        
        {if(prep_step_log && length(prms_logtr)>0){
           step_log(., any_of(prms_logtr)) }else{.}
        }  %>%
        {if(prep_step_normalize){
          step_normalize(., all_numeric(), -all_outcomes(), -has_role("ID")) }else{.}
        }  %>% 
        
      
        {if(prep_step_corr){
          suppressMessages(
            step_corr(., all_numeric(), -all_outcomes(), threshold = thres_cor)
          ) 
        }else{.} %>%  
            
        # factor handling
        {if(! is.null(vars_ordinalscore)){
          step_ordinalscore(.,  any_of(!! vars_ordinalscore ) )}else{.}
        } %>%  
      
        #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
        {if(prep_step_dummy){
          step_dummy(., all_nominal(), -all_outcomes(), -has_role("ID")  , one_hot = one_hot) }else{.} 
        }
          
       }   
    } else {
      rcp <- prep_recipe
    }

    
    rcp_prep <- rcp %>% 
      prep(strings_as_factors = FALSE)
    
    d_train <- rcp_prep %>% juice()
    d_valid <- rcp_prep %>% bake( d_valid_raw)
    
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
      
      dict = attr(feature, "dict")
      
    )
    
    #saveRDS(prep_output, file = paste0("data/prep_output_",outcome_mode,".rds"))
    
    prep_output
    
}
