#'
#'
#'
#'
#'


prepare_ml <- function(
   feature,
   outcome,
   outcome_name = NULL,
   prep_recipe  = NULL,
   seed         = NULL
){
   # todo check if outcome name is valid  
  
  
    # guess outcome column, if outcome has more than 2 columns use first that's not '.id'
   if(is.null(outcome_name)){
    outcome_name <- setdiff(colnames(outcome), '.id')[1]
    # Note what was selected
    needs_guessing <- length(setdiff(colnames(outcome), '.id')) > 1
    if(needs_guessing){
      usethis::ui_info( paste0(
        crayon::silver('The outcome object you provided has multiple options. MARTINI chose: \n'), 
        '\t' , crayon::magenta( outcome_name)        )
      )
    }  
   }
    outcome_mode <- ifelse(is.numeric(outcome[, outcome_name, drop = TRUE]), "regression", "classification")
    
    # todo 
    # create variable sets (none, dummies, one_hots, ordinals)
    
    # First merge preds and (selected) outcome by .id -> d_raw
    d_raw <- outcome %>%
      select(all_of('.id'), .out = all_of(outcome_name)) %>% 
      inner_join( feature, by = ".id") %>% 
      {if(outcome_mode == "classification"){
        mutate(., .out = factor(.out))
      }else{.}
             } #%>% 
            # recode factors to numeric if should be used as ordinal
            #mutate_at( any_of(ordinals, 
            #         ~  as.numeric(factor(.x)) )
    
    if(!is.null(seed))  set.seed(seed)
    
    
    strata <- NULL
    if(outcome_mode == "classification")strata <- '.out'
    
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
      filter(skew > 2) %>% 
      pull(PARAMCD)
    
    if (is.null(prep_recipe)){
      rcp <- as.formula(".out ~ .") %>%  
        recipe(data = d_train_raw   # %>%
               # janitor::remove_constant(na.rm=TRUE)
               ) %>% 
        update_role(.id, new_role = "ID") %>% 
        step_naomit(all_outcomes()) %>% 
        step_zv(all_predictors())   %>% 
        step_knnimpute(all_predictors())   %>% 
        {if(length(prms_logtr)>0){
           step_log(., any_of(prms_logtr))}else{.}
        }  %>%
        step_normalize(all_numeric(), -all_outcomes(), -has_role("ID")) %>% 
        
        # to be implemented: encoding based on user selection (nones, dummies, one_hots, ordinals)
        # might be different for each categorical variable 
        #     -> create separate step_dummy for variable groups
        #  step_dummy(all_of(dummies), one_hot = FALSE) 
        #  step_dummy(all_of(one_hots), one_hot = TRUE) 
        #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
        step_dummy(all_nominal(), -all_outcomes(), -has_role("ID")  , one_hot = FALSE) 
             # There are new levels in a factor: NA    
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
