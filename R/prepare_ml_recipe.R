

#' create recipe from data 
#'
#' @param data raw data set to create recipe for
#' @param prep_recipe if NULL, recipe will be created
#' @param corr_method,corr_use defaulting to corr_method "pearson" and corr_use "pairwise.complete.obs"
#' @param thres_list,step_list named list objects collecting all threshold values 
#' and step selection info, resp. please refer to the documentation of the `thres_*` and `prep_step_*` arguments 
#' in \code{\link{prepare_ml}()} for detailed documentation and list entry names.
#' @param vars_imp_ignore,vars_fct_expl_na,vars_ordinalscore,vars_keep_corr vars relevant 
#' to steps. see \code{\link{prepare_ml}()} for details.
#' @param level_other name for the "other"-category in `recipes::step_other()`. defaults to "other".
#' @param one_hot,log_base see \code{\link{prepare_ml}()}
#'
#' @return
#' a named list with entries containing
#' * the prepared recipe
#' * info on steps included in the recipe
#' * a list of relevant variables
#' * a list of thresholds used
#' 
#' @seealso \code{\link{prepare_ml}()}
#' 
#' @export
#'
#'
prepare_ml_recipe <- function(
  
  data, 
  
  prep_recipe = NULL,
  corr_method = "pearson",
  corr_use    = "pairwise.complete.obs",
  
  thres_list = NULL,
  step_list  = NULL,
  
  vars_imp_ignore     = c(".trt"),
  vars_fct_expl_na    = NULL,
  vars_ordinalscore   = NULL,
  vars_keep_corr      = NULL,
  
  level_other,
  one_hot,
  log_base
  
){
  
  # define thresholds to use in recipe steps ####
  
  # ... set defaults
  
  thres_default <- tibble::lst(
    
    # using recipe defaults where available
    thres_lump       = get_default(recipes::step_other, 'threshold'), 
    thres_nzv_freq   = get_default(recipes::step_nzv,   'freq_cut'),
    thres_nzv_unique = get_default(recipes::step_nzv,   'unique_cut'),
    thres_corr       = get_default(recipes::step_corr,  'threshold'),
    
    # no recipes equivalent
    thres_count = 10,   
    thres_log   = 2,    
    thres_imp   = 0.8  
    
  )
  
  # ... use recipes default thresholds if not provided otherwise
  if(!is.null(thres_list)){
    thres_used <- purrr::imap(thres_default, ~ {thres_list[[.y]] %||% .x})
  }else{
    thres_used <- thres_default
  }
  
  
  
  # select recipe steps to include ####
  step_default <- list(
    prep_step_normalize = TRUE,
    prep_step_knnimpute = TRUE,
    prep_step_log       = TRUE,
    prep_step_corr      = TRUE,
    prep_step_dummy     = FALSE
  )
  
  if(!is.null(step_list)){
    step_used <- purrr::imap(step_default, ~{step_list[[.y]] %||% .x})
  }else{
    step_used <- step_default
  }
 
  
  # variable lists for steps ####
  
  # ... passed from prepare_ml
  
  # ... derive 
  vars <- prepare_ml_vars(
    data        = data,
    thres_count = thres_used$thres_count,
    thres_log   = thres_used$thres_log,
    thres_lump  = thres_used$thres_lump
  )
  
  vars_count   <- vars$count   
  vars_log     <- vars$log   
  vars_nolump  <- vars$nolump
  
  # placeholder for recipe, will be filled later
  vars_rm_corr <- NULL
  
  # RECIPE ####
  
  if (is.null(prep_recipe)){
    
    # ... formula ####
    # TODO check, if different formula is needed for repeated measurements
    if(".out" %in% names(data)){
      the_formula <- as.formula(".out ~ .")
    }else{
      the_formula <- as.formula('.time + .status ~ .') # best guess...
    }  
    
    # ... write recipe ####
    # Note that order is important when building the recipe,
    # e.g. exclude variable before imputation, nzv and log before normalize 
    
    # TODO rewrite recipe using new step fcts:
    # recipes::step_impute_bag()
    # recipes::step_lincomb()
    # recipes::check_range() (did not work on the first try, can not handle NA?)
    
    rcp <- recipes::recipe(the_formula, data = data) %>% 
      
      recipes::update_role(tidyselect::any_of(c(".id", ".rmtime")), new_role = "ID") %>% 

      # ... ... make clean levels ####
      recipes::step_mutate_at(
        recipes::all_nominal_predictors(), 
        fn = ~ {prepare_replace(.x)$x %>% factor()}
      ) %>% 
      
      # ... ... add explicit NAs to selected factor variables (optional) ####
      {if(!is.null(vars_fct_expl_na)){
        recipes::step_mutate_at(., vars_fct_expl_na, ~ fct_na_to_level(.x, level = "missing"))
      }else{.}} %>% 
      
      # ... ... consistent handling of factors with level other ####
      recipes::step_mutate_at(
        recipes::all_factor_predictors(), 
        fn = ~ {prepare_ml_other(.x)}
      ) %>% 
      
      # ... ... remove variables that are correlated to 'vars_keep_corr' ####
      recipes::step_rm(tidyselect::any_of(vars_rm_corr)) %>% 
    
      # ... ... exclude variables with too many missings ####
      {if(thres_used$thres_imp>0){
        recipes::step_filter_missing(., recipes::all_predictors(), threshold = 1-thres_used$thres_imp)
      }else{.}} %>% 
      
      # ... ... omit observations with missing endpoint ####
      recipes::step_naomit(recipes::all_outcomes()) %>% 
      
      # ... ... imputation ####
      {if(step_used$prep_step_knnimpute){
        recipes::step_impute_knn(., recipes::all_predictors(), -tidyselect::any_of(vars_imp_ignore)) %>% 
          # simple imputation for values that could not be imputed by knn
          recipes::step_impute_median(recipes::all_numeric_predictors(), -tidyselect::any_of(vars_imp_ignore)) %>% 
          recipes::step_impute_mode(recipes::all_nominal_predictors(), -tidyselect::any_of(vars_imp_ignore))
      }else{.}} %>% 
      
      # ... ... omit observations with missing data in variables ####
      recipes::step_naomit(recipes::all_predictors()) %>% 
      
      # ... ... (near) zero variance ####
      recipes::step_zv(recipes::all_predictors()) %>% 
      recipes::step_nzv(
        recipes::all_predictors(),
        freq_cut   = thres_used$thres_nzv_freq, 
        unique_cut = thres_used$thres_nzv_unique
      ) %>% 
      
      # ... ... log transformation ####
      {if(step_used$prep_step_log && length(vars_log)>0){
        recipes::step_log(., tidyselect::any_of(vars_log), base = log_base) 
      }else{.}} %>%
      
      # ... ... normalization ####
      {if(step_used$prep_step_normalize){
        recipes::step_normalize(
          ., 
          recipes::all_numeric(), -recipes::all_outcomes(), -recipes::has_role("ID"),
          # exclude vars identified as counts (previously excluded from logtrafo as well)
          -tidyselect::any_of(vars_count),
        )
      }else{.}} %>% 
        
      # ... ... remove highly correlated variables ####
      {if(step_used$prep_step_corr){
        recipes::step_corr(
          ., 
          recipes::all_numeric(), -recipes::all_outcomes(), 
          threshold = thres_used$thres_corr, method = corr_method,
          use = corr_use
        )
      }else{.}} %>%  
        
      # ... ... lump factors ####
      recipes::step_other(
        ., 
        recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"), -tidyselect::any_of(vars_nolump),
        threshold = thres_used$thres_lump, other = level_other
      ) %>%  
        
      # ... ... factor handling ####
      {if(! is.null(vars_ordinalscore)){
        recipes::step_ordinalscore(.,  tidyselect::any_of(!! vars_ordinalscore ) )
      }else{.}} %>%  
        
      #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
      # ... ... dummy coding ####
      {if(step_used$prep_step_dummy){
        recipes::step_dummy(
          .,  
          recipes::all_nominal(), - recipes::all_outcomes(), - recipes::has_role("ID")  , 
          one_hot = one_hot
        ) 
      }else{.}} 
    
  } else {
    rcp <- prep_recipe
  }
  
  # ... prep recipe ####
  
  # modify recipe if corr step is applied and given var set should be kept
  if(step_used$prep_step_corr &&
     !is.null(vars_keep_corr)
  ){
    
    # identify corr step from recipe
    number_corr <- rcp %>% 
      recipes::tidy() %>% 
      dplyr::filter(type == 'corr') %>% 
      dplyr::pull(number)
    
    # prep and train
    rcp_nocorr <- rcp
    rcp_nocorr$steps[[number_corr]]$skip <- TRUE
    
    rcp_prep_nocorr <- rcp_nocorr %>% 
      {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE, training = data)} %>% 
      purrr::pluck("result")
    
    # identify naomit step from recipe
    number_naomit <- rcp_prep_nocorr %>% 
      recipes::tidy() %>% 
      dplyr::filter(type == 'naomit') %>% 
      dplyr::pull(number)
    
    # prep and train
    purrr::walk(number_naomit, ~{
      rcp_prep_nocorr$steps[[.x]]$columns <<- unname(rcp_prep_nocorr$steps[[.x]]$columns)
      rcp_prep_nocorr$steps[[.x]]$skip    <<- FALSE
    })
    
    d_train_nocorr <- rcp_prep_nocorr %>%
      recipes::bake(new_data = data)
    
    # for all variables that need to be kept, identify highly correlated variables from d_train_nocorr
    vars_rm_corr <- vars_keep_corr %>% 
      rlang::set_names() %>% 
      purrr::map(~{
        
        d_ref <- d_train_nocorr %>% 
          dplyr::select(tidyselect::all_of(.x))
        
        d_check <- d_train_nocorr %>% 
          dplyr::select_if(is.numeric) %>% 
          dplyr::select(-tidyselect::any_of(c(.x, ".id")))
        
        cor(d_check, d_ref, method = corr_method, use = corr_use) %>% 
          as.data.frame() %>% 
          tibble::rownames_to_column() %>% 
          dplyr::filter(abs(!!rlang::sym(.x)) > thres_used$thres_corr) %>% 
          dplyr::pull(rowname)
      }) %>% 
      unlist() %>% 
      as.character()
    
    # modify removal step in original recipe to add vars_rm_corr
    number_rm <- rcp %>% recipes::tidy() %>% dplyr::filter(type == 'rm') %>% dplyr::pull(number)
    env_rm    <- rcp$steps[[number_rm]]$terms[[1]] %>% attr(which = '.Environment') 
    
    assign('vars_rm_corr', vars_rm_corr, envir = env_rm) 
    
  }
  
  # prep recipe
  rcp_prep <- rcp %>% 
    {purrr::quietly(recipes::prep)(., 
      strings_as_factors = FALSE,
      training           = data
     #, log_changes        = TRUE,
     #  fresh              = TRUE
     # retain
    )} %>% 
    purrr::pluck("result")
  # TODO 
  # check new parameters 'retain' and 'log_changes' in 'prep()'
  

  # identify naomit step from recipe
  number_naomit <- rcp_prep %>% 
    recipes::tidy() %>% 
    dplyr::filter(type == 'naomit') %>% 
    dplyr::pull(number)
  
  # prep and train
  purrr::walk(number_naomit, ~{
    rcp_prep$steps[[.x]]$columns <<- unname(rcp_prep$steps[[.x]]$columns)
    rcp_prep$steps[[.x]]$skip    <<- FALSE
  })
  
  tibble::lst(
    rcp_prep,
    vars = tibble::lst(
      vars_count,
      vars_fct_expl_na,
      vars_imp_ignore,
      vars_keep_corr,
      vars_log,
      vars_nolump,
      vars_ordinalscore
    ),
    steps = step_used,
    thres = thres_used
  )
  
}