

#' create recipe from data 
#'
#' @param data raw data set to create recipe for
#' @param prep_recipe if NULL, recipe will be created
#' @param corr_method,corr_use defaulting to corr_method "pearson" and corr_use "pairwise.complete.obs"
#' @param thres_list,step_list named list objects collecting all threshold values 
#' and step selection info, resp. please refer to the documentation of the `thres_*` and `prep_step_*` arguments 
#' in \code{\link{prepare_ml}()} for detailed documentation and list entry names.
#' @inheritParams prepare_ml
#'
#' @return
#' a named list with entries containing
#' 
#' * the unprepared recipe  
#' * the prepared recipe  
#' * info on steps included in the recipe  
#' * a list of relevant variables  
#' * a list of thresholds used  
#' * \code{high_corr} a tibble listing correlations above \code{thres_corr}. \code{NULL} if \code{step_list$prep_step_corr = FALSE}.  

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
  vars_no_trafo       = NULL,
  
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
    thres_count = get_default(prepare_ml, 'thres_count'),   
    thres_log   = get_default(prepare_ml, 'thres_log'),    
    thres_imp   = get_default(prepare_ml, 'thres_imp')  
    
  )
  
  # ... use recipes default thresholds if not provided otherwise
  if(!is.null(thres_list)){
    thres_used <- purrr::imap(thres_default, ~ {thres_list[[.y]] %||% .x})
  }else{
    thres_used <- thres_default
  }
  
  
  
  # select recipe steps to include ####
  step_default <- args(prepare_ml) %>%
    as.list() %>% 
    head(-1) %>% 
    purrr::keep_at(., names(.) %>% stringr::str_subset('prep_step'))
  
  if(!is.null(step_list)){
    step_used <- purrr::imap(step_default, ~{step_list[[.y]] %||% .x})
  }else{
    step_used <- step_default
  }
 
  
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
    
    rcp <- recipes::recipe(
      formula = the_formula, 
      data = data,
      # `strings_as_factors` only affects variables with role 'outcome' and 
      # 'predictor'. 'ID' is not affected, even though it is not defined yet 
      # (but in the next step)
      strings_as_factors = TRUE
    ) %>% 
      
      recipes::update_role(tidyselect::any_of(c(".id", ".rmtime")), new_role = "ID") %>% 
      
      # ... ... make clean levels ####
      recipes::step_mutate_at(
        recipes::all_factor_predictors(), 
        fn = ~{prepare_replace(.x)$x}
        # fn = ~ forcats::fct_relabel(., ~ prepare_replace(.)$x)) %>% 
        #   ~ {prepare_replace(.x)$x}
      ) %>% 
      
      # ... ... add explicit NAs to selected factor variables (optional) ####
      {if (!is.null(vars_fct_expl_na)) {
        recipes::step_mutate_at(., vars_fct_expl_na, ~ fct_na_to_level(.x, level = "missing"))
      }else{.}} %>% 
        
      # ... ... exclude variables with too many missings ####
      {if (thres_used$thres_imp>0) {
        recipes::step_filter_missing(., recipes::all_predictors(), threshold = 1-thres_used$thres_imp)
      }else{.}} %>% 
      
      # ... ... omit observations with missing endpoint ####
      recipes::step_naomit(recipes::all_outcomes(), skip = FALSE) %>% 
      
      # ... ... omit observations with missing data in variables excluded from imputation ####
      recipes::step_naomit(tidyselect::any_of(vars_imp_ignore), skip = FALSE) %>%   
      
      # ... ... log transformation ####
      step_log_skewness(
        ., 
        recipes::all_numeric_predictors(), 
        -tidyselect::any_of(vars_no_trafo),
        base = log_base, 
        skewness = thres_used$thres_log
      ) %>% 
      
      # ... ... imputation ####
      {if (step_used$prep_step_knnimpute) {
        recipes::step_impute_knn(., recipes::all_predictors()) %>% 
          # simple imputation for values that could not be imputed by knn
          recipes::step_impute_median(recipes::all_numeric_predictors()) %>% 
          recipes::step_impute_mode(  recipes::all_nominal_predictors())
      } else {
        recipes::step_naomit(., recipes::all_predictors(), skip = FALSE)   
      }} %>% 
      
      # ... ... omit observations with missing data in case not prep_step_knnimpute = FALSE ####
      #recipes::step_naomit(recipes::all_predictors(), skip = FALSE) %>%   
      
      # ... ... undo log transformation ####
      {if (!step_used$prep_step_log) {
        step_log_skewness_undo(
          ., recipes::all_numeric_predictors()
        )
      }else{.}} %>%
      
      # ... ... (near) zero variance ####
      recipes::step_zv(recipes::all_predictors()) %>% 
      recipes::step_nzv(
        recipes::all_predictors(),
        freq_cut   = thres_used$thres_nzv_freq, 
        unique_cut = thres_used$thres_nzv_unique
      ) %>% 
      
      # ... ... normalization ####
      {if (step_used$prep_step_normalize) {
        recipes::step_normalize(
          ., 
          recipes::all_numeric_predictors(),
          -tidyselect::any_of(vars_no_trafo)
        )
      }else{.}} %>% 

      # ... ... remove highly correlated variables with a twist #### 
      {if (step_used$prep_step_corr) {
        step_corr_keep(
          ., 
          recipes::all_numeric_predictors(), 
          threshold = thres_used$thres_corr, 
          method = corr_method,
          use = corr_use, 
          keep = vars_keep_corr
        )
      }else{.}} %>%  
        
      # ... ... lump factors ####
      step_other2(
        ., 
        recipes::all_nominal_predictors(),
        threshold = thres_used$thres_lump
      ) %>%  
        
      # ... ... factor handling ####
      {if (!is.null(vars_ordinalscore)) {
        recipes::step_ordinalscore(.,  tidyselect::any_of(!! vars_ordinalscore ))
      }else{.}} %>%  
        
      #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
      # ... ... dummy coding ####
      {if(step_used$prep_step_dummy){
        recipes::step_dummy(
          .,  
          recipes::all_nominal_predictors(),  
          one_hot = one_hot
        ) 
      }else{.}} 
    
  } else {
    rcp <- prep_recipe
  }
  
  # ... prep recipe ####
  rcp_prep <- rcp %>% 
    {purrr::quietly(recipes::prep)(
      ., 
      retain = TRUE
      #, log_changes        = TRUE,
      #  fresh              = TRUE
      # retain
    )} %>% 
    purrr::pluck("result")
  # TODO 
  # check new parameters 'retain' and 'log_changes' in 'prep()'
  
  # ... extract corr tibble ####
  if (is.null(prep_recipe) && step_used$prep_step_corr) {

    number_step_corr_keep <- recipes::tidy(rcp_prep) %>% 
      dplyr::pull(type) %>% 
      magrittr::equals("corr_keep") %>% 
      which() 
      
    corr_tibble <- rcp_prep$steps[[number_step_corr_keep]]$high_corr
    
  } else {
    corr_tibble <- NULL
  }
  
  # ... extract log-transformed variables ####
  if (step_used$prep_step_log) {
    
    number_step_log <- recipes::tidy(rcp_prep) %>% 
      dplyr::pull(type) %>% 
      magrittr::equals("log_skewness") %>% 
      which()
    vars_log <- rcp_prep$steps[[number_step_log]]$columns
    
  } else {
    vars_log <- NULL
  }
  
  tibble::lst(
    rcp_raw = rcp,
    rcp_prep,
    vars = tibble::lst(
      vars_no_trafo,
      vars_fct_expl_na,
      vars_imp_ignore,
      vars_keep_corr,
      
      vars_log,
      vars_ordinalscore
    ),
    high_corr = corr_tibble,
    steps = step_used,
    thres = thres_used
  )
  
}
