#' Prepare ML ready data set from outcome and predictor data
#'
#' Given \code{feature}, a tibble representing a wide format feature matrix, and \code{outcome}, 
#' a tibble containing the outcome information (regression/classification/survival is supported),
#' \code{prepare_ml()} will provide data sets suitable for various machine learning problems along with additional information.
#' The data preparation steps include, but are not limited to data splitting, 
#' handling missing values, normalization, removal of redundant information (highly correlated features). 
#' Please refer to the Details section for more information.
#' 
#' 
#' @param feature feature matrix in wide format, e.g. output object of \code{\link{build}()}, 
#' i.e. containing \code{.id} column and predictors
#' @param outcome tibble containing \code{.id} column and the outcome of interest, \code{\link{prepare_ml_outcome}()}
#' @param outcome_name if NULL (default), the first column that's not `.id` is chosen for outcome_name
#' and the outcome_type is guessed to be either classification or regression.
#' One may also provide a single character giving the name of the outcome column OR 
#' a vector of length two giving the column names for the 'time' and 'status' data in survival analysis,
#' where `.time` is numeric and `.status` is binary with 0 coding for censored, and 1 coding for event.
#' Currently, only right-censoring is supported. Please note, that survival will never be guessed.
#' @param level_order level order for a classification outcome. Default \code{NULL} keeps the natural order (only used for classification).
#' @param prep_recipe a custom, pre-defined \code{recipes::recipe()} may be provided for data preparation. Defaults to NULL, yielding a data-driven preparation. 
#' please refer to the details section to learn about the individual recipe steps.
#' @param train_prop the proportion of data to be used for the training set. Has to be in \[0.5;1.0\]. Defaults to 3/4, keeping a quarter of the data for testing.
#' @param strata_trt boolean. Expand default stratum variable (\code{.out} for classification, \code{.stratum} for tte, \code{NULL} for regression) by trt (if character, else ignored). Defaults to FALSE.
#' @param seed optionally set a seed before the data splitting. 
#' @param prep_step_knnimpute,prep_step_log,prep_step_normalize,prep_step_corr,prep_step_dummy logicals determining 
#' whether or not the corresponding step function should be included in the recipe, 
#' possibly specified further using additional parameters (`thres_*`, `log_base`, `one_hot`)
#' Please refer to the details section for the full list of recipe steps.
#' @param thres_imp Minimal proportion of non-missing data per feature required to be kept 
#' in the data and completed using \code{recipes::step_impute_knn()}. 
#' Variables not meeting the threshold will be dropped and not be included in \code{data_prep} data. 
#' Per default \code{thres_imp = 0.8}, i.e. variables will be dropped if the proportion of available data is less than 80%. 
#' Variables listed in \code{vars_imp_ignore} will never be imputed, observations with missing data in the respective
#' variables will be removed.
#' @param thres_log variables will be log-transformed (with base \code{log_base}) if \code{prep_step_log = TRUE},
#' all observations are positive, and \code{e1071::skewness() > thres_log}, where `thres_log` defaults to 2.   
#' @param thres_corr if \code{prep_step_corr = TRUE}, \code{thres_corr} is passed to \code{recipes::step_corr()}'s 
#' \code{threshold} argument with a default of 0.9 to remove highly correlated features
#' @param thres_nzv_freq,thres_nzv_unique parameters passed to \code{recipes::step_nzv()} with defaults 
#' \code{thres_nzv_freq = 95/5)} and \code{thres_nzv_unique = 10} 
#' @param thres_count integer variables with no more than `thres_count` distinct values are considered as count variables and
#' are excluded from the log-transformation and normalization. Defaults to 10.
#' @param thres_lump this parameter is used to prevent renaming of a single low frequency class to 'other' by \code{recipes::step_other()}, to
#' which `thres_lump` is passed as parameter `threshold`. Defaults to 0.05.
#' @param one_hot boolean. passed to \code{recipes::step_dummy()} to choose one hot encoding over dummy encoding
#' @param vars_imp_ignore variables that shall not be imputed can be specified in \code{vars_imp_ignore}
#' (vector of column names, e.g. \code{vars_imp_ignore = '.trt'}). 
#' Observations with missing values in these variables will be removed. Removal is documented in `removed$rows`.
#' @param vars_fct_expl_na column names of factors for which NAs should be treated as an explicit factor level. Defaults to NULL.
#' @param vars_keep_corr choose these variables over other options when removing variables due to high correlation in \code{recipes::step_corr()}
#' @param vars_ordinalscore  column names of ordinal factor variables to be converted into numeric scores. Defaults to NULL.
#' @param log_base base to use for log-transformation in \code{recipes::step_log()}. Defaults to _exp(1)_.
#' @param outlier_remove,outlier_ctrl For outcome mode regression only, see \code{\link{prepare_ml_outcome}()}
#'  for details on how outliers are removed from outcome variables. `outlier_remove` defaults to FALSE, `outlier_ctrl` to `list(coef = 3)`.
#' @param quiet boolean. Suppress messages during outcome preparation to the console on NA and outlier removal, resp. Defaults to `FALSE`.
#'
#' @details 
#'
#' The following order of recipe steps for data preparation will be applied (if no recipe is provided).
#' The variable sets that a particular step function will be applied to are determined based on user input 
#' and output of the function \code{\link{prepare_ml_vars}()}, respectively.
#' 
#' * drop variables e.g. not meeting the minimum threshold for non-missing data proportion (`step_rm()`)
#' * remove observations with missing data in outcome (`step_naomit()`)
#' * knn imputation on variables with missing values that are not explicitly excluded from imputation  (`step_impute_knn()`)
#' * omit observations with remaining missing values (i.e. in variables that were excluded from imputation and not dropped before) (`step_naomit()`)
#' * removal of near-zero variance variables (`step_nzv()`)
#' * log-transformation (`step_log()`)
#' * normalization (`step_normalize()`)
#' * removal of highly correlated variables (`step_corr()`)
#' * lumping of low frequency factor levels into a single class (`step_other()`)
#' * transform ordinal factors into numeric variables (`step_ordinalscore()`)
#' * dummy/one hot encoding (`step_dummy()`) 
#' 
#'
#' @return 
#' 
#' ## Data sets
#' 
#' \code{prepare_ml()} produces a list that contains the data set both with (\code{data_prep}) and 
#' without (\code{data_raw}) applying the specified ML preparation steps. 
#' Both versions are splitted in \code{train} and \code{test} set.
#' In addition, \code{split} contains the combined \code{rsample::initial_split()} object that 
#' the \code{train} and \code{test} data was extracted from. Depending on the programming workflow, 
#' one might be more convenient to use than the other.
#' Both \code{data_test} slots as well as \code{split} are NULL
#' if \code{train_prop} was set to 1 (i.e. no splitting was done) and \code{train} contains the full ML data set.
#'  
#' 
#' The slot \code{outcome} contains a list giving \code{name}, the standardized names of the 
#' output column in the data sets ( \code{.out} for regression/classification, \code{.time} and \code{.status}
#' for survival, as well as a \code{mode}, character string of the outcome mode \code{regression/classification/survival} 
#' 
#' The dictionary available as an attribute of `feature` is updated with information on the outcome variable
#' and the log-transformation and available from the \code{dict} slot, NULL if no such attribute is defined.
#' 
#' The \code{source} slot simply passes the \code{source} attribute of \code{feature}, NULL if no such attribute is defined.
#' If \code{\link{build}()} from the \code{MLAIprepare} package was used to generate \code{feature}, 
#' this attribute lists the full paths of the files that were used in data generation of \code{feature}. 
#' 
#' ## Data preparation and documentation
#' 
#' \code{prep_recipe} contains the prepared recipe object, 
#' \code{prep_params} documents the parameters/thresholds used in the data preparation, 
#' giving bare \code{value} slots, as well as a verbose description in \code{text}.
#' \code{removed} gives a list of removed \code{rows} and \code{columns} along with the information
#'  on why/in which recipe step the data was removed.
#' 
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @export 


prepare_ml <- function(
  feature,
  outcome,
  outcome_name = NULL,
  level_order  = NULL,
  prep_recipe  = NULL,
  train_prop   = 3/4,
  strata_trt   = FALSE,
  seed         = 1130,
  
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
  vars_keep_corr      = NULL,
  
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
  renaming <- c(
    '<= |<=' = 'less_than_',  
    '> '  = 'over_',
    '< '  = 'under_',
    ' - ' = '_to_',  
    '>= |>=' = 'at_least_', 
    '<'   = 'under_' ,
    '>'   = 'over_',
    ' years|years' = '_y',
    '%'   = 'pct',
    '[[:punct:]]|[[:space:]]' = '_',
    '_+'  = '_',
    '_$' = ''
  )
  
  
  # ... intersect 'vars_fct_expl_na' with factor columns ####
  if (!is.null(vars_fct_expl_na)){
    vars_fct_expl_na <- feature %>% 
      dplyr::select_if(is.factor) %>% 
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
  
  train_prop_valid <- c(0.5, 1)
  if (!dplyr::between(train_prop, train_prop_valid[1], train_prop_valid[2])){
    usethis::ui_stop(paste0(
      "The provided training proportion 'train_prop' is outside [",
      train_prop_valid[1], ", ", train_prop_valid[2], "]. Please check!"
    ))
  } 
  
  if (train_prop < 1){
    if(!is.null(seed))  set.seed(seed)
    
    # create a new column .strata for stratified splitting by outcome
    d_raw <- d_raw %>% 
      {if(outcome_mode == "classification"){
         mutate(., .strata = .out)
      }else{.}
      } %>% 
      
      {if(outcome_mode == "survival"){
        mutate(., .strata = .status)
      }else{.}
      } %>% 
      
      # no outcome stratification for regression, but create the column
      # anyways to make it extendable by strata_trt = TRUE
      {if(outcome_mode == "regression"){
         mutate(., .strata = '')
      }else{.}
      }
    
    # extend strata variable by treatment
    if(strata_trt){
      if(! '.trt' %in% colnames(d_raw)){
        usethis::ui_info(crayon::silver(paste(
          'No treatment variable was detected in the data set.', 
          'Argument strata_trt was set to TRUE but will be ignored.')))
      }else{
        d_raw <- d_raw %>% 
          mutate(strata = paste0(.strata, .trt , sep='_'))
      }  
    }
    
  
    d_split <- d_raw %>%
      rsample::initial_split(
        strata = tidyselect::all_of('.strata'), 
        prop   = train_prop
      )
    
    # remove the strata variable '.strata' after splitting
    d_split$data <- d_split$data %>% dplyr::select(-tidyselect::any_of(c('.strata')))
    
    d_train_raw  <- rsample::training(d_split) 
    d_test_raw   <- rsample::testing( d_split) 
  }else{
    d_split     <- NULL
    d_train_raw <- d_raw
    d_test_raw  <- NULL
  }
  
  #  RECIPE PREP ####
  # derive variable lists for steps
  
  vars <- prepare_ml_vars(
    data             = d_train_raw,
    thres_count      = thres_count,
    thres_log        = thres_log,
    thres_lump       = thres_lump,
    thres_imp        = thres_imp
  )
  
  
  vars_count   <- vars$count   
  vars_log     <- vars$log   
  vars_nolump  <- vars$nolump 
  vars_exclude <- vars$exclude  
  
  # var %in% 'vars_imp_ignore' : rows with missing values are dropped
  # else if thres_imp is not met vars are dropped
  vars_imp     <- vars$imp %>% setdiff(vars_imp_ignore) 
  
  
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
      recipes::step_impute_knn(., tidyselect::any_of(vars_imp), -recipes::all_outcomes(), -recipes::has_role("ID")) }else{.}
    } %>% 
      
      # ... ... omit observations with missing data in variables ignored in imputation ####
    recipes::step_naomit(recipes::all_predictors()) %>% 
      
      # ... ... near zero variance ####
    recipes::step_nzv(recipes::all_predictors(),
                      freq_cut = thres_nzv_freq, unique_cut = thres_nzv_unique
    ) %>% 
      
      # ... ... log transformation ####
    {if(prep_step_log && length(vars_log)>0){
      recipes::step_log(., tidyselect::any_of(vars_log), base = log_base) 
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
    }else{.}
    } %>%  
      
      # ... ... lump factors ####
    recipes::step_other(., 
                        recipes::all_nominal(), -recipes::all_outcomes(), -recipes::has_role("ID"),
                        -tidyselect::any_of(vars_nolump),
                        threshold = thres_lump, other = level_other) %>%  
      
      # ... ... factor handling ####
    {if(! is.null(vars_ordinalscore)){
      recipes::step_ordinalscore(.,  tidyselect::any_of(!! vars_ordinalscore ) )
    }else{.}
    } %>%  
      
      #  step_novel(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
      # ... .. dummy coding ####
    {if(prep_step_dummy){
      recipes::step_dummy(.,  recipes::all_nominal(), - recipes::all_outcomes(), - recipes::has_role("ID")  , 
                          one_hot = one_hot) 
    }else{.} 
    }
    
    
  } else {
    rcp <- prep_recipe
  }
  
  # ... prep recipe ####
  rcp_prep <- rcp %>% 
    {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE)} %>% 
    purrr::pluck("result")
  
  # were variables from vars_keep_corr removed by step_corr?
  number_corr <- rcp_prep %>% recipes::tidy() %>% dplyr::filter(type == 'corr') %>% dplyr::pull(number)
  
  terms_corr <- rcp_prep %>% 
    recipes::tidy(number = number_corr) %>%
    dplyr::pull(terms)
  
  # adjust_corr <- any(terms_corr %in% vars_keep_corr)
  adjust_corr <- TRUE
  
  vars_exclude_corr <- NULL
  
  # if so re-create feature matrix without corr removal to identify 'competing' variables
  if(adjust_corr){
    
    # prep and train
    rcp_nocorr <- rcp
    rcp_nocorr$steps[[number_corr]]$skip <- TRUE
    
    d_train_nocorr <- rcp_nocorr %>% 
      {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE, training = d_train_raw)} %>% 
      purrr::pluck("result") %>% 
      recipes::bake(new_data = d_train_raw)
    
    vars_exclude_corr <- vars_keep_corr %>% 
      rlang::set_names() %>% 
      purrr::map(~{
        
        d_ref <- d_train_nocorr %>% 
          dplyr::select(tidyselect::all_of(.x))
        
        d_test <- d_train_nocorr %>% 
          dplyr::select_if(is.numeric) %>% 
          dplyr::select(-tidyselect::any_of(c(.x, ".id")))
        
        cor(d_test, d_ref, method = "pearson") %>% 
          as.data.frame() %>% 
          tibble::rownames_to_column() %>% 
          dplyr::filter(abs(!!sym(.x)) > thres_corr) %>% 
          dplyr::pull(rowname)
      })
    
    # modify removal step to add vars_exclude_corr
    number_rm <- rcp %>% recipes::tidy() %>% dplyr::filter(type == 'rm') %>% dplyr::pull(number)
    env_rm    <- rcp$steps[[number_rm]]$terms[[1]] %>% attr(which = '.Environment') 
    
    assign(
      'vars_exclude', 
      c(vars_exclude,  vars_exclude_corr %>%  unlist() %>%  as.character()),
      envir = env_rm
    ) 
    
    # update recipe
    rcp_prep <- rcp %>% 
      {purrr::quietly(recipes::prep)(., strings_as_factors = FALSE, training = d_train_raw)} %>% 
      purrr::pluck("result")
    
  }
  
  # training data
  d_train <- rcp_prep %>% recipes::juice()
  
  # compute test data
  
  if (train_prop < 1){
    d_test  <- rcp_prep %>% 
      {purrr::quietly(recipes::bake)(., d_test_raw)} %>% 
      purrr::pluck("result")
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
                            ifelse(dplyr::near(log_base, exp(1)), 'e', log_base),
                            ') if e1071::skewness() > ',  thres_log,
                            '. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details).'),
                     'No variables were logtransformed.')
    ),
    
    # ... log trafo excluded (integer with low number of values) ####
    thres_count  = list(
      value = ifelse( length(vars_log) > 0 && length(vars_count) > 0, 
                      thres_count, NA_real_),
      text  = ifelse(length(vars_log) > 0 && length(vars_count) > 0,
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
  
  
  # DICT ####
  # prevent error in joining logtr column
  if(is.null(vars_log)) vars_log <- NA_character_
  the_dict <- NULL
  if(!is.null(attr(feature, "dict"))){
    the_dict <- dplyr::bind_rows(
      outcome_dict,
      attr(feature, "dict")  
    ) %>% 
      dplyr::left_join(
        ., 
        tibble::tibble(
          param = vars_log,
          logtr = "Y"
        ),
        by = c("param")
      )
  }
  
  
  
  # OUTPUT #### 
  
  
  prep_output <- list(
    
    # data
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
    
    dict   = the_dict,
    
    source = attr(feature, "source"),
    
    # documentation
    prep_recipe = rcp_prep,
    
    prep_params = prep_params,
    
    removed = list(
      rows = removed_rows,
      cols = removed_columns
    )
  )
  
  prep_output
  
}




# test area ####
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
  strata_trt          = FALSE
  one_hot = FALSE
  
  thres_log           = 2
  thres_count         = 10
  thres_corr          = 0.8
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


if(FALSE){
  
  trt_groups <- c('PLA', 'trt1', 'trt2')
  n_total    <- 90
  
  feature <- tibble(
    .id  = 1:n_total,
    .trt = rep(trt_groups, length.out = n_total),
    cont = rnorm(n_total),
    cont2 = 1.5*cont + rnorm(sd=.01)
  )
  outcome <- tibble(
    .id  = 1:n_total,
    .out = rep(c(
      rep('no event', round(n_total/length(trt_groups))-9),
      rep('event',    9)), 
      length.out = n_total) 
  )
  
  d_raw <- inner_join(outcome, feature) %>% 
    unite(trt.out, .trt, .out, remove = FALSE)
  prop_tot_event_trt <- d_raw %>% 
    pull(trt.out) %>% 
    table %>% 
    {. / sum(.)}
  
  seed <- 1950 # 1130
  train_prop <- .5
  res_out <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = FALSE,
    seed       = seed
  )$data_raw
  
  
}