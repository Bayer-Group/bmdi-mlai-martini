#'Prepare ML ready data set from outcome and predictor data
#'
#'@description
#'`r lifecycle::badge('maturing')`
#'
#'Given \code{feature}, a tibble representing a wide format feature matrix,
#'and \code{outcome}, a tibble containing the outcome information
#'(regression/classification/survival is supported),
#'\code{prepare_ml()} will provide data sets suitable for various
#'machine learning problems along with additional information.
#'The data preparation steps include, but are not limited to data splitting,
#'handling missing values, normalization, removal of redundant information
#'(highly correlated features).
#'Please refer to the Details section for more information.
#'
#'
#'@param feature feature matrix in wide format, e.g. output object of 
#'\code{\link{build}()}, 
#'i.e. containing \code{.id} column and predictors
#'@param outcome tibble containing \code{.id} column and the outcome of
#'interest, \code{\link{prepare_ml_outcome}()}
#'@param outcome_name single character giving the name of the outcome for 
#'regression or classification. For survival and repeated measurements analysis
#'(classification or regression), resp., a named vector of length two needs
#'to be specified, `c(.time = "<time-coln>", .status = "<status-coln>")` 
#'for survival and 
#'`c('.rmtime' =  "<timepoint-coln>", '.out' = "<endpoint-coln>")` 
#'for repeated measurements, resp. See Details section.
#'@param level_order level order for a classification outcome.
#' Default \code{NULL} keeps the natural order (only used for classification).
#'@param prep_recipe a custom, pre-defined \code{recipes::recipe()} may be 
#'provided for data preparation. Defaults to NULL, yielding a data-driven
#'preparation. Please refer to the details section to learn about the 
#'individual recipe steps.
#'@param train_prop the proportion of data to be used for the training set. 
#'Has to be in \[0.5;1.0\]. Defaults to 3/4, keeping a quarter of the data 
#'for testing.
#'@param strata_trt boolean. Expand default stratum variable (\code{.out} 
#'for classification, \code{.status} for tte, \code{NULL} for regression)
#'by trt (if character, else ignored). Defaults to FALSE, but is highly 
#'recommended to be set to TRUE.
#'@param seed optionally set a seed before the data splitting. 
#'@param prep_step_knnimpute,prep_step_log,prep_step_normalize,prep_step_corr,prep_step_dummy
#'logicals determining whether or not the corresponding step function should 
#'be included in the recipe, possibly specified further using additional 
#'parameters (`thres_*`, `log_base`, `one_hot`). Please refer to the details 
#'section for the full list of recipe steps.
#'@param thres_imp Minimal proportion of non-missing data per feature required
#'to be kept in the data and completed using \code{recipes::step_impute_knn()}. 
#'Variables not meeting the threshold will be dropped and not be included in 
#'\code{data_prep} data. 
#'Per default \code{thres_imp = 0.8}, i.e. variables will be dropped if the 
#'proportion of available data is less than 80%. Variables listed in 
#'\code{vars_imp_ignore} will never be imputed, observations with missing data
#'in the respective variables will be removed.
#'@param thres_log variables will be log-transformed (with base \code{log_base})
#'if \code{prep_step_log = TRUE}, all observations are positive, and 
#'\code{e1071::skewness() > thres_log}, where `thres_log` defaults to 2.   
#'@param thres_corr if \code{prep_step_corr = TRUE}, \code{thres_corr} is passed
#'to \code{recipes::step_corr()}'s \code{threshold} argument with a default
#'of 0.9 to remove highly correlated features
#'@param thres_nzv_freq,thres_nzv_unique parameters passed to
#'\code{recipes::step_nzv()} with defaults 
#'\code{thres_nzv_freq = 95/5)} and \code{thres_nzv_unique = 10} 
#'@param thres_count integer variables with no more than `thres_count` distinct 
#'values are considered as count variables and are excluded from the 
#'log-transformation and normalization. Defaults to 10.
#'@param thres_lump this parameter is used to prevent renaming of a single low 
#'frequency class to 'other' by \code{recipes::step_other()}, to
#'which `thres_lump` is passed as parameter `threshold`. Defaults to 0.05.
#'@param one_hot boolean. passed to \code{recipes::step_dummy()} to choose one 
#'hot encoding over dummy encoding
#'@param vars_imp_ignore variables that shall not be imputed can be specified 
#'in \code{vars_imp_ignore} (vector of column names, defaults to 
#'\code{vars_imp_ignore = '.trt'}). Observations with missing values in these
#' variables will be removed. Removal is documented in `removed$rows`.
#'@param vars_fct_expl_na column names of factors for which NAs should be
#' treated as an explicit factor level. Defaults to NULL.
#'@param vars_keep_corr choose these variables over other options when removing
#'variables due to high correlation in \code{recipes::step_corr()}. 
#'See \code{recipes::step_rm()} below for details. 
#'@param vars_ordinalscore  column names of ordinal factor variables to be 
#'converted into numeric scores. Defaults to NULL.
#'@param log_base base to use for log-transformation in 
#'\code{recipes::step_log()}. Defaults to _exp(1)_.
#'@param outlier_remove,outlier_ctrl For outcome mode regression only, see 
#'\code{\link{prepare_ml_outcome}()}
#'for details on how outliers are removed from outcome variables.
#'`outlier_remove` defaults to FALSE, `outlier_ctrl` to `list(coef = 3)`.
#'@param quiet boolean. Suppress messages during outcome preparation to the 
#'console on NA and outlier removal, resp. Defaults to `FALSE`.
#'
#'@details 
#'
#'The following order of recipe steps for data preparation will be applied
#'(if no recipe is provided). The variable sets that a particular step function
#' will be applied to are determined based on user input 
#'and output of the function \code{\link{prepare_ml_vars}()}, respectively.
#'Further details on particular steps are given below.
#' 
#'* drop variables e.g. not meeting the minimum threshold for non-missing 
#'data proportion (`step_rm()`) or for variable removal related
#'to the `vars_keep_corr` parameter (see below).  
#'* remove observations with missing data in outcome (`step_naomit()`)
#'* knn imputation on variables with missing values that are not explicitly 
#'excluded from imputation (`vars_imp_ignore`). Please note, that missing 
#'values can still occur after imputation if a large majority (or all) of the 
#'imputing variables are also missing (see `?recipes::step_impute_knn()`).
#'Related subjects/observations will be removed to obtain a complete data set 
#'and listed in removed$rows of the output object.
#'* omit observations with remaining missing values (i.e. in variables that 
#'were excluded from imputation and not dropped before) (`step_naomit()`)
#'* removal of near-zero variance variables (`step_nzv()`)
#'* log-transformation (`step_log()`)
#'* normalization (`step_normalize()`)
#'* removal of highly correlated variables (`step_corr()`)
#'* lumping of low frequency factor levels into a single class (`step_other()`)
#'* transform ordinal factors into numeric variables (`step_ordinalscore()`)
#'* dummy/one hot encoding (`step_dummy()`) 
#' 
#'The \code{vars_keep_corr} parameter allows to prioritize these variables in 
#'the \code{step_corr()} part of the recipe over the variables that yield high
#' correlations with them (i.e. exceeding \code{thres_corr}). 
#'This allows to choose a _representative_ from a set of correlated variables 
#'that is e.g. commonly used in the context of the indication or easier to 
#'interpret. Please note, that these imposed restrictions may increase the 
#'total number of removed variables in this step in comparison to the 
#'unrestricted version.
#' 
#'A note on \code{step_impute_knn()} and the interpretation of the 
#'\code{prep()}ped recipe: The variables listed for this step are the ones 
#'that are **used** for the imputation step. It does not mean that missing 
#'values in these variables have been or will be imputed. 
#'For more details on this matter please refer to the documentation of
#'tidymodels and the difference in \code{prep()} and \code{bake()}, 
#'in particular. For example, \code{vars_imp_ignore} includes the standard
#'treatment variable \code{.trt} by default to prevent any imputations; 
#'however, it will be listed in the variable set of the \code{prep()}ped
#'recipe (for older versions of `recipes` package). Don't panic. #rtfm.
#' 
#'For repeated measurement analyses, all observations of the same `.id`
#'will end up the either in the training or test set (using 
#'`rsample::group_initial_split()`). Note that the strata argument will be
#'ignored (with a warning) for versions below 1.1.1.
#'Currently, grouping is not accounted for in missing value imputation yet.
#' 
#'Specification of `outcome_name` for survival analysis or repeated 
#'measurements: For survival analysis, specify column names for 'time' and
#''status' of the `Surv` object: 
#'`c(.time = "<time-coln>", .status = "<status-coln>")`, where `.time` is 
#'numeric and `.status` is binary with 0 coding for censored, and 1 coding
#'for event. Currently, only right-censoring is supported. 
#' 
#'For repeated measurements, specify `outcome_name` as 
#'`c('.rmtime' =  "<timepoint-coln>", '.out' = "<endpoint-coln>")`. 
#'The outcome mode will be guessed as regression or classification according 
#'to the type of the column specified in `.out`. 
#' 
#'If `outcome_name = NULL` (default), the first column in `outcome` that's 
#'not `.id` is chosen for `outcome_name` and the outcome mode is guessed 
#'accordingly. Thus, neither survival nor repeated measurement analysis will 
#'ever be guessed.
#' 
#' 
#' 
#' @return 
#' 
#' ## Data sets
#' 
#'\code{prepare_ml()} produces a list that contains the data set both with 
#'(\code{data_prep}) and without (\code{data_raw}) applying the specified ML 
#'preparation steps. Both versions are split in \code{train} and \code{test} 
#'set. In addition, \code{split} contains the combined
#'\code{rsample::initial_split()} object that the \code{train} and \code{test} 
#'data was extracted from. Depending on the programming workflow, one might be
#'more convenient to use than the other. Both \code{data_test} slots as well
#'as \code{split} are `NULL` if \code{train_prop} was set to 1 
#'(i.e. no splitting was done) and \code{train} contains the full ML data set.
#'  
#' 
#'The slot \code{outcome} contains a list giving \code{name}, the standardized
#'names of the output column in the data sets ( \code{.out} for
#'regression/classification, \code{.time} and \code{.status} for survival, 
#'as well as a \code{mode}, character string of the outcome mode 
#'\code{regression/classification/survival} 
#' 
#'The dictionary available as an attribute of `feature` is updated with 
#'information on the outcome variable, any log-transformation as well as 
#'alternative labels (`label2`, `label3`) indicating correlated variable groups
#'e.g. HB (HCT), where HB is kept for the analysis, HCT was dropped due to
#'absolute correlation above `thres_corr`. Dictionary is available from 
#'the \code{dict} slot, `NULL` if no such attribute is defined.
#' 
#'The \code{source} slot simply passes the \code{source} attribute of 
#'\code{feature}, NULL if no such attribute is defined.
#'If \code{\link{build}()} from the \code{martini} package was used to generate
#'\code{feature}, this attribute lists the full paths of the files that were 
#'used in data generation of \code{feature}. 
#' 
#'## Data preparation and documentation
#' 
#'\code{prep_recipe} contains the prepared recipe object, 
#'\code{prep_params} documents the parameters/thresholds used in the data 
#'preparation, giving bare \code{value} slots, as well as a verbose description
#' in \code{text}.
#'\code{removed} gives a list of removed \code{rows} and \code{columns} along 
#'with the information on why/in which recipe step the data was removed.
#'\code{high_corr} a tibble listing correlations above \code{thres_corr}. 
#'\code{NULL} if \code{prep_step_corr = FALSE}.
#'\code{input} a list giving the `martini` \code{packageVersion} and a 
#'list of (most) input parameters, including the seed used 
#' 
#'@section Authors:
#'Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#'@export 


prepare_ml <- function(
  feature,
  outcome,
  outcome_name = NULL,
  level_order  = NULL,
  prep_recipe  = NULL,
  train_prop   = 3 / 4,
  strata_trt   = FALSE,
  seed         = 1130,
  
  prep_step_normalize = TRUE,
  prep_step_knnimpute = TRUE,
  prep_step_log       = TRUE,
  prep_step_corr      = TRUE,
  prep_step_dummy     = FALSE,
  
  thres_log           = 2,
  thres_count         = 10,
  thres_corr          = 0.9,
  thres_lump          = 0.05,
  thres_imp           = 0.8,
  thres_nzv_freq      = 95 / 5,
  thres_nzv_unique    = 10,
  
  vars_imp_ignore     = c(".trt"),
  vars_fct_expl_na    = NULL,
  vars_ordinalscore   = NULL,
  vars_keep_corr      = NULL,
  
  one_hot             = NULL,
  
  log_base            = exp(1),
  outlier_remove      = FALSE,
  outlier_ctrl        = list(coef = 3),
    
  quiet               = FALSE
    
) {
  
  # save all input args
  all_args <- as.list(environment())
  
  if (prep_step_dummy && is.null(one_hot)) {
    one_hot <- FALSE
    cli::cli_inform(c(
      "You set {.arg prep_step_dummy = TRUE}.",
      "i" = "This preparation step uses dummy-coding based on reference level 
      by default, i.e. {.arg one_hot = FALSE} (see {.fn ?recipes::step_dummy} 
      for details}.",
      "*" = "Depending on your chosen ML technique consider setting
      {.arg one_hot = TRUE}."
    ))
  }
  
  # OUTCOME ####
  
  #COMBAK account for .rmtime renaming in prepare_ml_outcome
  # (necessary for merge)
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
  outcome_mode  <- outcome_prep$outcome_mode
  outcome_dict  <- outcome_prep$outcome_dict
  na_outcome    <- outcome_prep$na_outcome
  id_outlier    <- outcome_prep$id_outlier
  
  if (length(id_outlier) > 0 && !quiet) {
    usethis::ui_info(paste0(
      "Based on the outcome distribution, ", length(id_outlier),
      ifelse(length(id_outlier) > 1,
             " observations were ", " observation was "),
      "identified as outlier and removed from the outcome data prior to data splitting and preprocessing.\n\n"
    ))
  }
  
  
  if (length(na_outcome) > 0 && !quiet) {
    usethis::ui_info(paste0(     length(na_outcome),
      ifelse(length(na_outcome) > 1,
             " observations were ", " observation was "),
      "removed from the outcome data prior to data splitting and preprocessing due to missingness.\n\n"
    ))
  }
  
  # ... intersect 'vars_fct_expl_na' with factor columns ####
  if (!is.null(vars_fct_expl_na)) {
    vars_fct_expl_na <- feature %>% 
      dplyr::select_if(is.factor) %>% 
      colnames() %>% 
      intersect(vars_fct_expl_na)
    # catch special case 'no factors in feature'
    if (length(vars_fct_expl_na) == 0) vars_fct_expl_na <- NULL
  }
  
  level_other <- "other"
  
  # MERGE OUTCOME AND FEATURE  ####
  
  clmn_by <- intersect(
    c(".id", ".rmtime"), 
    intersect(colnames(outcome), colnames(feature))
  )
  
  if (!all(
    outcome[clmn_by] %>% purrr::map_chr(class) %>% unname() ==
    feature[clmn_by] %>% purrr::map_chr(class) %>% unname()
  )) {
    usethis::ui_stop(paste0(
      "Column(s) used for joining `outcome` and `feature` are not of the same type."
    ))
  }
  
  #COMBAK check merge for rm case
  d_raw <- outcome %>%
    dplyr::inner_join(
      feature, 
      by = clmn_by
    )
  
  if (nrow(d_raw) == 0) {
    cli::cli_abort(c(
      "x" = "There are no common values in {.code .id} columns of {.arg outcome} and {.arg feature}.",
      "*" = "{.arg outcome}: {sort(head(outcome$.id))}",
      "*" = "{.arg feature}: {sort(head(feature$.id))}",
      ">" = "Please check your id columns."
    ))
  }
  
  # DATA SPLIT ####
  data_split_res <- do.call(
    prepare_ml_data_split,
    tibble::lst(
      train_prop, 
      seed, 
      data = d_raw,
      outcome_mode,
      strata_trt
    )
  ) 
  
  d_train_raw <- data_split_res$d_train_raw
  d_test_raw  <- data_split_res$d_test_raw
  
  #  RECIPE ####
  
  rcp_output <- prepare_ml_recipe(
    
    data         = d_train_raw,
    prep_recipe  = prep_recipe,
    
    corr_method = "pearson",
    corr_use    = "pairwise.complete.obs",
    
    thres_list = tibble::lst(
      thres_log,
      thres_count,
      thres_corr,
      thres_lump,
      thres_imp,
      thres_nzv_freq, 
      thres_nzv_unique
    ),
    
    step_list = tibble::lst(
      prep_step_normalize,
      prep_step_knnimpute,
      prep_step_log,
      prep_step_corr,
      prep_step_dummy
    ),
    
    vars_imp_ignore     = vars_imp_ignore,
    vars_fct_expl_na    = vars_fct_expl_na,
    vars_ordinalscore   = vars_ordinalscore,
    vars_keep_corr      = vars_keep_corr,
    
    level_other = level_other, # 'other'
    one_hot     = one_hot,
    log_base    = log_base
    
  )
  
  rcp_prep  <- rcp_output$rcp_prep
  vars      <- rcp_output$vars
  steps     <- rcp_output$steps
  thres     <- rcp_output$thres
  high_corr <- rcp_output$high_corr
  
  # training data
  d_train <- rcp_prep %>% recipes::juice()
  
  # compute test data
  if (train_prop < 1) {
    d_test  <- rcp_prep %>% 
      #recipes::check_range(recipes::all_numeric(), slack_prop = 0.1) %>% 
      {purrr::quietly(recipes::bake)(., d_test_raw)} %>% 
      purrr::pluck("result")
  } else {
    d_test  <- NULL
  }
  
  # CLEAN UP ####
  
  for (i in 1:ncol(d_train)) {
    attr(d_train[[i]], "format.sas") <- NULL
    attr(d_test [[i]], "format.sas") <- NULL
    attr(d_train[[i]], "label"     ) <- NULL
    attr(d_test [[i]], "label"     ) <- NULL
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
        stringr::str_remove("^step_")
    })
  
  # create list of removed columns per step for output object
  removed_columns <- prep_steps %>% 
    purrr::map(~ .x$removal) %>% 
    # keep all steps with a 'removal' slot
    purrr::keep(~{!is.null(.x)}) %>% 
    # set empty 'removal' slots (=vector of length 0) to NULL
    purrr::map(~{if (length(.x) > 0) .x})
  
  
  if ("rm" %in% names(removed_columns)) {
    # step_rm and step_corr only used simultaneously in recipe
    
    removed_columns$corr <- removed_columns$corr  %>% 
      c(removed_columns$rm) %>% 
      # 'rm' is returned as named vector 
      unname()
    
    removed_columns$rm <- NULL
    
  }
  
  
  # DOCUMENT PREP PARAMETER SETTINGS ####
  # NOTE TEMP text slots will be removed once documentation is fully available
  # TODO  documentation of pre-processing parameters    
  prep_params <- list(
    
    # ... log trafo  ####
    thres_log  = list(
      value = ifelse(steps$prep_step_log, thres$thres_log, NA),
      text  = ifelse(steps$prep_step_log,
                     paste0("Variables were log transformed (base ", 
                            ifelse(dplyr::near(log_base, exp(1)), "e", log_base),
                            ") if e1071::skewness() > ",  thres$thres_log,
                            ". Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details)."),
                     "No variables were log transformed.")
    ),
    
    # ... log trafo excluded (integer with low number of values) ####
    thres_count  = list(
      value = ifelse(length(vars$vars_log) > 0 && length(vars$vars_count) > 0, 
                     thres$thres_count, NA_real_),
      text  = ifelse(length(vars$vars_log) > 0 && length(vars$vars_count) > 0,
                     paste0("Variables were excluded from log transformation if they are integer coded 
                             and have ", thres$thres_count, " distinct values."),
                     "Not applicable.")
    ),
    
    
    # ... correlated variables ####
    thres_corr  = list(
      value = ifelse(steps$prep_step_corr, thres$thres_corr, NA),
      text  = ifelse(steps$prep_step_corr,
                     paste0("The applied cutoff for removal of variables due to high correlations was ",  thres$thres_corr, "."),
                     "No variables were removed for reasons of high correlation.")
    ),  
    
    vars_keep_corr = list(
      value = ifelse(!is.null(vars$vars_keep_corr), vars$vars_keep_corr, NA),
      text  = ifelse(steps$prep_step_corr && !is.null(vars$vars_exclude_corr),
                     "Variable selection in recipes::step_corr() was adjusted according to 'vars_keep_corr'",
                     "No variables were excluded specifically due to high correlation with the variables in 'vars_keep_corr'")
    ),
    
    
    # ... lump factor levels (always applied) ####
    thres_lump = list(
      value = thres$thres_lump,
      text  = paste0("Low frequency factor levels were lumped using recipes::step_other(threshold = ", thres$thres_lump, "). ")  
    ),
    
    # ... imputation/missing values  ####
    ## imputation/dropping of variables based on available probability
    imp_ignore = list(
      value = ifelse(steps$prep_step_knnimpute, thres$thres_imp, NA),
      text  = ifelse(steps$prep_step_knnimpute,
                     paste0("Variables were dropped if the proportion of available data was less than ", 
                            thres$thres_imp * 100, "%."),
                     "No imputation was done on the feature matrix.")
    ),
    
    # ... nzv ####
    nzv = list(
      value = list(freq_cut = thres$thres_nzv_freq, unique_cut = thres$thres_nzv_unique),
      text  = paste0(
        "Highly sparse and unbalanced variables were dropped using ",  
        "recipes::step_nzv(freq_cut = ", round(thres$thres_nzv_freq),
        ", unique_cut = ", thres$thres_nzv_unique, ")."
      )
    )
    
  )    
  
  # ... outlier_remove ####
  # NOTE adjust to output of 'prepare_ml_outcome()'
  
  if (outcome_mode == "regression") {
    prep_params <- append(
      prep_params, 
      list(
        value = ifelse(
          outlier_remove,
          unlist(outlier_ctrl), 
          NA),
        text  = ifelse(
          outlier_remove,
          paste0("Based on the outcome distribution, observations outside the interval ",
                 "[q25 - ", outlier_ctrl$coef, "*iqr; ",  
                 "q75 + ", outlier_ctrl$coef, "*iqr] were removed prior to data splitting and preprocessing."
          ),
          NA)
      )
    )
  } 
  
  
  # DICT ####
  
  # prevent error in joining logtr column
  if (is.null(vars$vars_log)) vars$vars_log <- NA_character_
  the_dict <- NULL
  if (!is.null(attr(feature, "dict"))) {
    the_dict <- dplyr::bind_rows(
      outcome_dict,
      attr(feature, "dict")  
    ) %>% 
      dplyr::left_join(
        ., 
        tibble::tibble(
          param = vars$vars_log,
          logtr = "Y"
        ),
        by = c("param")
      )
    
    # add alternative label with correlated variables
    add_labels <- high_corr %>% 
      dplyr::left_join(
        the_dict %>% 
          dplyr::select(column, "label_x" = label) %>% 
          dplyr::distinct(),
        by = c("x" = "column")
      ) %>% 
      dplyr::left_join(
        the_dict %>% 
          dplyr::select(column, "label_y" = label) %>% 
          dplyr::distinct(), 
        by = c("y" = "column")
      ) %>% 
      dplyr::select(-r) %>% 
      dplyr::group_by(x, label_x) %>% 
      dplyr::summarize(
        label2  = paste0(unique(x),       " (", paste(y, collapse = ", "), ")"),
        label3  = paste0(unique(label_x), " (", paste(label_y, collapse = ", "), ")"), 
        .groups = "drop"
      ) %>% 
      dplyr::select("column" = x, tidyselect::everything(), -label_x)
    
    the_dict <- dplyr::left_join(
      the_dict, 
      add_labels,
      by = "column"
    ) %>% 
      dplyr::mutate(
        label2 = dplyr::coalesce(label2, column),
        label3 = dplyr::coalesce(label3, label)
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
    
    outcome = list(
      name = list(
        "regression"     = ".out", 
        "classification" = ".out", 
        "survival"       = c(".time", ".status")
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
    ),
    
    high_corr = high_corr,
    
    input = list(
      martini = utils::packageVersion("martini"), 
      args = all_args %>% 
        # TODO usage conditional by installed purrr version
        magrittr::inset(c("outcome", "feature", "outcome_name", "quiet"), NULL)
    )
  )
  
  # TODO add attribute to indicate repeated measurements data
  
  prep_output
  
}



#' split data in train and test
#'
#' @param data data to split
#' @param train_prop proportion to use for training split, must be in (0.5, 1]
#' @param seed defaults to NULL
#' @param outcome_mode used in stratification
#' @param strata_trt logical
#'
#' @return
#' a named list containing 
#' 
prepare_ml_data_split <- function(
    
  data, 
  train_prop,
  strata_trt,
  
  seed = NULL,
  outcome_mode
  
) {
  
  train_prop_valid <- c(0.5, 1)
  
  if (!dplyr::between(train_prop, train_prop_valid[1], train_prop_valid[2])) {
    
    cli::cli_abort(c(
      "The provided training proportion {.code train_prop} is expected to fall within 
        [{train_prop_valid[1]};{train_prop_valid[2]}]",
      "x" = "You've supplied {train_prop}."
    )
    )
    
  } 
  
  if (train_prop < 1) {
    if (!is.null(seed))  set.seed(seed)
    
    # create a new column .strata for stratified splitting by outcome
    data <- data %>% 
      {if (outcome_mode == "classification") {
        dplyr::mutate(., .strata = .out)
      } else {.}
      } %>% 
      
      {if (outcome_mode == "survival") {
        dplyr::mutate(., .strata = .status)
      } else {.}
      } %>% 
      
      # no outcome stratification for regression, but create the column
      # anyways to make it extendable by strata_trt = TRUE
      {if (outcome_mode == "regression") {
        dplyr::mutate(., .strata = "")
      } else {.}
      }
    
    # extend strata variable by treatment
    if (strata_trt) {
      if (! ".trt" %in% colnames(data)) {
        usethis::ui_info(crayon::silver(paste(
          "No treatment variable was detected in the data set.", 
          "Argument strata_trt was set to TRUE but will be ignored.")))
      } else {
        data <- data %>% 
          dplyr::mutate(.strata = paste0(.strata, .trt, sep = "_"))
      }  
    }
    
    if (!c(".rmtime") %in% names(data)) {
      
      d_split <- data %>%
        rsample::initial_split(
          strata = tidyselect::all_of(".strata"), 
          prop   = train_prop
        )
      
    } else {
      
      strata_ignored <- utils::packageVersion("rsample") %>%
        package_version() %>% 
        {. < "1.1.1"}
      
      if (strata_ignored) {
        cli::cli_warn(paste(
          "Please update `rsample` to version 1.1.1 or higher",
          "to enable stratified sampling in `rsample::group_initial_split()`"
        ))}
      
      d_split <- data %>%
        rsample::group_initial_split(
          prop   = train_prop,
          group  = ".id",
          strata = tidyselect::all_of(".strata")  # ignored if rsample < '1.1.1'
        )
      
    }
    
    # remove the strata variable '.strata' after splitting
    d_split$data <- d_split$data %>% 
      dplyr::select(-tidyselect::any_of(c(".strata")))
    
    d_train_raw  <- rsample::training(d_split) 
    d_test_raw   <- rsample::testing(d_split) 
    
  } else {
    
    d_split     <- NULL
    d_train_raw <- data
    d_test_raw  <- NULL
  }
  
  tibble::lst(
    d_train_raw, 
    d_test_raw
  )
}
