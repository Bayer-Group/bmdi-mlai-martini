#' Prepare ML ready outcome data set
#'
#' Prepares an ML ready outcome data set (used in \code{\link{prepare_ml}})
#'
#' @param outcome tibble containing \code{.id} column and the outcome of interest
#' @param outcome_name if NULL (default), the first column that's not `.id` is chosen for outcome_name
#' and the outcome_type is guessed to be either classification or regression.
#' One may also provide a single character giving the name of the outcome column OR 
#' a named vector of length two giving the column names for the 'time' and 'status' data in survival analysis, i.e. 
#' `c(.time = "<time-coln>", .status = "<status-coln>")`,
#' where `.time` is numeric and `.status` is binary with 0 coding for censored, and 1 coding for event.
#' Currently, only right-censoring is supported. Please note, that survival will never be guessed.
#' @param level_order Level order for a classification outcome. \code{NULL} keeps the natural order (only used for classification).
#' @param outlier_remove Remove outliers in a regression outcome based on the 'boxplot definition'. The outlier coefficient can be modified
#' in \code{outlier_ctrl} (only used for regression).
#' @param outlier_ctrl Control list for the outlier removal, if \code{outlier_remove} is \code{TRUE}. Currently, the list contains only
#' the boxplot outlier coefficient \code{coef}, which defaults to 3.
#' 
#' 
#' @return 
#' 
#' A list with the following slots
#' 
#' \item{outcome}{The outcome data set containing only the id and one or two columns 
#' with standardized column names (\code{.out} for regression or classification, \code{.time} and \code{.status} for survival).}
#' \item{outcome_name}{Named vector with the original name(s) of the outcome variable(s).}
#' \item{outcome_label}{Named vector with the labels(s) of the outcome variable(s). If the columns of \code{outcome} do not contain labels,
#' the column name is used instead.}
#' \item{outcome_mode}{The outcome mode (\code{regression}, \code{classification} or \code{survival}.}
#' \item{outcome_dict}{Dictionary tibble for the outcome variable(s). If no label was provided for the selected columns, the column name will be reused as label in the dictionary.}
#' \item{na_outcome}{The IDs of NAs in \code{outcome}.}
#' \item{id_outlier}{The IDs of removed outliers.}
#' 
#' @section Authors:
#' 
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @md

prepare_ml_outcome <- function(
  
  outcome,
  outcome_name   = NULL,
  level_order    = NULL,
  outlier_remove = FALSE,
  outlier_ctrl   = list(coef = 3)
  
){
  
  # outcome_name ####
  # guess outcome column, if outcome_name = NULL and outcome has more than 2 columns: use first that's not '.id'
  
  if(is.null(outcome_name)){
    
    outcome_options <- setdiff(colnames(outcome), '.id')
    needs_guessing  <- length(outcome_options) > 1
    outcome_name    <- outcome_options[1]
    
    if(needs_guessing){
      usethis::ui_info( paste0(
        crayon::silver('The outcome object you provided has multiple options. The following option was chosen: \n'), # MARTINI chose: \n'), 
        '  ' , crayon::magenta( outcome_name)     , '\n\n'
      ))
    } 
    
  } else { # outcome_name is provided
    
    # do columns exist?  
    clmn_miss <- setdiff(outcome_name, colnames(outcome))    
    
    if(length(clmn_miss) > 0){
      cli::cli_abort(c(
        'i' = 'You selected {.code {outcome_name}} as the column{?s} defining your outcome.',
        'x' = '{.code {clmn_miss}} {?is/are} not present in the data set {.arg outcome}.',
        '*' = 'Please correct the input of {.arg column_name}.' # or let {.fn prepare_ml_outcome} choose from existing columns (regression and classification only).'
      ))
    }
    
    
    # check number of provided outcome columns
    if(length(outcome_name) > 2 ){ 
      usethis::ui_stop('Please check input for outcome_name. No more than two columns might be selected.')
    }else if( length(outcome_name) == 2 ){
      # check column names and types for survival  
      
      names_valid  <- {sort(names(outcome_name)) == c('.status', '.time')} %>%  all()
      if(!names_valid)  usethis::ui_stop('For survival analysis, please provide vector with names .status and .time for outcome_name.')
      
      status_valid <- outcome[, outcome_name['.status']] %>% dplyr::pull() %>%  { . %in% c(0,1) } %>%  all() 
      if(!status_valid) usethis::ui_stop('status may only contain values 0 and 1.')
      # stops if NAs are present
      
      time_valid   <- outcome[, outcome_name['.time'  ]] %>% dplyr::pull() %>%  is.numeric()
      if(!time_valid)   usethis::ui_stop('Please check type of time column.')
      
      # sort by name
      outcome_name <- outcome_name[ c('.time', '.status')]
    }  
  } # -> outcome_name is set, either of length one or two
  
  
  # outcome_mode ####
  if(length(outcome_name) == 2){
    outcome_mode <- 'survival'
  }else{ 
    outcome_mode <- ifelse(
      is.numeric(outcome[[outcome_name]])
      && dplyr::n_distinct(outcome[[outcome_name]]) > 5,
      "regression", 
      "classification"
    )
  }  
  
  # for consistency, add name if mode != survival
  if(outcome_mode != 'survival'){
    names(outcome_name) <- '.out'
  }
  
  # outcome_label ####
  # extract label(s) of outcome before potentially mutating to factor (classification)
  # for consistency, outcome label is a named vector.
  outcome_label <- outcome_name 
  purrr::iwalk(outcome_name, ~ {
    the_label <- labelled::var_label(outcome)[.x] %>% unlist()
    if(!is.null(the_label)){
      outcome_label[.y] <<- the_label
    }
  })
  
  
  # outcome dict ####
  outcome_dict <- tibble::tibble(
    param  = names(outcome_name)) %>% 
    dplyr::mutate(
      column = param,
      source = "user_outcome",
      label  = outcome_label[param]
    )
  
  
  # outcome data ####
  
  # ... standardize outcome name ####
  outcome <- outcome %>% 
    dplyr::select(tidyselect::all_of('.id'), tidyselect::all_of(outcome_name))
  
  # ... classification -> factor(), fct_relevel() ####
  if (outcome_mode == "classification"){
    
    outcome <- outcome %>% dplyr::mutate_at(".out", factor) # strips labels
    outcome_level <- outcome[[".out"]] %>% levels()
    
    if (!is.null(level_order)){
      level_order <- intersect(level_order, outcome_level)
      if (length(level_order) > 0){
        outcome <- outcome %>% 
          dplyr::mutate_at(".out", ~ forcats::fct_relevel(., level_order))
      }
    }
    
  }
  
  # ... regression -> outlier_removal ####
  id_outlier <- NULL
  if(outcome_mode == "regression" && outlier_remove){
    
    # with c = outlier_ctrl$coef, exclude observations outside [q25 - c*iqr;  q75 + c*iqr]
    q   <- quantile(outcome$.out, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
    loq <- q + c(-1,1) * abs(outlier_ctrl$coef[1]) * diff(q)
    is_outlier <- !dplyr::between(outcome$.out, loq[1], loq[2])
    
    id_outlier <- outcome$.id[which(is_outlier)]
    
    outcome    <- outcome %>% dplyr::filter(!.id %in% id_outlier) # !is.na(.out) NAs will be removed and tracked in the recipe
    
  }
  
  # na_outcome: IDs of NA rows ####
  na_outcome <- outcome %>% 
    dplyr::mutate_at(dplyr::vars(-tidyselect::any_of(".id")), is.na) %>%  
    dplyr::rowwise() %>% 
    dplyr::mutate(any_na  = dplyr::c_across(-.id) %>% any()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(any_na) %>% 
    dplyr::pull(.id)
  
  attributes(na_outcome) <- NULL
  if (length(na_outcome) == 0) na_outcome <- NULL
  
  # remove NA rows ####
  outcome <- outcome %>% dplyr::filter(!.id %in% na_outcome)
  
  # output ####
  list(
    outcome       = outcome,
    outcome_name  = outcome_name,
    outcome_label = outcome_label,
    outcome_mode  = outcome_mode,
    outcome_dict  = outcome_dict,
    na_outcome    = na_outcome,
    id_outlier    = id_outlier
  )
  
}

