

# check for low freq factors: check_freq()
# check for category 'other_ml' (default step_other2())
# skewed variables / extreme outliers: skw(na.rm = TRUE) / refactor prepare_ml_outcome()
# proportion NA
# check for potential ordinalscore variable

# guess count variable, suggest providing names in vars_no_trafo if they should be excluded from log trafo and normalization


#' check feature matrix
#' 
#' Running `check_feature()` is by default run in [prepare_ml()] on the 
#' input `feature` to notify the user on sources of potential issues
#' 
#'
#' @param x feature matrix to check, such as the output of [build()].
#' @param check_low_freq,check_other,check_NA,check_count 
#' logicals to control which checks to include. All default to TRUE.
#' @param quiet logical controlling whether any informative messages are 
#' printed to the console
#' @param verbose TRUE by default, FALSE results in messages used for 
#' [prepare_ml()]
#' @param thres_count_distinct check_count
#' @param ... arguments to be passed to other methods
#'
#'
#' @return a nested list, where each entry has a `vars` entry that contains
#' column names that do not pass the respective check (if check is included), 
#' character of length 0 for empty sets.
#' NULL if not tested at all.
#' @export
#'
check_feature <- function(
  x,
  check_low_freq = TRUE,
  check_other    = TRUE,
  check_NA       = TRUE,
  check_count    = TRUE,
  quiet          = TRUE,
  verbose        = TRUE,
  thres_count_distinct = 30,
  ...
){
  

all_args <- rlang::dots_list(..., .homonyms = "error")
  
out <- list()

# check_freq ####
if(check_low_freq){
  # args_check_freq_default <- args(check_freq) %>% 
  #   as.list() %>% 
  #   head(-1) 
  # # $x, $thres 100
  # args_check_freq <- args_check_freq_default %>% 
  #  purrr::list_modify(all_args %>% purrr::keep_at(names(args_check_freq_default)))
  
  out$low_freq <- check_freq(x)
} else {
  out$low_freq <- list(NULL)
}

# skewness ####
if(FALSE){
# $x, $na.rm FALSE
cols_skewness <- x %>% 
  dplyr::select(dplyr::where(is.numeric)) %>% 
  purrr::map_dbl(~skw(.x, na.rm = TRUE)) %>% 
  magrittr::is_greater_than(get_default(prepare_ml, "thres_log")) %>% 
  purrr::keep(isTRUE) %>% 
  names()

out$log_skewness <- cols_skewness

if (length(cols_skewed) > 0) {
  cli::cli_inform(c(
    "Highly skewed variables might be log transformed during ML data prep in {.fn prepare_ml}.",
    "i" = "By default, columns {cols_skewness} would be subject to log transformation (skewness above {get_default(prepare_ml, 'thres_log')}).",
    "*" = "See {.fn step_log_skewness} and {.fn prepare_ml}'s arguments {.code prep_step_log} and {.code thres_log} for details."
  )
  )
}
}else {
 # out$log_skewness <- list(NULL)
}


# other_ml ####
# inform about potentially ambiguous 'other_ml' group  
# recycle prepare_ml_other()   
if (check_other) {
  other2_class <- get_default(step_other2, "other")
  cols_with_class_other2 <- x %>% 
    dplyr::select(dplyr::where(is.character), dplyr::where(is.factor)) %>% 
    purrr::map_lgl(~ any(.x == other2_class)) %>% 
    purrr::keep(isTRUE) %>% 
    names()
  if (length(cols_with_class_other2) > 0 && ! quiet) {
    if(verbose){
      cli::cli_inform(c(
        "Low frequency classes may be pooled during ML data prep into a class {other2_class} in {.fn prepare_ml}.",
        "i" = "Note that {other2_class} is already a value in column{?s} {cols_with_class_other2}.",
        "*" = "See {.fn step_other2} for details and modify your data as needed before proceeding."
      ))
    } else {
      cli::cli_inform(c(
        "Note that {other2_class} is already a value in column{?s} {cols_with_class_other2}.",
        "*" = "Run {.fn check_feature} on your feature matrix for details."
      ))
    }
  }
}

# proportion NA ####
if(check_NA){
  
  thres_imp <-  get_default(prepare_ml, "thres_imp")
  cols_high_miss <- x %>% 
    purrr::map_lgl(~ {!is.na(.x) %>% mean() %>% magrittr::is_less_than(thres_imp)}) %>% 
    purrr::keep(isTRUE) %>% 
    names()
  out$filter_missing <- cols_high_miss
  
  if (length(cols_high_miss) > 0 && ! quiet) {
    cli::cli_inform(c(
      "Variables with a high proportion of missing values will be discarded instead of imputed.",
      "i" = "By default, at least {thres_imp*100}% of data must be available.",
      "i" = "Note that {other2_class} is already a value in column{?s} {cols_high_miss}.",
      "*" = "See {.fn step_other2} for details and modify your data as needed before proceeding."
    )
    )
  }
}


# ordinalscores ####
if (check_count) {
  x_count <- x %>% 
    dplyr::select(-tidyselect::any_of(".id")) %>% 
    dplyr::select(dplyr::where(is.numeric)) %>% 
    dplyr::mutate(dplyr::across(1:dplyr::last_col(), as.character))
  
  looks_like_count <- x_count %>% 
    purrr::map_lgl(~ {
      (readr::guess_parser(.x, guess_integer = TRUE) == "integer") &&
        all(.x >= 0) &&
        dplyr::n_distinct(.x <= thres_count_distinct)
    }) %>% 
    purrr::keep(isTRUE) %>% 
    names()
  
  if(!quiet){
    cli::cli_inform(
      "{cli::qty(looks_like_count)}Data set contains {?''/a} numeric variable{?s} with only positive integer values and few distinct values.",
      "*" = "{cli::qty(looks_like_count)}Please check whether conversion to factor{?s} is appropriate."
    )
  }
  
  out$count_vars <- looks_like_count
}

# RETURN ####
out

}
