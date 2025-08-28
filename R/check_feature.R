#' check feature matrix
#' 
#' Running `check_feature()` is by default run in [prepare_ml()] on the 
#' input `feature` to notify the user on sources of potential issues. 
#' 
#'
#' @param x feature matrix to check, such as the output of [build()].
#' @param check_low_freq,check_other,check_missing,check_count 
#' logicals to control which checks to include. All default to TRUE.
#' @param quiet logical controlling whether any informative messages are 
#' printed to the console. Defaults to FALSE.
#' @param thres_count [check_count()]
#' @param thres_low_freq passed to [check_freq()]
#' @param thres_missing passed to [check_non_missing()]
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
  check_missing  = TRUE,
  check_count    = TRUE,
  quiet          = FALSE,
  thres_count    = 30,
  thres_low_freq = NULL,
  thres_missing  = NULL,
  ...
){
  

quiet_checks <- TRUE
out <- list()

# check_freq ####
if (check_low_freq) {
  out$low_freq <- check_freq(x, thres = thres_low_freq, quiet = quiet_checks)
} else {
  out$low_freq <- list(NULL)
}


# other_ml ####
# inform about potentially ambiguous 'other_ml' group 
if (check_other) {
  out$other <- check_other_class(x, other2_class = NULL, quiet = quiet_checks)
} else {
  out$other <- NULL
} 

# proportion NA ####
if (check_missing) { 
  out$missing <- check_non_missing(x, thres = thres_missing, quiet = quiet_checks)
} else {
  out$missing <-  NULL
}

# 
# # counts ####
if (check_count) {
  out$count <- check_count(x, thres = thres_count, quiet = quiet_checks)
} else {
  out$count <-  NULL
}


# RETURN ####

finding_yn <- out %>% 
  purrr::map(~ {.x$finding %>% purrr::set_names(.x$check)}) %>%
  unname() %>% 
  unlist()

checks_finding <- finding_yn %>% 
  purrr::keep(isTRUE) %>% 
  names()

if (!quiet) {
  # build message
  msg <- c(
    "i" = "Data set was checked for causes for potential downstream issues with ML preparation."
  )
  
  if (length(checks_finding) > 0) {
    msg <- c(
      msg, 
      "!" = "Potential issues were identified.",
      "*" = "Run {checks_finding} on the input to {.fun prepare_ml}'s {.arg feature} to learn more."
    )
  } else {
    msg <- c(
      msg, 
      "v" = "No issues were detected for the particular parameters."
    )
  }
  
  cli::cli_inform(msg)
}

invisible(list(
  summary = finding_yn,
  details = out
))

}

#' Identify factors with low frequency classes
#'
#' @param x data set to check
#' @param thres integer. 
#' Factors with at least one class of size smaller than `thres` will be identified. 
#' Defaults to NULL, 
#' @param quiet whether to suppress printing messages to the console.
#' defaults to FALSE.
#' @return invisibly returns a list for downstream use in [check_feature()]
#' 
#' @details 
#' If `x` is the output of `\link{prepare_ml}()`, the tibble `x$data$prep$train` is checked for
#'  factors with at least one low frequency class. 
#'  Please refer to the package vignette for further details.
#' 
#' @export
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)


check_freq <- function(
    x,
    thres = NULL,
    quiet = FALSE
){
  
  thres <- thres %||% get_default(step_other2, "threshold")
  if(thres < 1) thres <- floor(thres * nrow(x))
  
  # TODO move to check_feature, keep check_*() for df
  # output of `\link{prepare_ml}()` or a tibble 
  # data_check <- if(inherits(x, "martini_ml")) {
  #   x$data$prep$train
  # }else if (is.null(x)) {
  #     cli::cli_abort("Please check your input. Currently NULL.")
  #   }else if (!is.data.frame(x)) {
  #     cli::cli_abort("Please check your input. Should be a data frame.")
  #   }else{
  #   x
  # }
  if (is.null(x)) {
    cli::cli_abort("Please check your input. Currently NULL.")
  }else if (!is.data.frame(x)) {
    cli::cli_abort("Please check your input. Should be a data frame.")
  }
  
  data_check <-x
  
  # determine all class distributions
  fct_counts <- data_check %>% 
    dplyr::select(dplyr::where(is.factor), dplyr::where(is.character)) %>% 
    purrr::map(~{
      tibble::tibble(fct = .x) %>% 
        dplyr::count(fct, sort = TRUE)
    }) 
  
  # determine size of smallest class per factor
  min_counts <- fct_counts %>% 
    purrr::map_int(
      ~.x %>% dplyr::slice_min(n) %>%  
        dplyr::pull(n) %>% 
        unique()
    )
  
  # get names of 'risky' factors
  risky <- purrr::keep(min_counts, ~{.x < thres}) %>% 
    names()
  
  # determine overall minimum of class sizes (named)
  overall_min <- if(length(fct_counts) == 0) {
    NA_integer_
  } else{
    min_counts %>% sort() %>% magrittr::extract(1)
  }
  
  
  out <- list(
    vars = risky,
    counts = fct_counts[risky], 
    overall_min = overall_min, 
    finding = length(risky) > 0, 
    threshold = thres, 
    check = "check_freq()"
  )
  
  if (!quiet) {
    if (length(risky) > 0 ) {
      cli::cli_inform(c(
        'i' = '{cli::qty(risky)}The following factor{?s} {?has/have} low frequencies (<{thres}) in at least one class: \n{risky}'
      )
      )
    } else{
      cli::cli_text(
        c("No factors with low frequency class (<{thres}) detected in data set.")
      )
      
    }
  } 
  
  invisible(out)
}


#' Check for occurrence of level to cause issue with lumping
#'
#' @param other2_class name of class to check for. 
#' If NULL (the default), uses the default of [step_other2()]'s argument
#' `other`.
#' @inheritParams check_freq 
#' 
#' @export
#' @inherit check_freq return
#'
check_other_class <- function(
    x,  
    other2_class = NULL, 
    quiet        = FALSE
){
  
  other2_class <- other2_class %||% get_default(step_other2, "other")
  
  cols_with_class_other2 <- x %>% 
    dplyr::select(
      dplyr::where(is.character), 
      dplyr::where(is.factor)
    ) %>% 
    purrr::map_lgl(~ any(.x == other2_class)) %>% 
    purrr::keep(isTRUE) %>% 
    names()
  
  
  if (length(cols_with_class_other2) > 0 && ! quiet) {
    
    cli::cli_inform(c(
      "Low frequency classes may be pooled during ML data prep into a class {other2_class} in {.fn prepare_ml}.",
      "i" = "Note that {other2_class} is already a value in column{?s} {cols_with_class_other2}.",
      "*" = "See {.fn step_other2} for details on downstream processing and modify your data as needed before proceeding."
    ))
    
  }
  
  counts <- x %>% 
    dplyr::select(tidyselect::any_of(cols_with_class_other2)) %>% 
    purrr::map(~{
      forcats::fct_count(.x, sort = TRUE) 
    }) 
  
  out <- list(
    vars = cols_with_class_other2,
    counts = counts, 
    finding = length(cols_with_class_other2) > 0,
    class = other2_class, 
    check = "check_other_class()"
  )
  
  invisible(out)
}


#' Check for the proportion of non-missing values 
#'
#' @param thres Minimum proportion of data available
#' @inheritParams check_freq
#'
#' @inherit check_freq return
#' 
#' @export
check_non_missing <- function(
    x,
    thres = NULL,
    quiet = FALSE
){
  
  thres <- get_default(prepare_ml, "thres_imp")
  high_miss <- x %>% 
    purrr::map_dbl(~ {(!is.na(.x)) %>% mean()}) %>% 
    purrr::keep(~{.x <= thres})
  
  
  if (length(high_miss) > 0 && ! quiet) {
    cli::cli_inform(c(
      paste0(
      "i" = "Variables with a high proportion of missing values ", 
        #"(default >{get_default(prepare_ml, 'thres_imp')*100}% non-missing) ", 
        "will be discarded instead of imputed during ML preparation."
      ),
      "!" = paste0(
        "For the tested threshold of {thres*100}% ",
        "{cli::qty(length(high_miss))}the following variable{?s} would be discarded: {names(high_miss)}."
      ),
      "*" = "See {.fun recipes::step_filter_missing} for details."
    )
    )
  }
  
  out <- list(
    vars = names(high_miss),
    prop_missing = high_miss, 
    finding = length(high_miss) > 0,
    threshold = thres, 
    check = "check_non_missing()"
  )
  
  invisible(out)
}


#' Check for variables that resemble count variables
#'
#' Identify variables that only have only (non-negative) integer values and a 
#' relatively small number of distinct values 
#'
#' @param thres number of distinct integer values 
#' @param non_neg logical controlling whether to only consider variables
#'  with non-negative values. Defaults to TRUE.
#' @inheritParams check_freq
#' 
#' @inherit check_freq return
#' 
#' @export
check_count <- function(
    x,
    thres   = NULL,
    non_neg = TRUE,
    quiet   = FALSE
){
  
  x_count <- x %>%
    dplyr::select(-tidyselect::any_of(".id")) %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    dplyr::mutate(dplyr::across(1:dplyr::last_col(), as.character))
  
  looks_like_count <- x_count %>%
    purrr::keep(~ {
      (readr::guess_parser(.x, guess_integer = TRUE) == "integer") &&
        ifelse(non_neg, all(.x >= 0, na.rm = TRUE), TRUE) &&
        dplyr::n_distinct(.x) <= thres
    }) %>%
    names()
  
  if (!quiet) {
    if(length(looks_like_count) > 0){
      cli::cli_inform(c(
        "i" = "{cli::qty(looks_like_count)}Data set {? contains a/contains} numeric variable{?s} with only positive integer values and few distinct values: {looks_like_count}.",
        "*" = "{cli::qty(looks_like_count)}Please check whether conversion to factor{?s} is appropriate."
      ))
    } else {
      cli::cli_inform(c(
        "i" = "Data set was checked for numeric variables that might need conversion to factor.",
        "v" = "None identified."
      ))
    }}
  
  out <- list(
    vars = looks_like_count,
    #n_distinct = high_miss, 
    finding = length(looks_like_count) > 0,
    threshold = thres, 
    check = "check_count()"
  )
  
  invisible(out)
  
}
