#' Prepare ML helper function
#'
#' Identify variable sets from input matrix that might require extra steps in data preparation,
#' e.g. skewed variables to be log transformed, counts
#'
#' @param data the data set to be searched for feature sets with specific characteristics releveant for further data preparation
#' @param thres_count used to detect integer columns with up to \code{thres_count} distinct values (might be excluded from further processing, e.g. log & normalization) 
#' @param thres_log threshold for log transformation
#' @param thres_lump proportion threshold for factor lumping; used to detect factors with exactly one level having a relative frequency below \code{thres_lump}
#' @param thres_imp proportion threshold used to detect variables with non-missing proportion up to \code{thres_imp}
#' @param remove columns to be excluded from all identified sets; defaults to c(".id", ".out", ".status", ".time")
#'
#' @return 
#' 
#' A list with slots specifying the detected variable sets of interest. 
#' NA if required thresholds were not defined; NULL if no variables meet the corresponding criteria.
#' 
#' \item{count}{assumed to be counts}
#' \item{log}{to be log transformed as the skewness exceeds \code{thres_log}}
#' \item{nolump}{to be excluded from lumping}
#' \item{imp}{to be imputed}
#' \item{exclude}{to be excluded from the data as the proportion of missing values exceeds \code{thres_imp}}
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
      
prepare_ml_vars <- function(

  data,
  thres_count      = NULL,
  thres_log        = NULL, 
  thres_lump       = NULL,
  thres_imp        = NULL,
  remove           = c(".id", ".out", ".status", ".time", ".trt")

){
  
  
  # vars_count: identify integers with only a limited number of values ####
  if (is.null(thres_count)){
    vars_count <- NA
  } else {
    vars_count <- NULL
    vars_integer <- purrr::map_lgl(data, 
      ~{ guess_parser(.x, guess_integer = TRUE)== 'integer'}) %>% 
      which(.) %>%  names()
    if (length(vars_integer)>0){
      vars_count <- data %>% 
        dplyr::select_if( ~{readr::guess_parser(.x, guess_integer = TRUE) == 'integer'} ) %>%  
        tidyr::pivot_longer(-tidyselect::any_of(remove), 
                            names_to = "paramcd", values_to = "aval") %>% 
        dplyr::group_by(paramcd) %>% 
        dplyr::summarise(n_dist = dplyr::n_distinct(aval), .groups = "drop") %>% 
        dplyr::filter(n_dist <= thres_count) %>% 
        dplyr::pull(paramcd)
    }
    if (length(vars_count) == 0) vars_count <- NULL
  }

  
  # vars_log: identify skewed parameters -> logtrafo later in recipe  ####
  if (is.null(thres_log)){
    vars_log <- NA
  } else {
    vars_log <- NULL
    if (any(purrr::map_lgl(data, is.numeric))){
      vars_log <- data %>% 
        dplyr::select_if(is.numeric) %>% 
        tidyr::pivot_longer(-tidyselect::any_of(remove), 
                            names_to = "paramcd", values_to = "aval") %>% 
        dplyr::group_by(paramcd) %>% 
        dplyr::mutate(min_aval = min(aval)) %>% 
        dplyr::filter(min_aval > 0) %>% 
        dplyr::summarise(skew = e1071::skewness(aval, na.rm = TRUE), .groups = "drop") %>% 
        dplyr::filter(skew > thres_log ) %>% 
        dplyr::pull(paramcd) %>% 
        setdiff(vars_count)
      if (length(vars_log) == 0) vars_log <- NULL
    } 
  }
  
  # vars_nolump: factors to skip from step_other ####
  # if a single class falls below the threshold thres_lump, the class would be renamed to 'other'
  if (is.null(thres_lump)){
    vars_nolump <- NA
  } else {
    vars_nolump <- NULL
    if(! is.null(thres_lump)){
      vars_nolump <- data %>% 
        dplyr::select_if(is.factor) %>% 
        purrr::map_lgl( ~ { freqs <- table(.x)/ length(.x); sum(freqs < thres_lump) == 1  } )  %>% 
        which(.) %>% 
        names()
      if (length(vars_nolump) == 0) vars_nolump <- NULL
    }
  }

  
  
  # imputation/exclusion based on proportion available ####
  # ... prop_available: calculate proportion of missing values per column ####
  prop_available <- data %>% 
    purrr::map_dbl(~ mean(!is.na(.))) %>% 
    tibble::enframe()
  
  # ... vars_imp:     missing values will be knn imputed ####
  # ... vars_exclude: variables with a large number of missing values are excluded ####
  if (is.null(thres_imp)){
    vars_imp     <- NA
    vars_exclude <- NA
  } else {
    vars_imp <- prop_available %>% 
      dplyr::filter(value >= thres_imp & value < 1) %>% 
      dplyr::pull(name) %>% 
      setdiff(remove)
    vars_exclude <- prop_available %>% 
      dplyr::filter(value < thres_imp) %>% 
      dplyr::pull(name) %>% 
      setdiff(remove)
    if (length(vars_imp    ) == 0) vars_imp     <- NULL
    if (length(vars_exclude) == 0) vars_exclude <- NULL
  }


  # output ####
  list(
    count    = vars_count,
    log      = vars_log,
    nolump   = vars_nolump,
    imp      = vars_imp,
    exclude  = vars_exclude
  )
  
  
}

# tests
if (FALSE){
  
  # require(tidyverse)
  n <- 27
  set.seed(1909)
  
  data <- tibble::tibble(
    count   = sample(0:3, size = n, replace = TRUE),
    log     = rnorm(n) %>% exp(),
    nolump  = sample(c("a", "b"), size = n-1, replace = TRUE) %>% c("c") %>% factor(),
    imp     = rnorm(n-1) %>% c(NA),
    exclude = rep(NA, n-2) %>% c("a", "b") %>% factor(),
    .out    = log
  )
  
  prepare_ml_vars(
    data,
    thres_count = 4,
    thres_log   = 1.5,
    thres_lump  = 0.05,
    thres_imp   = 0.9
  )
 
  list(
    count   = "count", 
    log     = "log", 
    nolump  = "nolump", 
    imp     = "imp", 
    exclude = "exclude"
  )
   
}
