#' Prepare ML helper function
#'
#' Identify variable sets from input matrix that might require extra steps in data preparation,
#' e.g. skewed variables to be log transformed, counts
#'
#' @param data the data set to be searched for feature sets with specific characteristics relevant for further data preparation
#' @param thres_count used to detect integer columns with up to \code{thres_count} distinct values (might be excluded from further processing, e.g. log & normalization) 
#' @param thres_log threshold for log transformation
#' @param thres_lump proportion threshold for factor lumping; used to detect factors with exactly one level having a relative frequency below \code{thres_lump}
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
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
      
prepare_ml_vars <- function(

  data,
  thres_count      = NULL,
  thres_log        = NULL, 
  thres_lump       = NULL,
  remove           = c(".id", ".out", ".status", ".time")

){
  
  # vars_count: identify integers with only a limited number of values ####
  if (is.null(thres_count)){
    vars_count <- NA
  } else {
    vars_count <- NULL
    
    # TODO simplify using type_convert(guess_integer = TRUE)
    vars_integer <- data %>%  
      dplyr::select(-tidyselect::any_of(remove)) %>% 
      dplyr::mutate_if(is.factor, as.character) %>% 
      purrr::map_lgl( 
        ~{ readr::guess_parser(.x, guess_integer = TRUE) == 'integer'}
      ) %>% 
      which(.) %>%  
      names()
    if (length(vars_integer)>0){
      vars_count <- data %>% 
        dplyr::mutate_if(is.factor, as.character) %>% 
        dplyr::mutate_if(is.numeric, as.character) %>% 
        dplyr::select_if( ~{readr::guess_parser(.x, guess_integer = TRUE) == 'integer'} ) %>%  
        
        tidyr::pivot_longer(
          -tidyselect::any_of(remove), 
          names_to = "paramcd", values_to = "aval"
        ) %>% 
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
        tidyr::pivot_longer(
          -tidyselect::any_of(remove), 
          names_to       = "paramcd",
          values_to      = "aval",
          values_drop_na = TRUE
        ) %>% 
        dplyr::group_by(paramcd) %>% 
        dplyr::mutate(min_aval = min(aval)) %>% 
        dplyr::filter(min_aval > 0) %>% 
        dplyr::summarise(skew = skw(aval, na.rm = TRUE), .groups = "drop") %>% 
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
  
  # output ####
  list(
    count    = vars_count,
    log      = vars_log,
    nolump   = vars_nolump
  )
  
  
}


#' consistent renaming of character vectors/factor levels
#'
#' @param x character vector
#'
#' @return
#' list with the updated x, obtained from call `stringr::str_replace_all(x, replacement)`,
#' where replacement is returned as separate same-name list entry
#' 
#' @export
#'
prepare_replace <- function(
    x = NULL
){
  
  # TODO parametrize later with checks
  replacement = NULL
  
  if(is.null(replacement)){
    # order matters!
    replacement <- c(
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
    )}
  
  tibble::lst(
    x  = forcats::fct_relabel(x, ~ stringr::str_replace_all(.x, replacement)),
    replacement
  )
  
}

#' prep feature matrix 
#'
#' @param feature feature tibble
#' @param vars_fct_expl_na defaults to NULL
#' @param level_other defaults to 'other'
#'
#' @details 
#' `r lifecycle::badge('deprecated')`
#' Deprecated since update in \code{prepare_ml()} as of martini 0.6.0
#' 
#' @return updated feature matrix
#'
#' 
prepare_ml_feature <- function(
    feature,
    vars_fct_expl_na = NULL,
    level_other = 'other'
){

  lifecycle::deprecate_soft(
    when = "0.7.0",
    what = "prepare_ml_feature()", 
    details = "Please reach out to the authors if you find yourself using this function."
  )
  
  # TODO !! remove. not needed in prepare_ml() anymore, but still used within a project as standalone function
  
  # ... transform all character columns into factors (strips labels) ####
  feature <- feature %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character) & !.id, factor)) %>% 
    dplyr::mutate_if(is.factor, ~{prepare_replace(.x)$x}) %>% 
    # add explicit NAs to selected factor variables (optional)
    {if(!is.null(vars_fct_expl_na)){
      dplyr::mutate_at(., vars_fct_expl_na, ~ fct_na_to_level(.x, level = "missing"))
    }else{.}
    }
  
  
  # consistent handling of factors with level other ####
  if(!is.null(level_other)){
    # ... identify columns with `level_other` level (e.g. 'Other', case insensitive)
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
  }
  
  feature
  
}


# consistent handling of factors with level other ####
# TODO docu
# deprecated
prepare_ml_other <- function(
    x,
    level_other = 'other_ml'
    
){
  
  if(! is.factor(x) ){
    return(x)
  } else{
  
    if(!is.null(level_other)){
      
      levs <- levels(x)
      
      # ... identify columns with `level_other` level (e.g. 'Other', case insensitive)
      has_other <- stringr::str_to_lower(level_other) %in% stringr::str_to_lower(levs)
      
      other_rename <- c(
        stringr::str_to_title(level_other),
        stringr::str_to_upper(level_other),
        stringr::str_to_lower(level_other)
      ) %>%  
        intersect(levs) %>% 
        rlang::set_names(level_other)
      
      x <- forcats::fct_recode(x, !!! other_rename)
      
    }
    
    x
  # if(!is.null(level_other)){
  #   # ... identify columns with `level_other` level (e.g. 'Other', case insensitive)
  #   vars_with_other <- feature %>% 
  #     purrr::map_lgl(~{any(stringr::str_to_lower(.) == stringr::str_to_lower(level_other))}) %>% 
  #     which() %>% 
  #     names()
  #   
  #   if(length(vars_with_other) > 0){
  #     feature <- feature %>% 
  #       dplyr::mutate_at(vars_with_other, ~{
  #         if (stringr::str_to_title(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_title(level_other))
  #         if (stringr::str_to_upper(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_upper(level_other))
  #         if (stringr::str_to_lower(level_other) %in% levels(.x)) forcats::fct_recode(.x, !!sym(level_other) := stringr::str_to_lower(level_other))
  #       })
  #   }
  # }
  } 
}


