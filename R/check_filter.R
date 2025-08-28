#' Check filter
#'
#' Check a given (set of) filter(s) against a data set and assess whether or not a non-empty data set would be returned. 
#' 
#' @param data data set that the given filter(s) should be applied to
#' @param filter an expression to be used in \code{dplyr::filter()}, (either a single filter, or multiple ones separated by ',')
#' @param data_id character to include in warning message to help identify the data sets when used in \code{\link{adam_spec}()}. defaults to NULL.
#' 
#' @return 
#' A list with two entries: `individual` for single filter assessment,
#' `overall_norow` for the combined filter assessment, if all individually applicable filters are applied to `data`. 
#' `Individual` consists of a list of three logicals per filter, with values TRUE
#' if the application of the _individual_ filter to `data` yields
#' \describe{
#'   \item{is_error}{an error}
#'   \item{is_norow}{a tibble with 0 rows}
#'   \item{keep}{neither of the above}
#'  }
#' `overall_norow` is TRUE, if the combination of all applicable filters to `data` results in a 0-row tibble. In this case a warning is thrown.
#' 
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 

check_filter <- function(
  data, 
  filter,
  data_id = NULL){
  
  if(length(filter)>0){
    
    # check which filters are applicable for the data set
    individual  <- purrr::map(purrr::set_names(filter), function(x){
      
      # A. filter not applicable, WOULD throw an error if it was to be applied in build_* ####
      #    (e.g. column not present in given domain)
      #    this is NOT a suppressed error in the 'check_filter()' function, 
      #    but a desired side effect to select applicable filters (filter/domain match)
      #    and to prevent errors (e.g. typos in column names)
      try_it <- try(
        {data %>% dplyr::filter(!! rlang::parse_expr(x))},
        silent = TRUE
      )
      is_error <- "try-error" %in% class(try_it)
      
      # B. column present, resulting tibble has nrow = 0 ####
      is_norow <- FALSE
      if (!is_error) is_norow <- nrow(try_it) == 0
      
      
      list(
        keep     = !(is_error || is_norow),
        is_error = is_error,
        is_norow = is_norow
      )
      
      
    })
  
    # C. overall filter yields no rows  ####
    overall_norow <- logical(0)
    filter_keep   <- individual %>% purrr::map_lgl("keep")
    if(any(filter_keep)){
      overall_norow <- data %>% 
        dplyr::filter(!!! rlang::parse_exprs(filter[filter_keep])) %>% 
        nrow() %>% 
        {. == 0}
      
      # user note
      if(overall_norow){
        usethis::ui_warn(cli::bg_magenta(cli::col_white(
          paste0(
            ifelse(!is.null(data_id), paste0(data_id, ': '), ''), 
            'The combination of all applicable filters yields an empty tibble.'
          )))
        )
      }
    }
    # TODO: individual message filters 
    # currently only info they where not applied, without discrimination of yields-empty or throws-error)
    
    }else{
      
      individual <- list(
        list(
          keep     = FALSE,
          is_error = logical(0),
          is_norow = logical(0)
        )
      )
      overall_norow <- logical(0)
      
    }
    
    list(
      individual    = individual,
      overall_norow = overall_norow
    )
  
}
    






