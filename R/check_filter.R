#' Check filter
#'
#' Check a given (set of) filter(s) against a data set and assess whether or not a non-empty data set would be returned. 
#' 
#' @param data data set that the given filter(s) should be applied to
#' @param filter an expression to be used in \code{dplyr::filter()}, (either a single filter, or multiple ones separated by ',')
#' 
#' @return 
#' The function returns a logical of length equal to the number of individual filters present in the \code{filter} argument. 
#' TRUE indicates that the filter applied individually to \code{data} returns a data set with positive number of rows. 
#' On the other hand, FALSE indicates either an empty result after filtering or a non-applicable filter
#' (e.g. if the column used in the filter definition is not present in \code{data}).
#' 
#' @section Authors:
#'
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)


check_filter <- function(data, filter){
  
  # check which filters are applicable for the data set
  purrr::map_lgl(filter, function(x){
    
    # A. filter not applicable, WOULD throw an error if it was to be applied in build_*
    #    (e.g. column not present in given domain)
    #    this is NOT a suppressed error in the 'check_filter()' function, 
    #    but a desired side effect to select applicable filters (filter/domain match)
    #    and to prevent errors (e.g. typos in column names)
    try_it <- try(
      {data %>% dplyr::filter(!! rlang::parse_expr(x))},
      silent = TRUE
    )
    is_error <- "try-error" %in% class(try_it)
    
    # B. column present, resulting tibble has nrow = 0
    is_norow <- FALSE
    if (!is_error) is_norow <- nrow(try_it) == 0
    
    !(is_error || is_norow)
    
  })

}


# tests
if(FALSE){
  test_filter <- c('mpg > 0', 'DISP < 200')
  check_filter(mtcars, test_filter)
  #expected: TRUE, FALSE
}



