#' Identify factors with low frequency classes
#'
#'
#' @param x output of `\link{prepare_ml}()` or a tibble 
#' @param thres integer. Factors with at least one class of size smaller than `thres` will be identified. Defaults to 10.
#'
#' @return
#' In case factors are present in the data set that meet the condition of interest, 
#' relevant names are printed in the console and a list of frequency tables is returned.
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
  thres = 10
){
  
  # TODO replace by checking for ml_object class once defined
  is_ml <- !is.data.frame(x)
  
  data  <- x
  if(is_ml) {data <- data$data$prep$train}
  
  if(is.null(data)){
    cli::cli_abort("Please check your input. Currently NULL.")
  }
  
  d_fct <- data %>% 
    dplyr::select_if(is.factor)
    
  if(ncol(d_fct) == 0){
    
    usethis::ui_info('Data does not contain any factors.')
    return(invisible(NULL))
    
  }
  
  # determine size of smallest class per factor
  min_count <- purrr::map_int(d_fct, ~{
    
    tibble::tibble(fct = .x) %>% 
      dplyr::count(fct) %>% 
      dplyr::slice_min(n) %>%  
      dplyr::pull(n) %>% 
      unique()
    
  })
  
  # get names of 'risky' factors
  risky <- names(min_count)[as.numeric(min_count) < thres]
  
  # determine overall minimum of class sizes
  overall_min <- min(min_count)
  
  if(length(risky) == 0){
    
    usethis::ui_info(paste0('Minimum observed class size is ', usethis::ui_value(overall_min), '.'))
    return(invisible(NULL))
    
  }
  
  
 # build message with details about number and names of potentially problematic factors
  mess <- c(
    paste0(
      "Data contains {length(risky)} factor{?s} with less than {thres} observations in at least one class",
    
      ifelse(
        !is_ml, 
        '.', 
        ' which may cause downstream problems. Changing the seed for test/train split and/or the modelling may solve potential issues.'
      )
    ),
    
    'i' = 'The following factor{?s} {?has/have} low frequencies in at least one class: \n{risky}'
  )
  cli::cli_inform(mess)
  
  d_fct %>% 
    dplyr::select(tidyselect::any_of(risky)) %>% 
    purrr::map(~{
      
      tibble::tibble(fct = .x) %>% 
        dplyr::count(fct) %>% 
        dplyr::arrange(n)
      
    })
  
}
