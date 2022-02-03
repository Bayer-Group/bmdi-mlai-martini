#' Extract filter info from a spec object
#' 
#' Extract applied filters and (optionally) compare to reference set
#'
#' @param spec spec object as returned by \code{\link{adam_spec}()}
#' @param filter if not NULL (default), applied filters are compared against a reference set, 
#' identifying filters that cannot be applied to the data without an error
#'
#' @return 
#' List of applied filters by data set is printed to the console. 
#' If `filter` is applied, missing filters are listed, if any are identified.
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @export

info_filter <- function(
  spec, 
  filter = NULL
  
  # filter <- c("ITTFL  = 'Y'", "ITTFL  == 'Y'", "AVISIT == 'Baseline'", "MHOCCUR != 'N'"  , "MHSTDY < 0 | is.na(MHSTDY)")
  ){
  
  # check martini_spec
  
  
  # compute ####
  applied <- spec %>% map('filter')
  
  if(!is.null(filter)){ 
    missing <- filter %>% setdiff(applied %>% unlist() %>% unique())
  }    
  
  # output ####
  
  print_msg <- ''
  # discarded filters
  if(!is.null(filter)){
    if(length(missing)>0){
      print_msg <- paste(print_msg, 
        paste0(
         '\nThe following filter(s) could not be applied, please double check \n  - ',
         paste(missing, collapse = '\n  - '),
         '\n'
        )
      )
    }else{
      print_msg <- paste(print_msg, 
         '\nEach filter was applied at least once.\n'
      )
    }
  }
  
  # applied filters
  if(length(applied %>% unlist() %>% unique()) > 0){ 
    txt_applied <- tibble::tibble(
      name   = names(applied), 
      filter = purrr::map_chr(applied, ~paste(.x, collapse = ', '))
    ) %>%  
      mutate_at('name', ~crayon::col_align(paste0(.x, ':'), width = max(nchar(.x))+1)) %>% 
      unite(txt, name, filter, sep = ' ') %>% 
      pull(txt) %>%  paste(collapse = '\n  - ')
    
    print_msg <- paste(print_msg,
      paste0(
        '\nThe following filter(s) could be applied \n  - ',
        txt_applied,
        '\n\n'
      )
    )
  }
  
  usethis::ui_info(print_msg)
  
  
  # any_error? 
  #error_lgl <- individual %>%  map('is_error') %>%  unlist()
  #if(any(error_lgl)){ 
  #  filter_error <- error_lgl %>% which(.) %>%  names()
  #  usethis::ui_info(paste0(
  #    'The following filter(s) could not be applied, please double check \n  - ',
  #    paste(filter_error, collapse = '\n  - ')
  #  ))
  #}
  
}