#' Extract filter info from a spec object
#' 
#' Extract applied filters and (optionally) compare to reference set
#'
#' @param spec spec object as returned by \code{\link{adam_spec}()}
#' @param filter if not NULL (default), applied filters are compared against a reference set, 
#' identifying filters that cannot be applied to the data without an error
#' @param quiet if TRUE instead of printing message to console, return list with messages on applied and discarded filters. defaults to FALSE.
#'
#' @return 
#' List of applied filters by data set is printed to the console. 
#' If `filter` is applied, missing filters are listed, if any are identified.
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 

info_filter <- function(
  spec, 
  filter = NULL,
  quiet  = FALSE
  
  # filter <- c("ITTFL  = 'Y'", "ITTFL  == 'Y'", "AVISIT == 'Baseline'", "MHOCCUR != 'N'"  , "MHSTDY < 0 | is.na(MHSTDY)")
  ){
  
  # check martini_spec
  
  
  # compute ####
  applied <- spec %>% purrr::map('filter')
  
  if(!is.null(filter)){ 
    missing <- filter %>% setdiff(applied %>% unlist() %>% unique())
  }    
  
  # build messages ####
  
  msg_discarded <- NULL
  msg_applied   <- NULL
  
  # discarded filters
  if(!is.null(filter)){
    if(length(missing)>0){
      msg_discarded <- paste0(
        crayon::blue('Please double check!') %>% crayon::bold(),
        '\n',
        'The following filter(s) could not be applied to any of the data sets',
        '\n  - ', paste(missing, collapse = '\n  - '),
        '\n\n'
      )
    }
  }
  
  # applied filters
  if(length(applied %>% unlist() %>% unique()) > 0){ 
    tibble_applied <- tibble::tibble(
      name   = names(applied), 
      filter = purrr::map_chr(applied, ~paste(.x, collapse = ', '))
    ) %>%  
      dplyr::mutate(filter = dplyr::case_when(
        filter == '' ~  '<none>',
        TRUE ~ stringr::str_squish(filter))
      ) 
    
    txt_applied <- tibble_applied %>% 
      dplyr::mutate_at('name', ~crayon::col_align(paste0(.x, ':'), width = max(nchar(.x))+1)) %>% 
      tidyr::unite(txt, name, filter, sep = ' ') %>% 
      dplyr::pull(txt) %>% paste(collapse = '\n  - ')
    
    msg_applied <- paste0(
      '\nThe following filter(s) could be applied \n  - ',
      txt_applied,
      '\n\n'
    )
    
  }
  
  # output messages ####
  
  if(quiet){
    plane <- list( 
      discarded = msg_discarded,
      applied   = msg_applied
    )
    return(invisible(plane))
    
  }else{
    
    if(!is.null(msg_discarded)){
      
      usethis::ui_oops(msg_discarded)
      
    } else if (!is.null(filter) && is.null(msg_discarded)){
      
      usethis::ui_done('\nEach filter may be applied at least once.\n')
      
    }
    
    if(!is.null(msg_applied)){
      cat('\n')
      usethis::ui_done(msg_applied)
      
    }
  }
  
}