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
  applied <- spec %>% purrr::map('filter')
  
  if(!is.null(filter)){ 
    missing <- filter %>% setdiff(applied %>% unlist() %>% unique())
  }    
  
  # build messages ####
  
  msg_discarded <- NULL
  msg_applied   <- NULL
  
  plane <- list()
  
  # discarded filters
  if(!is.null(filter)){
    if(length(missing)>0){
      tibble_discarded <- as_tibble(missing)
      
      msg_discarded <- paste0(
        crayon::blue('Please double check!') %>% crayon::bold(),
        '\n',
        'The following filter(s) could not be applied to any of the data sets',
        '\n  - ', paste(missing, collapse = '\n  - '),
        '\n\n'
      )
    }
  }else{
    tibble_discarded <- NULL
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
    
  }else{
    tibble_applied <- NULL
  }
  
  # output messages ####
  
  if(!is.null(msg_discarded)){
    
    usethis::ui_oops(msg_discarded)
    
  } else if (!is.null(filter) && is.null(msg_discarded)){
    
    usethis::ui_done('\nEach filter was applied at least once.\n')
    
  }
  
  if(!is.null(msg_applied)){
    
    usethis::ui_done(msg_applied)
    
  }

  #msgs <- paste(msg_applied, msg_discarded, collapse = '\n')
  
  
  plane <- list(
    discarded = tibble_discarded,
    applied   = tibble_applied
  )
  
  return(invisible(plane))
  
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