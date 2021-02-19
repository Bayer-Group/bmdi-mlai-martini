#' Guess time and label columns for subsequent use in adam_spec_occds()
#' 
#' For domains admh, adcm and adae, appropriate choices for the parameters \code{label} and \code{time} 
#' are returned for further use in \code{\link{adam_spec_occds}()}. 
#' 
#' @param file the file containing the data for which label, time (and value) columns need to be guessed
#' 
#' @description 
#' \code{\link{adam_guess}()} makes use of the internal function \code{\link{adam_domain_type}()} to map 
#' the domain type to the file name.
#' The set of domains for which guessing options are available will be extended in the future if required.
#' 
#' @return If guessing options are available, a list will be returned with possible options for 
#' label, time and value columns, ranked by relevance. 
#' These may be intersected with the actual column names of the data set at 
#' a later point to determine the input for `adam_spec_*()`.
#' The function will exit for domains without guessing options.
#' 
#' @seealso \code{\link{adam_spec_occds}()}
#'
#' @section Authors
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export 

adam_guess <- function(file){
  
  # ... check file exists ###
  if( ! file.exists(file) ){
    usethis::ui_stop(paste0(
      crayon::silver( "The provided file does not exist. \n\t "), 
      crayon::blue(file)
    ))
  }
  
  file_info <- adam_domain_type(file)
  # adam_domain_type() returns domain name and the mapped domain type 
  # depending on the type, further distinction by domain may or may not be required (occds vs bds)

  
  # OCCDS  ####
  # if (file_info$type == "occds"){
  # occds_time <- paste0( str_sub(file_info$domain, 3, 4)) %>%  str_to_upper(), 'STDY')
  
  if (        file_info$domain == "admh"){
      list(
        label  = paste0('MH', c("HLGT", "HLT", "BODSYS", "SOC", "DECOD")), 
                             #c("MHHLGT", "MHHLT", "MHBODSYS", "MHSOC", "MHDECOD"),
        time   = "MHSTDY", # occds_time
        value  = NULL
      )
    } else if(file_info$domain == "adcm"){
      list(
        label  = c("BDG01", "CMSCL01C", "CMCL01C", "DRUGRP1", "CMDECOD"),
        time   = "CMSTDY", # occds_time
        value  = NULL
      )
    } else if(file_info$domain == "adae"){
      list(
        label  = paste0('AE', c("HLGT", "HLT", "BODSYS", "SOC", "DECOD", "CAT")) , 
                             #c("AEHLGT", "AEHLT", "AEBODSYS", "AESOC", "AEDECOD", "AECAT"),
        time   = "AESTDY", # occds_time
        value  = NULL #AESERV
      )
    } else {
      usethis::ui_stop(
        paste0("No guessing options available for domain '", file_info$domain, "' yet.\n")
      )
    }

  #}
}

# tests 
if(FALSE){
  file <- "../../../adcm.sas7bdat"
  
  # works
  adam_guess(file)
  
  # file doesn't exist
  adam_guess(str_remove(file, 'Original/'))
  
  # nothing guessed in adam_domain_type
  adam_guess(str_replace(file, 'prod/adcm', 'prod/adpr'))
  
  # nothing guessed for label and time
  adam_guess(str_replace(file, 'prod/adcm', 'prod/advs'))
  
}


