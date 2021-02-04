#' Guess time and label columns for subsequent use in adam_spec_occds()
#' 
#' For domains admh, adcm and adae, appropriate choices for the parameters \code{label} and \code{time} 
#' are returned for further use in \code{adam_spec_occds()}. 
#' 
#' @param file the file containing the data for which label and time columns need to be guessed
#' 
#' @description 
#' \code{adam_guess()} makes use of the internal function \code{adam_domain_type()} to map the domain type to the file name.
#' The set of domains for which guessing options are available will be extended in the future if required.
#' 
#' @return If guessing options are available, a list will be returned with possible options for label, time and value columns, ranked by relevance. 
#' These may be intersected with the actual column names of the data set at a later point to determine the input for \code{adam_spec_*()}.
#' The function will exit for domains without guessing options.
#' 
#' @seealso \code{\link{adam_spec_occds()}}
#'
#' @section Authors:
#' 
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @md

adam_guess <- function(file){
  
  file_info <- adam_domain_type(file)
  # adam_domain_type() returns domain name and the mapped domain type 
  # depending on the type, further distinction by domain may or may not be required (occds vs bds)
  
  
  # OCCDS  ####
  if (file_info$dom == "admh"){
      list(
        label  = c("MHHLGT", "MHHLT", "MHBODSYS", "MHSOC", "MHDECOD"),
        time   = "MHSTDY",
        value  = NULL
      )
    } else if (file_info$dom == "adcm"){
      list(
        label  = c("BDG01", "CMSCL01C", "CMCL01C", "DRUGRP1", "CMDECOD"),
        time   = "CMSTDY",
        value  = NULL
      )
    } else if (file_info$dom == "adae"){
      list(
        label  = c("AEHLGT", "AEHLT", "AEBODSYS", "AESOC", "AEDECOD", "AECAT"),
        time   = "AESTDY",
        value  = NULL #AESERV
      )
    } else {
      usethis::ui_stop(
        paste0("No guessing options available for domain '", file_info$dom, "' yet.\n")
      )
    }
 
  

}