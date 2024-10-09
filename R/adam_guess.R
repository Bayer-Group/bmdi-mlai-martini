#' Guess time and label columns for subsequent use in adam_spec_occds()
#' 
#' For domains admh, adcm and adae, appropriate choices for the parameters \code{label} and \code{time} 
#' are returned for further use in \code{\link{adam_spec_occds}()}. 
#' 
#' @param file the file containing the data for which label, time (and value) columns need to be guessed
#' 
#' @details  
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
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'

adam_guess2 <- function(file){
  
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
        time   = "MHSTDY",
        value  = NULL
      )
    } else if(file_info$domain == "adcm"){
      list(
        label  = c("BDG01", "CMSCL01C", "CMCL01C", "DRUGRP1", "CMDECOD"),
        time   = "CMSTDY",
        value  = NULL
      )
    } else if(file_info$domain == "adae"){
      list(
        label  = paste0('AE', c("HLGT", "HLT", "BODSYS", "SOC", "DECOD", "CAT")),
        time   = "AESTDY",
        value  = NULL #AESERV
      )
    } else if(file_info$domain == "adxa"){  
      list(
        label  = paste0('XA', c( "DECOD")),
        time   = "XASTDY",
        value  = NULL #AESERV
      )
      
    } else {
      usethis::ui_stop(
        paste0("No guessing options available for domain '", file_info$domain, "' yet.\n")
      )
    }
}

# test area ####
if(FALSE){
  file <- "../../../adcm.sas7bdat"
  
  # works
  adam_guess(file)
  
  # file doesn't exist
  adam_guess(stringr::str_remove(file, 'Original/'))
  #testthat::expect_failure()
  
  # nothing guessed in adam_domain_type
  adam_guess(stringr::str_replace(file, 'prod/adcm', 'prod/adpr'))
  #testthat::expect_failure()
  
  # nothing guessed for label and time
  adam_guess(stringr::str_replace(file, 'prod/adcm', 'prod/advs'))
  
}

#' guess role columns for spec
#'
#'
#' @param role the spec role of interest
#' @param type either `bds` or `occds`. defaults to `bds`
#' @param colnames_data
#'
#' @return
#' 
adam_guess <- function(
    role,
    type = c("bds", "occds"),
    colnames_data
  ){
  
  type <- rlang::arg_match(type)
  
  if(type == "bds"){

    guesses_bds <- list(
      # candidates 'param'
      param = c('PARAMCD', 'PARAM'),
      
      # candidates 'time'
      time = c('AVISIT', 'AVISITN', 'VISIT', 'VISITN'),
      
      # candidates 'value'
      value =  c('AVAL', 'AVALC'),
      
      # candidates 'unit'
      unit = c('AVALU'),
      
      # label 
      label = c('PARAM', 'PARAMCD')
    )
    
    actual_guess <- intersect(
      guesses_bds[[role]],
      colnames_data
    ) %>% 
      head(1)
    
  }else if(type == "occds"){
    
    guesses_occds <- list(
      label = c(
        "BDG01", "SCL01C", "CL01C", "DRUGRP1",
        "HLGT", "HLT", "BODSYS", "SOC", "DECOD", "CAT",
      ),
      time = c("STDY")
    ) %>% 
      purrr::map(~{paste0(.x, "$") %>% paste(collapse = "|")})
    
    actual_guess <- stringr::str_subset(
      string  = colnames_data, 
      pattern = guesses_occds[[role]]
    ) %>% 
      head(1)
    
  }
  
  if(length(actual_guess) == 0){
    return(NULL)
  }
  
  actual_guess
  
}


