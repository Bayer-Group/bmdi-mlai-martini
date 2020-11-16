

adam_guess <- function(file){
  
  file_info <- adam_domain_type(file)
  
  if (file_info$type == "none"){
    usethis::ui_stop(
      paste0("No guessing options available for domain '", file_info$dom, "'\n")
    )
  }
  
  if (file_info$type == "occds"){
    if (file_info$dom == "admh"){
      list(
        param  = c("MHHLGT", "MHHLT", "MHBODSYS", "MHSOC", "MHDECOD"),
        time   = "MHSTDY",
        value  = NULL
      )
    } else if (file_info$dom == "adcm"){
      list(
        param  = c("BDG01", "CMSCL01C", "CMCL01C", "DRUGRP1", "CMDECOD"),
        time   = "CMSTDY",
        value  = NULL
      )
    } else if (file_info$dom == "adae"){
      list(
        param  = c("AEHLGT", "AEHLT", "AEBODSYS", "AESOC", "AEDECOD", "AECAT"),
        time   = "AESTDY",
        value  = NULL #AESERV
      )
    }
  }
  
}