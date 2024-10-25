#' Guess role columns for spec
#'
#' @param role the spec role of interest
#' @param type either `bds` or `occds`. defaults to `bds`
#' @param colnames_data
#'
#' @return
#' The guessed column name or NULL

adam_guess <- function(
    role,
    type = c("bds", "occds"),
    colnames_data
  ){
  
  type <- rlang::arg_match(type)
  
  if (type == "bds") {

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
    
  }else if (type == "occds") {
    
    guesses_occds <- list(
      label = c(
        "BDG01", "SCL01C", "CL01C", "DRUGRP1",
        "HLGT", "HLT", "BODSYS", "SOC", "DECOD", "CAT"
      ),
      time = c("STDY")
    ) %>% 
      purrr::map(~{paste0(.x, "$") %>% paste(collapse = "|")})
    
    actual_guess <- if (!is.null(guesses_occds[[role]])) {
      # in contrast to 'bds', column names in guess list are not 
      # necessarily exact but might have a data set specific prefix
      stringr::str_subset(
        string  = colnames_data, 
        pattern = guesses_occds[[role]]
      ) %>% 
        head(1)
    }else{
      NULL
    }
    
  }
  
  if (length(actual_guess) == 0) {
    return(NULL)
  }
  
  actual_guess
  
}


