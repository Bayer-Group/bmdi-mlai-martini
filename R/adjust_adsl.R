#' Adjust spec object for data sets of type 'adsl'
#'
#' Helper function to make common adjustments to the spec object built by `\link{adam_spec}()`
#' for data sets of type 'adsl' to be used with the `%>%`. 
#'
#' @param spec spec object to modify
#' @param add character vector of columns to added (in comparison to automated selection) 
#' @param drop character vector of column names to be dropped
#' @param id name of list element to modify in the spec 
#' 
#' @return
#' A modified version of `spec` to be used as input to `\link{build}()`
#' 
#' @export
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @md
adjust_adsl <- function(
  spec, 
  add  = NULL,
  drop = NULL,
  id   = 'adsl'
){

  if (!id %in% names(spec)) usethis::ui_stop(
    crayon::magenta(
      paste0("No spec with the id ", usethis::ui_code(id), " available.") 
    )
  )
  
  if ("dict" %in% names(spec[[id]])){
    
    params <- spec[[id]][["dict"]][["param"]]
    
    add    <- intersect(params, add)
    drop   <- intersect(params, drop)
    
    spec[[id]][["dict"]] <- spec[[id]][["dict"]] %>% 
      dplyr::mutate(selected = dplyr::case_when(
        param %in% add  ~ TRUE,
        param %in% drop ~ FALSE,
        TRUE            ~ selected
      ))
    
  }
  
  spec[[id]][["adjustments"]] <- list(add = add, drop = drop)
  
  spec[[id]][["select"]] <- c(spec[[id]][["select"]], add) %>% 
    unique() %>% 
    setdiff(drop)
  
  spec
  
}



