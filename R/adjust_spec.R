#' Adjust spec object
#'
#' Helper function to make adjustments to the spec object built by `\link{adam_spec}()` to be used with the `%>%`. 
#'
#' @param spec spec object to modify
#' @param id name of list element to modify in the spec
#' @param append boolean. If TRUE, modifications are appended, otherwise overwritten. defaults to FALSE.
#' @param ... modifications to the `spec[[id]]` of the form `<name> = <value>`.
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
adjust_spec <- function(
  spec, 
  id,
  ..., 
  append = FALSE
){
  
  mod <- list(...)
  
  if('filter' %in% names(mod)){
    usethis::ui_todo(paste(
      'Please specify all required filters in `adam_spec()` to ensure proper filter checks.'
    ))
  }

  if (!append){
    
    spec[[id]][names(mod)] <- list(NULL)
    
    spec[[id]] <- spec[[id]] %>% purrr::list_modify(!!! mod)
    
  }else{
    
    for (i in names(mod)){
      
      spec[[id]][[i]] <- append(spec[[id]][[i]], mod[[i]])
      
    }
    
  }
  
  spec
  
}



