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

  if (!append){
    
    spec[[id]][names(mod)] <- list(NULL)
    
    spec[[id]] <- spec[[id]] %>% purrr::list_modify(!!! mod)
    
  }else{
    
    for (i in names(mod)){
      
      spec[[id]][[i]] <- append(spec[[id]][[i]], mod[[i]])
      
    }
    
  }
  
  # update data_info and filters if possible
  if(!is.null(spec[[id]][["data"]])){

    if('filter' %in% names(mod)){
      
      # re-check filters
      keep_filter   <- check_filter(spec[[id]][["data"]], spec[[id]][["filter"]], data_id = id)$individual %>% 
        purrr::map_lgl("keep") %>% 
        as.logical()
      spec[[id]][["filter"]] <- spec[[id]][["filter"]][keep_filter]
    }
    
    # COMBAK adjust dict 
    
    # update data info
    spec[[id]][["data_info"]] <- data_info(spec[[id]])
    
  }else{
    # else message
    attr(spec, 'data_info_ok') <- FALSE
    
    if('filter' %in% names(mod)){
      
      usethis::ui_todo(paste(
        'Please specify all required filters in `adam_spec()` to ensure proper filter checks.'
      ))
      
      attr(spec, 'filter_ok') <- FALSE
      
    }
    
  }

  spec
  
}



