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
    
    attributes(spec, 'filter_ok') <- TRUE
    
  }

  if (!append){
    
    spec[[id]][names(mod)] <- list(NULL)
    
    spec[[id]] <- spec[[id]] %>% purrr::list_modify(!!! mod)
    
  }else{
    
    for (i in names(mod)){
      
      spec[[id]][[i]] <- append(spec[[id]][[i]], mod[[i]])
      
    }
    
  }
  
  if('filter' %in% names(mod)){

    attributes(spec, 'filter_ok') <- TRUE
    
    if(is.null(spec[[id]][["data"]])){
      
      usethis::ui_todo(paste(
        'Please specify all required filters in `adam_spec()` to ensure proper filter checks.'
      ))
      
    }else{
      
      # re-check filters
      keep_filter   <- check_filter(spec[[id]][["data"]], spec[[id]][["filter"]], data_id = id)$individual %>% 
        purrr::map_lgl("keep") %>% 
        as.logical()
      spec[[id]][["filter"]] <- spec[[id]][["filter"]][keep_filter]
      
    }
    
  }
  
  # update data info
  if(is.null(spec[[id]][["data"]])){
    
    attr(spec, 'data_info_ok') <- TRUE
    
  }else{
    
    # TODO refactor - used in all adam_spec_*() functions
      
    spec[[id]][["data_info"]] <- with(spec[[id]], 
      list(
        nsubj = data %>% 
          {if(length(filter) > 0){ 
            dplyr::filter(., !!! rlang::parse_exprs(filter))
          }else{.}} %>% 
          dplyr::select(tidyselect::all_of(id)) %>% 
          dplyr::n_distinct(),
        ncol  = dict %>% 
          {if(type == "adsl"){
            dplyr::filter(., selected)
          }else{.}} %>% 
          nrow()
      )
    )
    
  }

  spec
  
}



