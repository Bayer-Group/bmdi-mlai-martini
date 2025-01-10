#' Adjust spec object
#'
#' Helper function to make adjustments to the spec object built by 
#' `\link{adam_spec}()` to be used with the pipe. 
#' For example, update of entries for 
#' `param`, `label`, `dupl_ctrl` (bds)
#'  `fct_levels`, `spec_id`, `trt`, `id`, (adsl)
#'  `valuen`, `value`, `count` (occds)
#'
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
  
  stopifnot(inherits(spec, what =  "martini_spec"))
  
  modifications <- list(...)
  if(length(modifications) == 0) return(spec)
  
  # CHECKS
  #modifications <- check_adjust(modifications, spec = spec, id = id, append = append)
 
  
  
  if (!append){
    
    spec[[id]][names(modifications)] <- list(NULL)
    
    spec[[id]] <- spec[[id]] %>% purrr::list_modify(!!! modifications)
    
  }else{
    
    for (i in names(modifications)){
      
      spec[[id]][[i]] <- append(spec[[id]][[i]], modifications[[i]])
      
    }
    
  }
  
  # update data_info and filters if possible
  if(!is.null(spec[[id]][["data"]])){

    if('filter' %in% names(modifications)){
      
      # re-check filters
      keep_filter <- check_filter(
        spec[[id]][["data"]], 
        spec[[id]][["filter"]], 
        data_id = id
        )$individual %>% 
        purrr::map_lgl("keep") %>% 
        as.logical()
      spec[[id]][["filter"]] <- spec[[id]][["filter"]][keep_filter]
    }
    
    # update dict and data_info
    spec[[id]][['dict']]      <- create_dict(spec[[id]])
    spec[[id]][["data_info"]] <- data_info(spec[[id]])
    
  }else{
    # if not data attached: message
    attr(spec, 'data_info_ok') <- FALSE
    
    if('filter' %in% names(modifications)){
      
      usethis::ui_todo(paste(
        'Please specify all required filters in `adam_spec()` to ensure proper filter checks.'
      ))
      
      attr(spec, 'filter_ok') <- FALSE
      
    }
    
    if(any(c('param', 'label') %in% names(modifications))) {
      
      usethis::ui_todo(paste(
        'Please specify all key columns (such as param, label) in `adam_spec()` to obtain an accurate dictionary.'
      ))
      
      attr(spec, 'filter_ok') <- FALSE
      
    }
  }

  spec
  
}


# COMBAK
# check keys are actual spec parameters
# for all column related changes: check if is in names(data)
# ? how to check values_fn
# adsl: factor levels are values?
# md5 sum nicht überschreiben

#' check key value pair inputs for adjust_* functions
#'
#' @param modifications 
#' @param spec spec object to modify
#' @param id name of list element to modify in  `spec`
#' @param append logical. If TRUE, modifications are appended,
#'  otherwise overwritten. 
#'
#' @return subset of modifications that are valid to apply
#' 
check_adjust <- function(modifications, spec, id, append){
  
  # check: append applicable ####
  if(append){
    
    
    if(!all(names(modifications) %in% names(spec[[id]]))){
      usethis::ui_info(paste0(
        'The following entries are not present in the spec object for id ', id,
        ' and will be ignored: ', 
        paste(setdiff(names(modifications), names(spec[[id]])), collapse = ', ')
      ))
      
      modifications <- modifications %>% purrr::discard_at(setdiff(names(modifications), names(spec[[id]])))
    }
  }
  
  
  # check: slots to be prevented from manual adjustments ####
  # if(FALSE){
  #   tibble::lst(adam_spec_adsl, adam_spec_bds, adam_spec_occds) %>%
  #     purrr::map(~formals(.x) %>% names())
  #   martini_spec %>% purrr::map(names)
  # }
  protected <- c("file", "data", "md5", "size", "data_info")
  
  if(any(names(modifications) %in% protected)){
    ignore <- names(modifications) %>% intersect(protected)
    usethis::ui_info(paste0(
      'The following slots are protected and should not be adjusted manually: ', 
      paste(ignore, collapse = ', ')
    ))
    
    modifications <- modifications %>%
      purrr::discard_at(ignore)
  }
  
  
  # check: entry must already exist in spec entry ####
  if(!all(names(modifications) %in% names(spec[[id]]))){
    
    not_present <- setdiff(names(modifications), names(spec[[id]]))
    usethis::ui_info(paste0(
      'No entr{?y/ies} called ', 
      paste(not_present, collapse = ', '), 
      ' in the spec object for id ', id,
      '. Specified modifications will be ignored.'
    ))
    
    modifications <- modifications %>%
      purrr::discard_at(not_present)
    
    
  }
  
  # check: column names correct ####
  entries_colnames <- c(
    # bds
    "param", "label", "unit", "time",
    # adsl
    "spec_id", "trt", "id",
    # occds
    "valuen", "value"
  )
  
  entries_check <- intersect(
    names(modifications),
    entries_colnames
  ) %>% purrr::set_names()
  
  if(length(entries_check) > 0){
    
    check_format <- purrr::map_lgl(entries_check, ~{
      is.character(modifications[[.x]]) && length(modifications) == 1
    })
    wrong_format <- names(check_format)[!check_format]
    
    cli::cli_info(c(
      'The following entries must be character vectors of length 1 and will be ignored: ',
      paste(wrong_format, collapse = ', ')
    ))
    modifications <- modifications %>% purrr::discard_at(wrong_format)
    
    if(!is.null(spec[[id]]$data)){
      
      cols_missing <- intersect(
        names(modifications),
        entries_colnames
      ) %>% 
        purrr::map_lgl(~{
          !modifications[[.x]] %in% names(spec[[id]]$data)
        }) %>% 
        purrr::keep(~ .x) %>% 
        names()
      
      if(length(cols_missing) > 0) {
        usethis::ui_info(paste0(
          'The columns specified for the following entries are not present ',  
          'in the data object and will be ignored: ',
          paste(cols_missing, collapse = ', ')
        ))
        modifications <- modifications %>% purrr::discard_at(cols_missing)
      }
    }
  }
  
  # for dupl_ctrl: check is list with names values_fn and arrange
  if(dupl_ctrl %in% names(modifications)){
    
    check_dupl_ctrl <- is.list(modifications[[dupl_ctrl]]) &&
      # full list needs to be provided or subentries?
      setequal(c('values_fn', 'arrange'), names(modifications[[dupl_ctrl]])) 
    
    # check: arrange: char length 1 / a column name
    # values_fn: function(name)
    
    
  }
  
  modifications
} 