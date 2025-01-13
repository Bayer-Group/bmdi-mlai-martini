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
#' @param entry name of list element to modify in the spec
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
    entry,
    ...
){
  
  stopifnot(inherits(spec, what =  "martini_spec"))
  
  if (!id %in% names(spec)) usethis::ui_stop(
    crayon::magenta(
      paste0("No spec with the id ", usethis::ui_code(id), " available.") 
    )
  )
  
  modifications <- list(...)
  if (length(modifications) == 0) return(spec)
  
  # CHECKS ----
  modifications <- check_adjust(modifications, spec = spec, id = id)
  if (length(modifications) == 0) return(spec)
  
  spec[[id]][names(modifications)] <- list(NULL)
  spec[[id]] <- spec[[id]] %>% purrr::list_modify(!!! modifications)
  
  # update data_info and filters if possible
  if (!is.null(spec[[id]][["data"]])) {
    
    # if('filter' %in% names(modifications)){
    #   
    #   # re-check filters
    #   keep_filter <- check_filter(
    #     spec[[id]][["data"]], 
    #     spec[[id]][["filter"]], 
    #     data_id = id
    #     )$individual %>% 
    #     purrr::map_lgl("keep") %>% 
    #     as.logical()
    #   spec[[id]][["filter"]] <- spec[[id]][["filter"]][keep_filter]
    # }
    
    # update dict and data_info
    spec[[id]][['dict']]      <- create_dict(spec[[id]])
    spec[[id]][["data_info"]] <- data_info(spec[[id]])
    
    attr(spec, 'data_info_ok') <- TRUE
    
  }else{
    
    attr(spec, 'data_info_ok') <- FALSE
    
    # if('filter' %in% names(modifications)){
    #   
    #   usethis::ui_todo(paste(
    #     'Please specify all required filters in `adam_spec()` to ensure proper filter checks.'
    #   ))
    #   
    #   attr(spec, 'filter_ok') <- FALSE
    #   
    # }
    # 
    # if(any(c('param', 'label') %in% names(modifications))) {
    #   
    #   usethis::ui_todo(paste(
    #     'Please specify all key columns (such as param, label) in `adam_spec()` to obtain an accurate dictionary.'
    #   ))
    #   
    #   attr(spec, 'filter_ok') <- FALSE
    #   
    # }
  }
  
  spec
  
}


#' check key value pair inputs for adjust_* functions
#'
#' @param modifications list of key value pairs defining adjustments
#' @param spec spec object to modify
#' @param entry name of list element to modify in `spec`
#'
#' @return subset of modifications that are valid to apply
#' 
check_adjust <- function(modifications, spec, entry){
  
  if("select" %in% names(modifications)){
    
    cli::cli_inform(c(
      "{.code select} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please rerun {.fun adam_spec} or use {.fun adjust_adsl_select()}."
    ))
    
    modifications <- modifications %>% purrr::discard_at("select")
    
  }
  
  if("id" %in% names(modifications)){
    # id column must be present in all data sets, needs to be checked globally 
    cli::cli_inform(c(
      "{.code select} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please rerun {.fun adam_spec} ."
    ))
    
    modifications <- modifications %>% purrr::discard_at("id")
    
  }
  
  protected <- c(
    "file", "data", "md5", "size", "data_info", 
    "dict", "spec_id", "drop_list", "flag_table"
  )
  
  if (any(names(modifications) %in% protected)) {
    
    ignore <- names(modifications) %>% intersect(protected)
    cli::cli_inform(c(
      "Some entries in the {.code spec} object are protected from being adjusted manually.",
      "i" = "You tried to adjust the following protected {cli::qty(length(ignore))} entr{?y/ies}: {ignore}.",
      "i" = "{cli::qty(length(ignore))}{?This/These} adjustment{?s} will be ignored."
    ))
    
    modifications <- modifications %>% purrr::discard_at(ignore)
    
  }
  
  if ("filter" %in% names(modifications)) {
    
    cli::cli_inform(c(
      "{.code filter} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please rerun {.fun adam_spec} or use {.fun adam_spec_filter()}."
    ))
    
    modifications <- modifications %>% purrr::discard_at("filter")
    
  }
  
  # check: entry must already exist in spec entry ####
  if (!all(names(modifications) %in% names(spec[[entry]]))) {
    
    not_present <- setdiff(names(modifications), names(spec[[entry]]))
    cli::cli_warn(c(
      "Only existing entries can be adjusted.",
      "i" = "You tried to adjust the following {cli::qty(length(not_present))} entr{?y/ies}: {not_present}.",
      "!" = "{cli::qty(length(not_present))}{?This/These} adjustment{?s} will be ignored.",
      "*" = "Please check your adjustment instructions."
    ))
    
    modifications <- modifications %>% purrr::discard_at(not_present)
    
  }
  
  # check: column names correct ####
  entries_colnames <- c(
    # bds
    "param", "label", "unit", "time",
    # adsl
    "trt", "id",
    # occds
    "valuen", "value"
  )
  
  entries_check <- intersect(
    names(modifications),
    entries_colnames
  ) %>% purrr::set_names()
  
  if (length(entries_check) > 0) {
    
    # ... format ####
    check_format <- purrr::map_lgl(entries_check, ~{
      rlang::is_character(modifications[[.x]], n = 1)
    })
    wrong_format <- names(check_format)[!check_format]
    
    cli::cli_info(c(
      "Entries that specify column names in the data must be character vectors of length 1.",
      "i" = "The following {cli::qty(length(wrong_format))} entr{?y/ies} {?is/are} of the wrong format: {wrong_format}.",
      "!" = "{cli::qty(length(wrong_format))}{?This/These} adjustment{?s} will be ignored.",
      "*" = "Please check your adjustment instructions."
    ))
    
    modifications <- modifications %>% purrr::discard_at(wrong_format)
    
    # ... availability of remaining columns####
    if(!is.null(spec[[entry]]$data)){
      
      cols_not_in_data <- intersect(
        names(modifications),
        entries_colnames
      ) %>% 
        purrr::map_lgl(~{
          !modifications[[.x]] %in% names(spec[[entry]]$data)
        }) %>% 
        purrr::keep(~ .x) 
      
      if (length(cols_not_in_data) > 0) {
        
        cli::cli_warn(c(
          "Specified column names must be present in the attached data.",
          "i" = paste(
            "The following {cli::qty(length(cols_not_in_data))}",
            "column{?s} {?is/are} not present in {.code data}:",
            "{cols_not_in_data}."
          ),
          "!" = paste(
            "{cli::qty(length(cols_not_in_data))}{?This/These}", 
            "adjustment{?s} will be ignored."
          ),
          "*" = "Please check your adjustment instructions."
        ))
        
        modifications <- modifications %>% purrr::discard_at(cols_not_in_data)
      }
    }
  }
  
  # dupl_ctrl ####
  # for dupl_ctrl: check is list with names values_fn and arrange
  if ("dupl_ctrl" %in% names(modifications)) {
    
    check_dupl_ctrl <- is.list(modifications[["dupl_ctrl"]]) &&
      # full list needs to be provided
      setequal(c('values_fn', 'arrange'), names(modifications[["dupl_ctrl"]])) 
    
    if (!check_dupl_ctrl) {
      
      cli::cli_warn(c(
        "{.code dupl_ctrl} must be a list of length 2 with entries {.code values_fn} and {.code arrange}.",
        "!" = "Adjustment will beignored.",
        "*" = "Please check your adjustment instructions."
      ))
      
      modifications <- modifications %>% purrr::discard_at("dupl_ctrl")
      
    }else{
      
      # check: arrange: char length 1 / a column name
      # values_fn: function(name)
      
      # check if values_fn is a function
      check_passed_valuesfn <- rlang::is_function(modifications[[dupl_ctrl]]$values_fn)
      # NOTE check if output length is 1 (works for either num or categorical)
      if (!check_passed_valuesfn) {
        
        cli::cli_warn(c(
          "{.code dupl_ctrl$values_fn} must be a function.",
          "!" = "Adjustment of {.code dupl_ctrl} will be ignored.",
          "*" = "Please check your adjustment instructions."
        ))
        
      }
      
      # check if arrange has length 1
      # TODO if data is available, check if arrange can be applied without error
      check_passed_arrange <- length(modifications[[dupl_ctrl]]$arrange) == 1
      if (!check_passed_arrange) {
        
        cli::cli_warn(c(
          "{.code dupl_ctrl$arrange} must be a character vector.",
          "!" = "Adjustment of {.code dupl_ctrl} will be ignored.",
          "*" = "Please check your adjustment instructions."
        ))
        
      }
      
      if (!all(check_passed_valuesfn, check_passed_arrange)) {
        modifications <- modifications %>% purrr::discard_at("dupl_ctrl")
      }
      
    }
    
  }
  
  modifications
  
} 


#' @title Adjust spec object filter 
#' 
#' @description
#' Helper function to make adjustments to the filter of the spec object built by
#' `adam_spec()` to be used with the pipe.
#' 
#' @param spec `martini_spec` object to modify
#' @param filter character vector of filter conditions
#' @param append logical, if TRUE, append filter to existing filter(s), else replace
#' 
#' @details
#' The function checks if the filter can be applied to the data attached to the spec.
#' If the data is not attached, the filter will be added to the spec as is.
#' 
#' @return A modified version of `spec` to be used as input to `build()`
#' @export

adjust_spec_filter <- function(spec, filter, append = TRUE){
  
  stopifnot(inherits(spec, what =  "martini_spec"))
  
  purrr::walk(names(spec), ~{
    
    if (append) {
      filter_update <- c(spec[[.x]][["filter"]], filter)
    }else{
      filter_update <- filter
    }
    
    
    if (!is.null(spec[[.x]][["data"]])) {
      
      # re-check filters
      keep_filter <- check_filter(
        spec[[.x]][["data"]],
        filter_update,
        data_id = entry
      )$individual %>%
        purrr::map_lgl("keep") %>%
        as.logical()
      spec[[.x]][["filter"]] <<- filter_update[keep_filter]
      
      attr(spec, "filter_ok") <- TRUE
      
      spec[[.x]][["dict"]]      <<- create_dict(spec[[.x]])
      spec[[.x]][["data_info"]] <<- data_info(spec[[.x]])
      
      attr(spec, "data_info_ok") <- TRUE
    }else{
      spec[[.x]][["filter"]] <<- filter_update
      
      attr(spec, "filter_ok")    <- FALSE
      attr(spec, "data_info_ok") <- FALSE
    }
    
  })
  
  any_data_missing <- any(purrr::map_lgl(spec, ~{is.null(.x[["data"]])}))
  
  if(any_data_missing){
    cli::cli_inform(c(
      "Can't check {.code filter} if {.code data} is not attached.",
      "i" = "At least one entry of {.code spec} does not have the data attached.",
      "i" = "The specified {.code filter} will be added to the spec as is.",
      "*" = "Please rerun `adam_spec()` with the additional filters for proper checks."
    ))
  }
  
  spec
  
}
