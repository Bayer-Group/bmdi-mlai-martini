#' Adjust column selection from data sets of type 'adsl' in spec object
#'
#' Helper function to make common adjustments to the spec object built by `\link{adam_spec}()`
#' for data sets of type 'adsl' to be used with the `%>%`. 
#'
#' @param spec spec object to modify
#' @param add,drop character vector of columns to add to/discard from
#' the automated selection that is stored in the `select` entry of the 
#' corresponding `spec` entry
#' @param select character vector of column names to be selected. if not NULL
#'  (the default), arguments `add` and `drop` will be ignored. 
#'  Overrides `select` entry of the corresponding `spec` entry.
#' @param id name of list element to modify in the spec, defaults to "adsl"
#' 
#' @details
#' if data is provided, the column names stored in the `select` slot will be 
#' intersected with actually existing column names. User will be informed in case of
#' misspecifications.
#' 
#' @return
#' A modified version of `spec` to be used as input to `\link{build}()`
#' 
#' @export
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
adjust_adsl_select <- function(
  spec, 
  add  = NULL,
  drop = NULL,
  select = NULL,
  id   = 'adsl'
){

  
  if (is.null(c(select, add, drop))) return(spec)
  
  # CHECKS ####
  if (!id %in% names(spec)) usethis::ui_stop(
    crayon::magenta(
      paste0("No spec with the id ", usethis::ui_code(id), " available.") 
    )
  )
  
  # TODO consider moving to helper functions
  length0_to_null <- function(x) if (length(x) == 0) NULL else x
  # COMBAK currently trt and id are also part of the select vector. Reconsider?
  # make sure that trt and id are not dropped
  drop <- setdiff(drop, c(spec[[id]][["id"]], spec[[id]][["trt"]])) %>% length0_to_null()
  
  if (!is.null(spec[[id]][["data"]])) {
    
    cols_not_in_data <- setdiff(
      unique(c(add, drop, select)),
      colnames(spec[[id]][["data"]])
    )
    
    if (length(cols_not_in_data) > 0) {
      
      cli::cli_warn(c(
        "Selected column names must be present in the attached data.",
        "i" = paste(
          "The following {cli::qty(length(cols_not_in_data))}",
          "column{?s} {?is/are} not present in {.code data}:",
          "{cols_not_in_data}."
        ),
        "!" = paste(
          "{cli::qty(length(cols_not_in_data))}{?This/These}", 
          "selection{?s} will be ignored."
        ),
        "*" = "Please check your adjustment instructions."
      ))
      
      select <- setdiff(select, colnames(spec[[id]][["data"]])) %>% length0_to_null()
      add    <- setdiff(add,    colnames(spec[[id]][["data"]])) %>% length0_to_null()
      drop   <- setdiff(drop,   colnames(spec[[id]][["data"]])) %>% length0_to_null()
      
    }
    
  }
  
  # UPDATES of the select and adjustment entries ####
  
  if (!is.null(select)) {
    
    if (!is.null(add) | !is.null(drop)) {
      usethis::ui_info(
        "Arguments `add` and `drop` will be ignored, as `select` is provided."
      )
      add  <- NULL
      drop <- NULL
    }

    spec[[id]][["adjustments"]] <- list(
      add  = setdiff(select, spec[[id]][["select"]]) %>% length0_to_null(),
      drop = setdiff(spec[[id]][["select"]], select) %>% length0_to_null()
    )
    # COMBAK currently trt and id also have to be part of the select vector. Reconsider?
    spec[[id]][["select"]] <- unique(spec[[id]][["id"]], spec[[id]][["trt"]], select)
    
  }else{
    
    spec[[id]][["adjustments"]] <- list(add = add, drop = drop)
    spec[[id]][["select"]] <- c(spec[[id]][["select"]], add) %>% 
      unique() %>% 
      setdiff(drop)
    
  }
  
  # if ("dict" %in% names(spec[[id]])){
  #   # intersect with param column from dict instead of data, to work independent of data attached
  #   add  <- intersect(spec[[id]][["dict"]][["param"]], add)
  #   drop <- intersect(spec[[id]][["dict"]][["param"]], drop)
  #   
  # }
  
  # update dict and data_info ####
  # COMBAK only works, if 'data' is attached
  # COMBAK data_info_ok attribute needs to be modified
  spec[[id]][['dict']]      <- create_dict(spec[[id]])
  spec[[id]][["data_info"]] <- data_info(spec[[id]])
  
  spec
  
}



