#' Adjust column selection from data sets of type 'adsl' in spec object
#'
#' Helper function to make common adjustments to the spec object built by 
#' `\link{adam_spec}()` for data sets of type 'adsl' to be used with the `%>%`. 
#'
#' @param spec spec object to modify
#' @param add,drop character vector of columns to add to/discard from
#' the automated selection that is stored in the `select` entry of the 
#' corresponding `spec` entry (drop wins over add). 
#' @param select character vector of column names to be selected. if not NULL
#'  (the default), arguments `add` and `drop` will be ignored. 
#'  Overrides `select` entry of the corresponding `spec` entry.
#' @param id name of list element to modify in the spec, defaults to "adsl"
#' 
#' @details
#' if data is provided, the column names stored in the `select` slot will be 
#' intersected with actually existing column names. 
#' User will be informed in case of misspecifications.
#' 
#' If a trt column shall be used and is provided in the `select` slot, 
#' the user has to make sure to update the `trt` slot accordingly in 
#' `adam_spec()`.
#' 
#' 
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
  entry   = 'adsl'
){

  # CHECK specified modifications ####
  modifications  <- check_adjust_adsl_select(
    spec = spec, 
    add = add, 
    drop = drop, 
    select = select, 
    entry = "adsl"
  )
  
  add    <- modifications$add
  drop   <- modifications$drop
  select <- modifications$select
  
  # catch missing trt ####
  # (not in check function as trt slot of spec[[entry]])
  # missing trt by updated select or in drop
  select_misses_trt <- !is.null(select) && 
    (!all(spec[[entry]][["trt"]] %in% select))
  dropping_trt <- !is.null(drop) && 
    is.null(select) &&
    (spec[[entry]][["trt"]] %in% drop)   
  
  if(select_misses_trt | dropping_trt) {
    cli::cli_inform(c(
      paste(
        "The column", spec[[entry]][["trt"]], 
        "is specified as the relevant treatment column according to", entry,
        "entry of the spec in the `trt` slot."
      ),
      "!" = paste(
        "According to the specified modifications, the treatment column", 
        spec[[entry]][["trt"]],
        "is not part of selected columns."
      ),
      "i" = paste(
        "The `trt` slot of the spec will be set to NULL."
      ),
      "*" = paste(
        "Please make sure to either include the relevant treatment column",
        "in the column selection", 
        "or adjust the {.code trt} slot accordingly."
      )
    )
    )
    
    spec[[entry]][["trt"]] <- NULL
  }
  
  
  # UPDATES of the select and adjustment entries ####
  
  if (!is.null(select)) {
    
    spec[[entry]][["adjustments"]] <- list(
      add  = setdiff(select, spec[[entry]][["select"]]) %>% length0_to_null(),
      drop = setdiff(spec[[entry]][["select"]], select) %>% length0_to_null()
    )
    spec[[entry]][["select"]] <- select
    
  }else{
    
    spec[[entry]][["adjustments"]] <- list(add = add, drop = drop)
    spec[[entry]][["select"]] <- c(spec[[entry]][["select"]], add) %>% 
      unique() %>% 
      setdiff(drop)
    
  }

  
  # update dict and data_info ####
  if (!is.null(spec[[entry]][["data"]])) {
    spec[[entry]][["dict"]]      <- create_dict(spec[[entry]])
    spec[[entry]][["data_info"]] <- data_info(spec[[entry]])
    
    attr(spec, "data_info_ok") <- TRUE
  }else{
    attr(spec, "data_info_ok") <- FALSE
  }

  
  spec
  
}



#' checks for adjustments in `adjust_adsl_select()`
#'
#' @param spec object of class `martini_spec`
#' @param add,drop character vectors of columns to add to/discard from current selection
#' @param select character vector of columns to select (override current selection)
#' @param entry name of spec entry to modify
#'
#' @return named list of valid modifications to apply 
#' (entries `add`, `drop`, `select`)
#' 
#' 
check_adjust_adsl_select <- function(
    spec, add, drop, select, entry = "adsl"
){
  
  # spec of correct type ####
  if(!inherits(spec, "martini_spec")) {
    cli::cli_abort(c(
      "Input is not a martini spec object."
    ))
  }
  
  # any modification specified ####
  if (is.null(c(select, add, drop))){
    cli::cli_inform(
      "No modifications specified. Returning unmodified spec object."
    )
    return(list(select, add, drop))
  } 
  
  # select wins over add, drop ####
  if (!is.null(select)) {
    
    if(!is.null(add) | !is.null(drop))
      cli::cli_inform(
        "Arguments `add` and `drop` will be ignored, as `select` is provided."
      )
    
    add  <- NULL
    drop <- NULL
  }
  
  
  # entry exists ####
  if (!entry %in% names(spec)) {
    cli::cli_abort(c(
      "No spec with the name ", entry, " available."
    ))
  }
  
  
  # catch missing id ####
  # missing id by updated select or in drop
  select_misses_id <- !is.null(select) && 
    (!all(spec[[entry]][["id"]] %in% select))
  dropping_id <- !is.null(drop) && 
    is.null(select) &&
    (spec[[entry]][["id"]] %in% drop)   
  
  if(select_misses_id | dropping_id) {
    cli::cli_warn(c(
      paste(
        "The identifier column contained in the subject level",
        "data set must be selected."
      ),
      "!" = paste(
        "According to the specified modifications, the identifier column", 
        spec[[entry]][["id"]],
        "is not part of the selected columns."
      ),
      "i" = paste(
        "The column", spec[[entry]][["id"]],
        "will be added to the selection."
      ),
      "*" = paste(
        "If the use of an alternative {.code id} column was intended,",
        "please re-run {.code adam_spec()}",
        "with the {.code id} argument."
      )
    )
    )
    
    drop   <- setdiff(drop, spec[[entry]][["id"]]) %>% length0_to_null()
    select <- c(spec[[entry]][["id"]], select)
  }
  
  
  
  if (!is.null(spec[[entry]][["data"]])) {
    
    cols_not_in_data <- setdiff(
      unique(c(add, drop, select)),
      colnames(spec[[entry]][["data"]])
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
      
      names_data <- colnames(spec[[entry]][["data"]]) 
      select <- setdiff(select, names_data) %>% length0_to_null()
      add    <- setdiff(add,    names_data) %>% length0_to_null()
      drop   <- setdiff(drop,   names_data) %>% length0_to_null()
      
    }
    
  }
  
  modifications <- tibble::lst(add, drop, select)
  modifications
  
}

