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
adjust_spec <- function(
    spec, 
    entry,
    ...
){
  
  stopifnot(inherits(spec, what =  "martini_spec"))
  
  if (!entry %in% names(spec)) usethis::ui_stop(
    crayon::magenta(
      paste0("No spec with the name ", usethis::ui_code(entry), " available.") 
    )
  )
  
  modifications <- list(...)
  if (length(modifications) == 0) return(spec)
  
  # CHECKS ----
  modifications <- check_adjust(modifications, spec = spec, entry = entry)
  if (length(modifications) == 0) return(spec)
  
  spec[[entry]][names(modifications)] <- list(NULL)
  spec[[entry]] <- spec[[entry]] %>% purrr::list_modify(!!! modifications)
  
  # update data_info and filters if possible
  if (!is.null(spec[[entry]][["data"]])) {
    
    # update dict and data_info
    spec[[entry]][["dict"]]      <- create_dict(spec[[entry]])
    spec[[entry]][["data_info"]] <- data_info(spec[[entry]])
    attr(spec[[entry]], "data_info_ok") <- TRUE
    # COMBAK restructure to attribute on entry level instead of global for spec
  }else{
    
    attr(spec[[entry]], "data_info_ok") <- FALSE
    
  }
  
  martini_col_spec_required <- list(
    "adsl"  = c("id"),
    "bds"   = c("id", "value", "param"),
    "occds" = c("id", "label")
  )[[spec[[entry]][["type"]]]]
  
  spec[[entry]][["use_for_build"]] <- spec[[entry]][martini_col_spec_required] %>% 
    purrr::map_lgl(~{!is.null(.x)}) %>% 
    all()

  spec
  
}


#' check key value pair inputs for adjust_* functions
#'
#' @param spec spec object to modify
#' @param entry name of list element to modify in `spec`
#' @param modifications list of key value pairs defining adjustments
#'
#' @return subset of modifications that are valid to apply
#' 
check_adjust <- function(spec, entry, modifications){
  
  # check: refer to other funs for id, select, filter ####
  if("select" %in% names(modifications)){
    
    cli::cli_inform(c(
      "{.code select} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please rerun {.fun adam_spec} or use {.fun adjust_adsl_select}."
    ))
    
    modifications <- modifications %>% purrr::discard_at("select")
    
  }
  
  if("id" %in% names(modifications)){
    # id column must be present in all data sets, needs to be checked globally 
    # TODO spec is given, just check here if available for all entries if data is attached
    cli::cli_inform(c(
      "{.code select} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please rerun {.fun adam_spec}."
    ))
    
    modifications <- modifications %>% purrr::discard_at("id")
    
  }
  
  if ("filter" %in% names(modifications)) {
    
    cli::cli_inform(c(
      "{.code filter} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = paste(
        "Please rerun {.fun adam_spec} or use",
        "{.fun adjust_filter} in case data is attached."
      )
    ))
    
    modifications <- modifications %>% purrr::discard_at("filter")
    
  }
  
  if("factor_levels" %in% names(modifications)){
    
    cli::cli_inform(c(
      "{.code factor_levels} can't be modified by {.fun adjust_spec}.",
      "!" = "Adjustment will be ignored.",
      "*" = "Please use {.fun adjust_adsl_factors}."
    ))
    
    modifications <- modifications %>% purrr::discard_at("factor_levels")
    
  }
  
  # check: protected entries ####
  protected <- c(
    "file", "data", "md5", "size", "data_info", 
    "dict", "use_for_build", "spec_id", "drop_list", "flag_table"
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
  
  # check entries that should have logical values ####
  # if they have the wrong format, inform the user and ignore modification by removing it
  entries_lgl_check <- c(
    # occds
    "count"
  ) %>% 
    intersect(names(modifications)) %>% 
    purrr::set_names()
  
  if (length(entries_lgl_check) > 0) {
    
    # ... format ####
    check_format <- purrr::map_lgl(entries_lgl_check, ~{
      rlang::is_logical(modifications[[.x]], n = 1)
    })
    wrong_format <- names(check_format)[!check_format]
    
    if (length(wrong_format) > 0) {
      cli::cli_inform(c(
        "i" = paste0(
          "The following {cli::qty(length(wrong_format))} entr{?y/ies} ", 
          "have to be logicals of length 1 but ",
          "{?is/are} of the wrong format: {wrong_format}."
        ),
        "!" = "{cli::qty(length(wrong_format))}{?This/These} adjustment{?s} will be ignored.",
        "*" = "Please check your adjustment instructions."
      ))
      
      modifications <- modifications %>% purrr::discard_at(wrong_format)
    }
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
    
    if (length(wrong_format) > 0) {
      cli::cli_inform(c(
        "Entries that specify column names in the data must be character vectors of length 1.",
        "i" = "The following {cli::qty(length(wrong_format))} entr{?y/ies} {?is/are} of the wrong format: {wrong_format}.",
        "!" = "{cli::qty(length(wrong_format))}{?This/These} adjustment{?s} will be ignored.",
        "*" = "Please check your adjustment instructions."
      ))
      
      modifications <- modifications %>% purrr::discard_at(wrong_format)
    }
    
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
  
  # check: dupl_ctrl ####
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
      check_passed_valuesfn <- rlang::is_function(modifications[["dupl_ctrl"]]$values_fn)
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
      check_passed_arrange <- length(modifications[["dupl_ctrl"]]$arrange) == 1
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


#' Adjust spec object filter 
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

adjust_filter <- function(spec, filter, append = TRUE){
  
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
      
      spec[[.x]][["dict"]]      <<- create_dict(spec[[.x]])
      spec[[.x]][["data_info"]] <<- data_info(spec[[.x]])
      
      attr(spec[[.x]], "filter_ok")    <<- TRUE
      attr(spec[[.x]], "data_info_ok") <<- TRUE
      
    }else{
      
      spec[[.x]][["filter"]] <- filter_update
      
      attr(spec[[.x]], "filter_ok")    <<- FALSE
      attr(spec[[.x]], "data_info_ok") <<- FALSE
      
    }
    
    
  })
  
  class(spec) <- c("martini_spec", class(spec))
  
  any_data_missing <- any(purrr::map_lgl(spec, ~{is.null(.x[["data"]])}))
  
  if(any_data_missing){
    cli::cli_inform(c(
      "Can't check {.code filter} if {.code data} is not attached.",
      "i" = "At least one entry of {.code spec} does not have the data attached.",
      "i" = "The specified {.code filter} will be added to the spec as is.",
      "*" = "Please rerun `adam_spec()` with the additional filters for proper checks."
    ))
  }
  
  # set of all filters
  attr(spec, "filter") <- spec %>% 
    purrr::map("filter") %>% unlist() %>% unique()
  
  spec
  
}


#' Adjust factor (levels) from adsl
#'
#' @param spec object of class `martini_spec`
#' @param fctrs named list (column in data) of named vectors with 
#' factor levels (values) and labels (names)
#' @param entry name of spec entry to modify, defaults to "adsl"
#'
#' @return the `spec` object with all valid modifications to the factor 
#' definitions applied.
#' The modification of a factor definition is skipped (with info) 
#' if not all factor levels of this factor that were derived from the data set 
#' are included in the modified list of levels.
#' Additional factor level can be introduced, the user is informed.
#' 
#' @export
#'
adjust_adsl_factors <- function(spec, fctrs, entry = "adsl"){
  
  stopifnot(inherits(spec, what =  "martini_spec"))
  stopifnot(entry %in% names(spec))
  stopifnot(inherits(fctrs, "list"))
  stopifnot(rlang::is_named(fctrs))
  
  
 # discard ignored ones, set names, then boils down to purrr::list_modify()
  fctrs_used <- check_adjust_adsl_factors(
    spec  = spec, 
    fctrs = fctrs,
    entry = entry
  )
  
  # apply adjustment (list_modify works with NULL)
  spec[[entry]][["factor_levels"]] <- spec[[entry]][["factor_levels"]] %>% 
    purrr::list_modify(!!! fctrs_used)
  
  spec
  
}

#' Helper for factor (level) adjustment of spec
#'
#' @param spec object of class martini_spec
#' @param fctrs named list(column in data) of named vectors with
#' factor levels (values) and labels (names)
#' @param entry name of spec entry to modify, defaults to 'adsl'
#'
#' @details
#' The function checks if the modifications to the factor definitions
#' provided in `fctrs` are valid:
#' 
#' * The names of `fctrs` top level entries must be present in the data 
#' of the entry (checked by attached data or dictionary).
#'  Else: ignore with message
#' * The factor levels provided must contain all current/known levels.
#' Else: ignore with warning.
#' * New factor levels (not seen in data) can be introduced (with message)
#' * if only levels are provided, labels are created via `purrr::set_names()`
#' 
#' @return subset of `fctrs` that are valid to apply, `NULL` if none
#'
check_adjust_adsl_factors <- function(
    spec, 
    fctrs,
    entry = "adsl"
){
  
  # check names(fctrs) are colnames of adsl/entry data
  # ignore non-matching ones
  adsl_names <- spec[[entry]][["dict"]][["param"]] 
  # NOTE could also check if colnames available and is part of select,
  # otherwise point to add in adjust_adsl_select()
  cols_missing <- setdiff(names(fctrs), adsl_names)
  
  if (length(cols_missing) > 0) {
    
    cli::cli_inform(c(
      paste("Only columns existing in", entry, "can be modified."),
      "i" = "You specified the following non-existing {cli::qty(length(cols_missing))} column{?s}: {cols_missing}.",
      "i" = "The corresponding factor {cli::qty(length(cols_missing))} definition{?s} will not be modified.",
      "*" = "Please check your adjustment instructions."
    )
    )
    
    fctrs <- fctrs %>% purrr::discard_at(cols_missing)
    
  }
  if(length(fctrs) == 0) return(NULL)
  
  # if is already factor (i.e. in names factor_levels) or data is present,
  # check factor levels are setequal with current entry -> reorder
  
  # if data is available extract unique values as ref for level set
  # if not check the current fct level list
  # ref_levels NULL for new factors AND no data
  ref_levels <- if (!is.null(spec[[entry]][["data"]])) {
    spec[[entry]][["data"]] %>% 
      dplyr::select(tidyselect::any_of(names(fctrs))) %>% 
      purrr::map(~{
        if(is.factor(.x)){
          levels(.x)
        }else{
          unique(.x)
        }
      }) %>% 
      purrr::map(na.omit)
  }else{
    spec[[entry]][["factor_levels"]] %>%
      purrr::keep_at(names(fctrs))
  }
  
  # build() expects named vector, where names are labels, values levels (i.e. in data)
  fctrs <- fctrs %>%  
    purrr::map(~{
      if(!rlang::is_named(.x)){
        rlang::set_names(.x) 
      }else .x
    })
  
  use_factor <- purrr::map_lgl(names(fctrs), ~{
    
    # no checks are possible without ref_levels -> apply adjustment as-is
    if(is.null(ref_levels[[.x]])) return(TRUE)
    
    # info only: new level introduced
    new_level_introduced <- !all(fctrs[[.x]] %in% ref_levels[[.x]])
    
    if (new_level_introduced) {
      
      new_levels <- setdiff(fctrs[[.x]], ref_levels[[.x]])
      
      cli::cli_inform(c(
        paste(
          "The current set of levels for the factor", .x, 
          "{?is/are} {ref_levels[[.x]]}"
        ),
        "i" = paste(
          "The provided set of levels for", .x, 
          "introduces the following {cli::qty(length(new_levels))}",
          "new level{?s}: {new_levels}."
        )
      ))
      
    }
    
    
    # warn and ignore: level missing
    any_level_missing <- !all(ref_levels[[.x]] %in% fctrs[[.x]])
    
    if (any_level_missing) {
      
      level_missing <- setdiff(ref_levels[[.x]], fctrs[[.x]])
      
      potential_label_mixup <- (length(level_missing) > 0) &&
        all(ref_levels[[.x]] %in% names(fctrs[[.x]]))
      
      cli_out <- c(
        paste(
          "The provided set of levels for", .x, 
          "is missing the following existing {cli::qty(length(level_missing))}",
          "level{?s}: {level_missing}."
        ),
        "i" = paste("Updates for factor", .x, "will be ignored."),
        "*" = "Please check your adjustment instructions."
      )
      
      if(potential_label_mixup){
        cli_out <- c(
          cli_out,
          "*" = paste(
            "Note that the values provided are interpreted as factor levels",
            "and labels to be used in {.fun build} may be provided as names."
          )
        )
      }
      
      cli::cli_warn(cli_out)
      
    }
    !any_level_missing
  })
  if(sum(use_factor) == 0) return(NULL)
  
  fctrs %>% 
    purrr::keep_at(names(fctrs)[use_factor]) 
} 


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
#' @param entry name of list element to modify in the spec, defaults to "adsl"
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
  modifications <- check_adjust_adsl_select(
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
    
    attr(spec[[entry]], "data_info_ok") <- TRUE
  }else{
    attr(spec[[entry]], "data_info_ok") <- FALSE
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
  if (!inherits(spec, "martini_spec")) {
    cli::cli_abort(c(
      "Input is not a martini spec object."
    ))
  }
  
  # any modification specified ####
  if (is.null(c(select, add, drop))) {
    cli::cli_inform(
      "No modifications specified. Returning unmodified spec object."
    )
    return(list(select, add, drop))
  } 
  
  # select wins over add, drop ####
  if (!is.null(select)) {
    
    if (!is.null(add) | !is.null(drop))
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
  
  if (select_misses_id | dropping_id) {
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



