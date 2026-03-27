#' Create dictionary for spec entry
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' the dictionary as a tibble.
#' For all data types, the dict contains columns 
#' `label`, `source`, `type` and `selected`.
#' In addition for data type adsl and bds there is `param`, with
#' `unit` and `selected` as additional column for bds and adsl, resp.
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

create_dict <- function(spec_entry){
  
  # input checks
  # COMBAK create_dict checks, provide for build() output
  # as should be exported to re-run after changes to build() output
  # not exported; no extra checks on spec_entry's list structure and entries
  
  if(is.null(spec_entry$data)){
    cli::cli_abort( c(
       'i' = 'Dictionary creation requires data to be attached to the spec object.',
       'x' = 'No data is attached.',
       '*' = 'Rerun {.fn adam_spec} with {.code attach_data = TRUE}.'
    ))
    
  }
   
  
  dict <- with(spec_entry, {
    
    if(type == "adsl"){
      
      labelled::var_label(data) %>%
        # fix labels (no label = empty string (already done in spec creation, just to be safe))
        purrr::imap(~{
          if(is.null(.x)){.y}else{.x}}
        ) %>% 
        unlist() %>% 
        tibble::enframe(name = 'param', value = 'label') %>% 
        dplyr::mutate(source = spec_id) %>% 
        dplyr::mutate(type = type) %>% 
        dplyr::mutate(selected = param %in% select)
      
    }else if(type == "bds"){
      
      if (is.null(param)) {
        list(NULL)
      } else {
        dict  <- data %>% 
          dplyr::select( tidyselect::any_of(
            # determine combinations of only param, label and unit to handle randomly missing unit entries
            # same unit should be filled across timepoints
            c("param" = param, "label" = label, unit) %>% na.omit() 
          )) %>% 
          {if (!is.null(unit)){
            dplyr::group_by(., dplyr::across(-tidyselect::any_of(c("param", "label")))) %>% 
              tidyr::fill(tidyselect::any_of(unit), .direction = "downup") %>% 
              dplyr::ungroup() %>% 
              dplyr::rename(tidyselect::any_of(c("unit" = unit)))
          } else {
            .
          }} %>% 
          dplyr::distinct() %>%
          dplyr::mutate(source = spec_id) %>% 
          dplyr::mutate(type   = type) 
        
        # for consistent dict structure: add NA columns for time and/or unit if missing
        if(is.null(unit)) dict <- dict %>% dplyr::mutate(unit = NA_character_)
        
        param_sel <- data %>% 
          {if(length(filter) > 0){       
            dplyr::filter(., !!! rlang::parse_exprs(filter))
          }else{.}
          } %>% 
          dplyr::pull(param) %>% 
          unique()
        
        dict %>% dplyr::mutate(selected = param %in% param_sel)
      }
      
    }else if(type == 'occds'){
      
      if (is.null(label)) {
        list(NULL)
      } else {
        dict  <- data %>% 
          dplyr::select(label = !! rlang::sym(label)) %>% 
          dplyr::distinct() %>%
          dplyr::mutate(source = spec_id) %>% 
          dplyr::mutate(type   = type) 
        
        label_sel <- data %>% 
          {if(length(filter) > 0){       
            dplyr::filter(., !!! rlang::parse_exprs(filter))
          }else{.}
          } %>% 
          dplyr::pull(!! rlang::sym(label)) %>% 
          unique()
        
        dict %>% dplyr::mutate(selected = label %in% label_sel)
        # TODO add param column
      }
      
    }
    
  })
  
  # remove data set label automatically created by haven::read_sas()
  dict <- dict %>% 
    haven::zap_formats() %>%
    haven::zap_label() # zaps e.g. label "Parameter code" for param
  attr(dict, "label") <- NULL
  
  dict
  # TODO write test 'create_dict'
}



#' data info on spec id
#' 
#' info on size of extracted data in terms of numbers of subjects and columns (roughly) 
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' list with entries `nsubj` and `ncol` giving
#' the number of distinct values in the .id column and the (approximate)
#' number of columns derived from the data set, respectively.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

# TODO complete docu

data_info <- function(spec_entry){
  
  # TODO rewrite for full spec and map over ids to return 
  #updated version of spec where data_info slots of ids are updated
  
  #spec_entry[["data_info"]] <-
  with(spec_entry, {
    
    out <- list(
      nsubj = NA_integer_,
      ncol  = NA_integer_
    )
    
    if(!is.null(data)){ 
      
      out$nsubj <- data %>% 
        {if(length(filter) > 0){ 
          dplyr::filter(., !!! rlang::parse_exprs(filter))
        }else{.}} %>% 
        dplyr::select(tidyselect::all_of(id)) %>% 
        dplyr::n_distinct()
      
    }
    
    if(!is.null(dict)){
      # ncol is based on dictionary, so it works with and w/o data attached
      out$ncol <- dict %>% 
        {if(type == "adsl"){
          dplyr::filter(., selected)
        }else{.}} %>% 
        nrow()
      
    }
    
    out
  })
  
}


cor_quiet <- purrr::quietly(stats::cor)


#' Prepare column selection
#' 
#' Prepare column selection in `adam_spec_*()` functions for output. This 
#' includes checks for column presence and guessing of column names based on
#' ADaM standards and transformation into a standard format for further use 
#' within the `adam_spec_*()` functions.
#'
#' @param ... objects containing the column names for the roles in the data set 
#' following the standard naming convention in the `adam_spec_*()` functions
#' (`id`, `value`, etc.).
#' @inheritParams check_and_guess_column
#'
#' @return
#' A list with `col_select` containing the column names for the roles in the
#' data set and `use_for_build` indicating, if all checks on the columns have
#' passed.

prepare_col_selection <- function(
    data, 
    ..., 
    type = c("adsl", "bds", "occds"), 
    call = rlang::caller_env()
){
  
  type <- rlang::arg_match(type)
  
  dots <- rlang::dots_list(..., .named = TRUE)
  
  # collect column name parameters ####
  # TODO use `spec_cols_required` object
  col_spec <- if (type == "adsl") {
    list(
      "id"  = list(column = dots$id,  required = TRUE),
      "trt" = list(column = dots$trt, required = FALSE)
    )
  } else if (type == "bds") {
    list(
      "id"    = list(column = dots$id,    required = TRUE),
      "value" = list(column = dots$value, required = TRUE),
      "param" = list(column = dots$param, required = TRUE),
      "time"  = list(column = dots$time,  required = FALSE),
      "unit"  = list(column = dots$unit,  required = FALSE),
      "label" = list(column = dots$label, required = FALSE)
    )
  } else if (type == "occds") {
    list(
      "id"     = list(column = dots$id,     required = TRUE),
      "label"  = list(column = dots$label,  required = TRUE),
      "value"  = list(column = dots$value,  required = FALSE),
      "valuen" = list(column = dots$valuen, required = FALSE)
    )
  }
  
  col_select_raw <- purrr::imap(col_spec, ~{
    check_and_guess_column(
      data = data, 
      role = .y, 
      column_spec = .x$column, 
      required = .x$required,
      type = type, 
      call = call
    )
  })
  
  use_for_build <- purrr::map_lgl(col_select_raw, "check_passed") %>% all()
  col_select    <- purrr::map(col_select_raw, "column")
  
  if (type == "bds") {
    col_select[['label']] <- col_select[['label']] %||% col_select[['param']]
  }
  
  list(
    col_select = col_select,
    use_for_build = use_for_build
  )
  
}

#' Check role specification for ADaM data set
#' 
#' Checks, if provided column for a role in an ADaM data set is present in the 
#' data. If no column is provided, it is guessed based on ADaM standards.
#'
#' @param data data set to check
#' @param role character. the role to check, e.g. "param", "id", "value" or
#' "time"
#' @param column_spec character. the selected column name. will be for 
#' presence in `data`and type. If `NULL` (the default), it will be guessed based 
#' on `domain` or `type`.
#' @param type character. either "bds" or "occds"
#' @param spec_id character. an optional id for the specification that is used 
#' for informative warnings
#' @param required boolean. `TRUE`, if `role` is required, `FALSE` if optional.
#' @param call the execution environment of a currently running function. 
#'
#' @return
#' A list with `role`, the column name `column` or `NULL` (if column check was 
#' not successful or no column for `role` could be guessed), `required` (passed 
#' as-is from the input) and a boolean `check_passed`, indicating, if all checks 
#' on the column have passed. Throws an informative warning if any check fails.
#' 

check_and_guess_column <- function(
    data,
    role,
    column_spec = NULL,
    type = c("adsl", "bds", "occds"),
    spec_id = NULL,
    required = TRUE,
    call = rlang::caller_env()
){
  
  type <- rlang::arg_match(type)
  
  out <- list(
    role = role,
    column = NULL,
    required = required,
    check_passed = TRUE
  )
  
  colnames_data <- colnames(data)
  
  if (is.null(spec_id)) {
    # check, if needed
    msg_start <- NULL
    msg_code  <- NULL
  }else{
    msg_start <- paste0("{.strong ", spec_id, ":} ")
    msg_code  <- c(
      "*" = paste(
        "Use {.code adjust_spec(<spec_obj>, id = '{spec_id}',",
        "{role} = <column>)} to add a valid '{role}' column."
      )
    )
  }
  
  # for provided column name...
  if (!is.null(column_spec)) {
    # ... check if column is present in data
    if (column_spec %in% colnames_data) {
      out$column <- column_spec
    }else{
      out$check_passed <- FALSE
      cli::cli_warn(
        c(
          "!" = "{msg_start}You provided {.code {column_spec}} as the '{role}' column.",
          "i" = "Column {.code {column_spec}} is not present in the data."
        ) %>% c(msg_code),
        call = call
      )
    }
  }else{ # if no column name was provided
    guess <- if (type == "adsl") {
      NULL
    }else{
      # returns a single column name or NULL if no column could be guessed
      adam_guess(
        role = role,
        type = type,
        colnames_data = colnames_data
      )
    }
    
    if (is.null(guess) && required) {
      out$check_passed <- FALSE
      cli::cli_warn(
        c(
          "!" = "{msg_start}No '{role}' column could be guessed from the data.",
          "i" = "Please provide a column name for '{role}'."
        ) %>% c(msg_code),
        call = call
      )
    }else{
      out$column <- guess
    }
  }
  
  out
  
}


#' Create output object for build specifications
#'
#' @param ... output objects
#' @inheritParams check_and_guess_column
#' @inheritParams adam_spec
#'
#' @return
#' Output object of `adam_spec_*()`
#' 
#' @seealso 
#' [adam_spec_adsl()]
#' [adam_spec_bds()]
#' [adam_spec_occds()]

create_spec_out <- function(
    ...,
    type = c("adsl", "bds", "occds"), 
    attach_data = TRUE
){
  
  type  <- rlang::arg_match(type)
  input <- rlang::dots_list(..., .named = TRUE)
  
  out <- list(
    file      = input$file,
    data      = input$data,
    md5       = input$md5,
    size      = input$size, 
    type      = type,
    filter    = input$actual_filter,
    spec_id   = input$domain
  ) %>% 
    append(
      # TODO create roles entry that contains col_select
      input$col_select
    ) 
  
  if (type == "bds") {
    # required by 'build_bds()'
    out$dupl_ctrl <- list(
      values_fn = NULL,
      arrange   = NULL
    )
  }
  
  if (type == "occds") {
    # required by 'build_occds()'
    out$count <- input$count
  }
  
  if (type == "adsl") {
    # required by 'build_adsl()'
    out$select        <- input$select_list
    out$factor_levels <- input$factor_levels
    out$drop_list     <- input$drop_list
    out$flag_table    <- input$flag_table
  }
  
  # create data_info and dict  ####
  out$dict      <- create_dict(out)
  out$data_info <- data_info(out)
  
  out$use_for_build <- input$use_for_build
  
  if (!attach_data) {
    # only keep data, if 'attach_data = TRUE'
    # (was needed to create data info)
    out$data <- NULL
  }
  
  out
  
}

#' Import file and collect info
#'
#' @param file filepath
#' @param catalog_file path to the catalog file to be passed to 
#' [haven::read_sas()]. Defaults to NULL. 
#' Ignored if `file` is not a sas7bdat file.
#'
#' @return  list containing data and corresponding md5 sum and file size

#' 
import_info <- function(
    file,
    catalog_file = NULL
){
  # TODO maybe pass context
  if (!fs::file_exists(file)) {
    cli::cli_abort(c(
      #"{.fn adam_spec_bds} 
      "Could not create a spec from the provided file." ,
      'x' = "The following file could not be found: {.path {file}}")
    )
  }
  file_ext <- tools::file_ext(file) 
  
  if (!file_ext %in% c("sas7bdat", "rds")) {
    cli::cli_abort(c(
      #"{.fn adam_spec_bds} 
      "expects a sas7bdat or rds file to read, but was provided {.path {file}}.",
      'x' = 'The provided file is of type {tools::file_ext(file)}.',
      '*' = 'Please check your input or attach a data set instead.'
    ))
    
  }
  
  data <- if (file_ext == 'sas7bdat') {
    haven::read_sas(
      data_file = file,
      catalog_file = catalog_file
    )
  }else if (file_ext == 'rds') {
    readRDS(file)
  }else{
    stop('Only sas7bdat and rds data supported.')
  }
  
  # transform empty strings into NAs (SAS does not have character NAs)
  # NOTE na_if() should not remove any attributes from the column in 
  # combination with mutate() 
  if (file_ext == 'sas7bdat') {
    data <- data %>% 
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.character),
        ~dplyr::na_if(., "")
      ))
  }
  
  list(
    data = data,
    md5  = tools::md5sum(file) %>% as.character(),
    size = fs::file_size(file)
  )
  
}

#' Check for 'N' values in --OCCUR column in occds
#'
#' @param data data set to check
#' @param domain name of data set
#' @param filters filters to be applied before checking for issues. 
#' defaults to NULL, in which case no filters are applied.
#' @param quiet whether to suppress messaging in the console. defaults to FALSE.
#' @param no_char,no_num values that code for 'no' in pre-specified list of 
#' events. defaults to `N` and `0` for character and numeric variables, resp.
#'
#' @return invisibly returns character vector of potentially problematic columns. 
#' (Invisible) return value has length 0 if no problems were detected.
#'
check_occds_occur <-  function(
    data, 
    domain = NULL, 
    filters = NULL,
    quiet = FALSE, 
    no_char = "N",
    no_num = 0
  ){
  
  occur_columns <- stringr::str_detect(
    string = colnames(data),
    pattern = "^.{1,3}(?i)occurN?(?-i)$",
  ) %>%
  purrr::set_names(colnames(data)) %>%
  purrr::keep(isTRUE) %>% 
  names()

  if (length(occur_columns) == 0) {
    return(invisible(character()))
  }
  contains_N <- data %>% 
    # TODO filter check?
    {if (!is.null(filters)) {
      dplyr::filter(., !!! rlang::parse_exprs(filters))
    } else {.}
    } %>% 
    dplyr::select(tidyselect::any_of(occur_columns)) %>% 
    # values to keep: 'Y' or missing (not predefined)
    purrr::imap_lgl(
      ~ {if(is.character(.x)) any(.x == no_char) else any(.x == no_num)}
    ) %>%
    purrr::keep(isTRUE) %>% 
    names()
  
  if (length(contains_N) > 0 && !quiet) {
    
    filter_suggested <- "--OCCUR == 'Y' | is.na(--OCCUR)"
    
    cli::cli_inform(c(
      "i" = cli::col_silver(paste0(
        "{if(!is.null(domain)) paste0(stringr::str_to_lower(domain), ': ')}", 
        "{cli::qty(contains_N)}The column{?s} {contains_N} contain{?s/} {no_char}/{as.character(no_num)} values", 
        ifelse(length(filters) > 0, " after using applicable filters", ""),
        ".")),
      "*" = cli::col_silver(paste0(
        "Please check if an additional filter is required",
        " such as {.code ", filter_suggested, "}."
      ))
    ))
  }
  
  invisible(contains_N)
}


