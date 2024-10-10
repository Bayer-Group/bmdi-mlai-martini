#' Title
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' the dictionary
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' TODO complete docu

create_dict <- function(spec_entry){
  
  # input checks
  
  if(is.null(spec_entry$data)){
    cli::cli_abort( c(
       'i' = 'Dictionary creation requires data to be attached to the spec object.',
       'x' = 'No data is attached.',
       '*' = 'Rerun {.fn adam_spec} with {.code attach_data = TRUE}.'
    ))
    
  }
  # not exported; no extra checks on spec_entry's list structure and entries
  
  
  dict <- with(spec_entry, {
    
    if(type == "adsl"){
      
      labelled::var_label(data)  %>%
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
      
    }else if(type == 'occds'){
      
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
    
  })
  
  # remove data set label automatically created by haven::read_sas()
  attr(dict, 'label') <- NULL
  
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

#' Check role specification for ADaM data set
#' 
#' Checks, if provided column for a role in an ADaM data set is present in the 
#' data. If no column is provided, it is guessed based on ADaM standards.
#'
#' @param data data set to check
#' @param role character. the role to check, e.g. "param", "id", "value" or
#' "time"
#' @param column_spec character. the selected column name. will be for 
#' presence in `data`and type. if NULL (the default), it will be guessed based 
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

check_role <- function(
    data,
    role,
    column_spec = NULL,
    type = c("bds", "occds"),
    spec_id = NULL,
    required = TRUE,
    call = rlang::caller_env()
){
  
  type <- rlang::arg_match(type)
  
  out <- list(
    column = NULL,
    required = required,
    check_passed = TRUE
  )
  
  colnames_data <- colnames(data)
  
  if(is.null(spec_id)){
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
  
  if(!is.null(column_spec)){
    if(column_spec %in% colnames_data){
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
  }else{
    guess <- adam_guess(
      role = role,
      type = type,
      colnames_data = colnames_data
    )
    
    if(is.null(guess) && required){
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
#' @inheritParams check_role
#' @inheritParams adam_spec
#'
#' @return
#' Output object of `adam_spec_*()`
#' 
#' @seealso 
#' [adam_spec_bds()]
#' [adam_spec_occds()]

create_spec_out <- function(..., type = c("bds", "occds"), attach_data = TRUE){
  
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
  
  if(type == "bds"){
    # required by 'build_bds()'
    out$dupl_ctrl = list(
      values_fn = NULL,
      arrange   = NULL
    )
  }
  
  # create data_info and dict  ####
  out$dict      <- create_dict(out)
  out$data_info <- data_info(out)
  
  if(!attach_data){
    # only keep data, if 'attach_data = TRUE'
    # (was needed to create data info)
    out$data <- NULL
  }
  
  out
  
}


