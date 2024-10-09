#' Create specification object for AdaM data sets of type 'bds'
#' 
#' Given a file containing a bds data set (e.g. adlb or advs), \code{\link{adam_spec_bds}()} will create a specification 
#' object for use in \code{\link{build_bds}()} to prepare the data to be used in machine learning. 
#' The main task is to collect the key columns
#' for reshaping the data into wide format and prepare the data filter.
#' 
#' @param data tibble with the data for which the specification is created
#' @param file the path of the sas(7bdat) or rds file to process, ignored if data is provided
#' @param id name of id column to be kept and used for merge of data sets
#' @param param name of the column that identifies the parameter. Defaults to NULL, will be guessed if not set (see Details).
#' @param label name of the column that gives column labels. Defaults to NULL.
#' @param unit Defaults to NULL, will be guessed if not set (see Details).
#' @param time Defaults to NULL, will be guessed if not set (see Details).
#' @param value Defaults to NULL, will be guessed if not set (see Details).
#' @param value_type NULL or character string 'numeric' or 'character'. Determines guessing of the value column (e.g. AVAL over AVALC by default).
#' If NULL (default), the type will be guessed based on name of the sas file (if `file` is provided) or set to 'numeric'.
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. Attach the imported raw data.
#' @param domain character string to be included in dictionary. Automatically derived for standard ADaM data sets. If not set for `data` provided, dictionary entry will be 'custom'.
#' 
#' @details
#' 
#' Values for arguments `param`, `label`, `unit`, `time` and `value` will be guessed if not provided. 
#' (Only available with usage of 'file'; for 'data' all columns have to be specified explicitly.)
#' Guess will be the first of the following options that matches a column name (exact match).
#' 
#' \describe{
#'   \item{`param`}{`PARAMCD`, `**TESTCD`, with `**` reflecting the two letter domain abbreviation (e.g. `LB`)}
#'   \item{`label`}{substring of `param` with trailing `CD` removed}
#'   \item{`time`}{`AVISIT`, `VISIT`}
#'   \item{`value`}{`AVAL`, `**STRESN`, `**ORRES` for numeric values, 
#'   `AVALC`, `**STRESC`, `**ORRES` for character values,
#'    with `**` reflecting the two letter domain abbreviation}
#'   \item{`unit`}{`AVALU`, `**STRESU`, `**ORRESU`, with `**` reflecting the two letter domain abbreviation}
#' }
#' 
#' Function will escape if one of `param` or `value` are neither provided nor can be guessed. The other columns are optional.
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{`data_info`}{a list containing the number of subjects `nsubj` and columns `ncol` in the data after applying `filter`}
#' \item{`type`}{character string \code{bds}, generally giving the type of AdaM data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields valid and non-empty result when applied individually (using \code{\link{check_filter}()})}
#' \item{`id`}{passing unchanged input}  
#' \item{`param`, `label`, `value`, `unit`, `time`}{names of the key columns to be used in \code{\link{build_bds}()} for reshaping}
#' \item{`spec_id`}{character string, generally the name of the domain} 
#' \item{`dict`}{a tibble with unique combinations within the `param` and `label` column (if present in the data set) 
#' to be used as a data dictionary}
#' \item{`dupl_ctrl`}{a list of length 2 with parameters `values_fn` and `arrange` that are passed to \code{\link{build_bds}()} to handle pivoting for duplicated values. Both default to NULL.}
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @export

adam_spec_bds <- function(
  file        = NULL,
  data        = NULL,
  id          = 'SUBJID', 
  param       = NULL,
  label       = NULL,
  unit        = NULL,
  time        = NULL, 
  value       = NULL,
  value_type  = NULL,
  filter      = NULL,
  attach_data = FALSE,
  domain      = NULL
){
  
  # initial check(s) ####
  
  if (all(c(is.null(data), is.null(file)))){
    usethis::ui_stop(paste0('At least one of ', usethis::ui_code('data'), ' or ', usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  if (!is.null(value_type) && !value_type %in% c("character", "numeric")){
    usethis::ui_stop(paste0(
      usethis::ui_code("value_type"), " needs to be either ",
      usethis::ui_value("numeric"), " or ", usethis::ui_value("character"), ".\n"
    ))
  }
  
  # collect column name parameters ####
  
  col_select <- list(
    "id"    = id,
    "value" = value,
    "param" = param,
    "time"  = time,
    "unit"  = unit,
    "label" = label
  )
  
  
  # check/modify 'col_select' and set 'domain' ####
  
  if (!is.null(data)){
  
    # ... if 'data' is used ####
      
    bds      <- data
    md5      <- NULL
    size     <- NULL
    coln_bds <- colnames(bds)
    
    # check if required columns are provided by user
    missing_params <- purrr::map_lgl(list(param = param, value = value, id = id), is.null) %>% which() %>% names()
    
    if(length(missing_params) > 0){
      cli::cli_abort(c(
       'i' = "{missing_params} {?is/are} required parameter{?s} for specification of bds-type data when a {.code data} is provided instead of {.file}.",
       'x' = "Information to create the spec for {.code spec$spec_id} is incomplete.",
       '*' = 'Please provide the column names to use for {missing_params} in order to create the spec.'
      )
      )
     # usethis::ui_stop(paste0(paste(usethis::ui_code(missing_params), collapse = ", "), " need(s) to be provided.\n"))
    }
    
    # set 'domain' to default, if not provided
    if(is.null(domain)) domain <- 'custom'
        
  }else{
    
    # ... if 'file' is used ####
    
    if (!file.exists(file)){
      cli::cli_abort(c(
       "{.fn adam_spec_bds} could not create a spec from the provided file." ,
       'x' = "The following file could not be found: {.path {file}}")
      )
    }
    
    if (!tools::file_ext(file) %in% c("sas7bdat", "rds")) {
      cli::cli_abort(c(
        "{.fn adam_spec_bds} expects a sas7bdat or rds file to read, but was provided {.path {file}}.",
        'x' = 'The provided file is of type {tools::file_ext(file)}.',
        '*' = 'Please check your input or attach a data set instead.'
      ))
      
    }
    
    # ... ... import data ####
    file_ext <- tools::file_ext(file) 
    
    bds <- if(file_ext == 'sas7bdat'){
      haven::read_sas(file) %>% 
        dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
    }else if(file_ext == 'rds'){
      readRDS(file)
    }else{
      stop('Only sas7bdat and rds data supported.')
    }
    
    coln_bds <- colnames(bds)
    
    md5  <- tools::md5sum(file) %>% as.character()
    size <- fs::file_size(file)
    
    
    # ... ... identify domain ####
    domain <- basename(file) %>% 
      stringr::str_remove_all('^ad') %>% 
      tools::file_path_sans_ext() %>% 
      stringr::str_to_upper()
    
    dom <- stringr::str_sub(domain, 1, 2) # used in e.g. EGTEST (instead of EGFTEST)
    
    
    # ... ... define candidates for column names (based on 'dom') ####
    
    # TODO move guessing candidates to adam_guess()
    
    if (is.null(value_type) && dom %in% c("TR", "EGF")){
      value_type <- "character"
    } else {
      value_type <- "numeric"
    }
    
    guess_value_lst <- list(
      "numeric"   = c('AVAL',  paste0(dom, c("STRESN", "ORRES"))),
      "character" = c('AVALC', paste0(dom, c("STRESC", "ORRES")))
    )
    
    guess_value <- c(
      guess_value_lst[[value_type]],
      guess_value_lst[[which(! names(guess_value_lst) %in% value_type)]]
    )
    

    guesses <- list(
      
      # candidates 'param'
      param = c('PARAMCD', paste0(dom, 'TESTCD')),
      
      # candidates 'time'
      time = c('AVISIT', 'VISIT', 'AVISITN', 'VISITN'),
      
      # candidates 'value'
      value = guess_value,
      
      # candidates 'unit'
      unit = c('AVALU', paste0(dom, 'STRESU'),  paste0(dom, 'ORRESU'))
      
    )
    
    # candidates 'label'
    guesses$label <- stringr::str_remove(c(param, guesses$param), 'CD$')
    
    
    # ... ... check data for candidate columns ####
    
    col_required <- c('value', 'param')
    
    # ... ... ... guessed columns ####
    
    for (i in names(guesses)){ 
      
      if (is.null(col_select[[i]])){
        
        choices <- guesses[[i]] %>% intersect(coln_bds)
        
        if (length(choices) == 0 && i %in% col_required){
          # add 'domain' for information when used within adam_spec()
          usethis::ui_info(crayon::silver(paste0(
            'AD', domain, ": No column could be identified to be used as ", i, ". No spec will be provided.\n")))
          return(NULL)
        }
        
        col_select[[i]] <- choices[1]
        
        if(is.na(col_select[[i]])) col_select[i] <- list(NULL)
        
      }
      
    }
    
    
  }

  # column check ####
  # covers both cases: data or file provided

  # 'id' (input parameter with default, not guessed) ####
  if (!id %in% coln_bds){
    
    cli::cli_abort(c(
      "{.fn adam_spec_bds} could not create a spec for domain {.code {domain}} from {.path {file}}.",
      'x' = "The {.code id} column {.code {id}} is not present in the data set.",
      '*' = "Please provide a valid input to use as {.code id}." 
    ))
    
  }
  
  # selected columns not present in bds
  miss_clmn <- setdiff(col_select %>% unlist(), coln_bds)
  
  if(length(miss_clmn) > 0){
    
    miss_arg_values <- miss_clmn %>% paste(names(.), "=", .)
    miss_arg_names  <- names(miss_clmn)
    
    cli::cli_abort(c(
      'i' = 'You provided the column{?s} {.code {miss_arg_values}} for the spec creation.',
      'x' = 'The column{?s} {.code {miss_clmn}} {?is/are} not present in the data set.',
      '*' = 'Please correct the input of {.arg miss_arg_names}.'
    ))
    
  }
  
  
  purrr::iwalk(col_select, ~{
    if (!is.null(.x) && (length(intersect(.x, coln_bds)) == 0)) {
      
      usethis::ui_stop(crayon::silver(paste0(
        "The ", usethis::ui_code(.y), " column '", .x, "' is not available in the data set.\n")))
    }
  })
  
  #
  
  if(is.null(col_select[['label']])) col_select[['label']] <- col_select[['param']]
  
  # filter check ####
  
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(bds, filter, data_id = domain)$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]
  
  
  # OUTPUT ####
 
  out <- list(
    file      = file,
    data      = bds,
    md5       = md5,
    size      = size, 
    type      = "bds",
    filter    = actual_filter,
    spec_id   = domain
  ) %>% 
    append(
      col_select
    ) %>% 
    c(
      list(dupl_ctrl = list(
        values_fn = NULL,
        arrange   = NULL
      ))
    )

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



adam_spec_bds2 <- function(
    file        = NULL,
    data        = NULL,
    id          = 'SUBJID', 
    param       = NULL,
    label       = NULL,
    unit        = NULL,
    time        = NULL, 
    value       = NULL,
    filter      = NULL,
    attach_data = FALSE,
    domain      = NULL
){
  
  # initial check(s)  ####
  
  if (all(c(is.null(data), is.null(file)))) {
    usethis::ui_stop(
      paste0(
        'At least one of ', usethis::ui_code('data'), ' or ',
        usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  # remove?
  # if (!is.null(value_type) && !value_type %in% c("character", "numeric")){
  #   usethis::ui_stop(paste0(
  #     usethis::ui_code("value_type"), " needs to be either ",
  #     usethis::ui_value("numeric"), " or ", usethis::ui_value("character"), ".\n"
  #   ))
  # }
  
  # import ####
  if (is.null(data)) {
    imported <- import_info(file)
    data <- imported$data
    md5  <- imported$md5
    size <- imported$size
  }else{
    md5  <- NULL
    size <- NULL
  }
  
  
   col_select <- list(
     "id"    = id,
     "value" = value,
     "param" = param,
     "time"  = time,
     "unit"  = unit,
     "label" = label
   )
   
   check_role(col_select, required)
   
   check_filter()
   
   out
   
  # consolidate check_passed to use_for_build
  
  
  # identify domain and type ####
  # either from filename or check data for ADSNAME column
  ...
  
  coln_data <- colnames(bds)
  
  # column checks ####
  # required column set differs by type
  # function returning for type and coln_data:
  # col_select, col_required, guess
  type <- 'bds'
  if (type == 'bds') {
    col_select <- list(
      "id"    = id,
      "value" = value,
      "param" = param,
      "time"  = time,
      "unit"  = unit,
      "label" = label
    )
  } else if (type == 'occds') {
    col_select <- list(
      "id"    = id,
      ...
    )
  }
  col_misspecified <- setdiff(col_select, coln_data)
  
  if(length(col_misspecified) > 0) warn
  
  # guessing game ####
  ...
  
  
  
}

#' Import file and collect info
#'
#' @param file filepath
#'
#' @return  list containing data and corresponding md5 sum and file size

#' 
import_info <- function(file){
  # TODO maybe pass context
  if (!tools::file.exists(file)) {
    cli::cli_abort(c(
      #"{.fn adam_spec_bds} 
      "Could not create a spec from the provided file." ,
      'x' = "The following file could not be found: {.path {file}}")
    )
  }
  
  if (!tools::file_ext(file) %in% c("sas7bdat", "rds")) {
    cli::cli_abort(c(
      #"{.fn adam_spec_bds} 
      "expects a sas7bdat or rds file to read, but was provided {.path {file}}.",
      'x' = 'The provided file is of type {tools::file_ext(file)}.',
      '*' = 'Please check your input or attach a data set instead.'
    ))
    
  }
  
  file_ext <- tools::file_ext(file) 
  
  bds <- if (file_ext == 'sas7bdat') {
    haven::read_sas(file) %>% 
      dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
  }else if (file_ext == 'rds') {
    readRDS(file)
  }else{
    stop('Only sas7bdat and rds data supported.')
  }
  
  
  list(
    data = bds,
    md5  = tools::md5sum(file) %>% as.character(),
    size = fs::file_size(file)
  )
  
}


#' Check role specification for ADaM data set
#' 
#' Checks, if provided column for a role in an ADaM data set is present in the 
#' data and of the correct type. If no column is provided, it is guessed based 
#' on ADaM standards.
#'
#' @param colnames_data character vector. column names of the data set to check
#' against
#' @param role character. the role to check, e.g. "param", "id", "value" or
#' "time"
#' @param column_spec character. the selected column name. will be for 
#' presence in `data`and type. if NULL (the default), it will be guessed based 
#' on `domain` or `type`.
#' @param type character. either "adsl", "bds" or "occds"
#' @param domain character. the ADaM domain of `data`. used for column guessing
#' @param required boolean. `TRUE`, if `role` is required, `FALSE` if optional.
#' @param call the execution environment of a currently running function. 
#'
#' @return
#' A list with `role`, the column name `column` or `NULL` (if column check was 
#' not successful or `role` could not be guessed) and a boolean `check_pass`,
#' indicating, if all checks on the column have passed.
#' Throws an informative warning if any of the checks fails.
#' 

check_role <- function(
    data,
    role,
    column_spec = NULL,
    type,
    domain = NULL,
    required = TRUE,
    call = rlang::caller_env()
  ){
  
  out <- list(
    role = role,
    column = NULL,
    check_passed = TRUE
  )
  
  colnames_data <- colnames(data)
  
  if(!is.null(column_spec)){
    if(column_spec %in% colnames_data){
      # TODO check type
      out$column <- column_spec
    }else{
      out$check_passed <- FALSE
      cli::cli_warn(
        c("Column {.code {column_spec}} is not present in data."),
        call = call
      )
    }
  }else{
    # TODO guess based on 'domain' and 'role' (and 'type'?)
    guess <- NULL
    if(!is.null(guess) && required){
      out$check_passed <- FALSE
      cli::cli_warn(c(
        "No '{role}' column could be guessed from the data",
        "Please provide a column name for '{role}'."
        ),
        call = call
      )
    }
  }
  
  out
  
}


