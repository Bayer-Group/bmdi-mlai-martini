#' Create specification object for AdaM data sets of type `occds`
#'
#' Given a file containing a occds data set (e.g. admh or adcm), \code{\link{adam_spec_occds}()}
#' will create a specification object for use in \code{\link{build_occds}()} to prepare the data 
#' to be used in machine learning. The main task is to collect the key columns for reshaping the 
#' data into wide format and prepare the data filter.     
#'
#' @param file the path of the sas(7bdat) or rds file to process
#' @param id name of id column to be kept and used for merge of data sets
#' @param label name of the column that identifies the occurrence labels. Defaults to NULL, will be guessed if not set (see Details). 
#' @param time name of the column that is used for time filtering (if required via \code{pre_study} argument). Defaults to NULL, will be guessed if not set (see Details).
#' @param value optional value column (e.g. AE severity). Defaults to NULL, which leads to an Y/N coding of the event.
#' @param valuen optional numeric coding column for `value`. Defaults to NULL, ignored if `value` is NULL.
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param count boolean, defaults to FALSE. 
#' @param pre_study boolean. filter the data set to pre_study observations based on non-negative values in `time`
#' @param attach_data boolean. attach the imported raw data in \code{data} slot of output object
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{`data_info`}{a list containing the number of subjects `nsubj` and columns `ncol` in the data after applying `filter`}
#' \item{`type`}{character string \code{occds}, generally giving the type of AdaM data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields valid and non-empty result when applied individually (using \code{\link{check_filter}())}}
#' \item{`id`}{passing unchanged input}  
#' \item{`label`, `value`, `valuen`}{names of the key columns to be used in \code{\link{build_occds}()} for reshaping}
#' \item{`spec_id`}{character string, generally the name of the domain} 
#' \item{`dict`}{a tibble with unique combinations within the `param` and `label` column (if present in the data set) to be used as a data dictionary}
#' 
#' @details 
#' For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat', values for
#' arguments \code{label} will be guessed if not provided, the same goes for \code{time} if `pre_study=TRUE`. 
#' Please refer to \code{adam_guess()} for details on guessing procedure.  
#' Function will exit if \code{label} is neither provided nor can be guessed.
#' If a pre-study filter is requested, the function will escape if \code{time} is neither provided nor can be guessed. 
#' Note that the original values in the \code{label} column will end up being the parameter labels, 
#' not the parameters in the ML feature matrix. These might be modified later using \code{make.names()} or the like in \code{prepare_ml()}.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 


adam_spec_occds <- function(
  file,
  id          = 'SUBJID', 
  label       = NULL,
  value       = NULL,
  valuen      = NULL,
  filter      = NULL,
  count       = TRUE, # NOTE: add further options (weights, scoring matrix, ...)
  time        = NULL,
  pre_study   = FALSE,
  attach_data = FALSE
){
  
  
  # initial check(s)  ####
  
  if (all(c(is.null(data), is.null(file)))) {
    usethis::ui_stop(
      paste0(
        'At least one of ', usethis::ui_code('data'), ' or ',
        usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  # import ####
  if (is.null(data)) {
    imported <- import_info(file)
    data   <- imported$data
    md5    <- imported$md5
    size   <- imported$size
    domain <- basename(file) %>% tools::file_path_sans_ext()
  }else{
    md5    <- NULL
    size   <- NULL
    domain <- domain %||% "custom"
  }
  
  # check input validity ####
  
  # collect column name parameters ####
  col_spec <- list(
    "id"     = list(column = id,     required = TRUE),
    "label"  = list(column = label,  required = TRUE),
    "value"  = list(column = value,  required = TRUE),
    "valuen" = list(column = valuen, required = FALSE),
    # only used for pre_study filter (to be deprecated)
    "time"   = list(column = time,   required = FALSE)
  )
  
  col_select_raw <- purrr::imap(col_spec, ~{
    check_role(
      data = data, 
      role = .y, 
      column_spec = .x$column, 
      required = .x$required,
      type = "bds", 
      call = rlang::caller_env(n = 4)
    )
  })
  
  col_select <- purrr::map(col_select_raw, "column")
  
  use_for_build <- purrr::map_lgl(col_select_raw, "check_passed") %>% all()
  

  
  # GUESS time ####
  if (is.null(col_select$time) && pre_study){
    
    if (length(time) == 0){
      usethis::ui_stop(
        "Parameter 'time' could not be guessed and needs to be provided to build the `pre_study` filter.\n"
      )
    }
  }
  
  # if requested, build and add pre-study filter ####
  if(pre_study){
    
    if (is.null(col_select$time)) {
      cli::cli_warn('pre_study filter could not be built. The provided parameter "time" is not present in the data.')
    }else{
      filter_time <- paste0( time , ' < 0 | is.na(', time, ')')
      filter      <- filter %>% append(filter_time)
    }
    
  }      
  
  # filter check ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(occds, filter, data_id = domain)$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]

  # collect key columns ####
  col_select <- c(label = label)
  if(!is.null(value)) {
    col_select <- c(col_select, value = value)
    
    if (!is.null(valuen)){
      col_select <- c(col_select, valuen = valuen)
    } else if (paste0(value, "N") %in% coln_occds ){ # works only for nchar(value) < 8 and standard coding
      col_select <- c(col_select, valuen = paste0(value, "N"))
    }
  }
  
  # output ####
  
  out <- list(
    file      = file,
    md5       = md5,
    size      = size, 
    data      = occds,
    type      = "occds",
    id        = id,
    filter    = actual_filter,
    count     = count,
    spec_id   = domain
  ) %>% 
    append(
      col_select %>% as.list()
    )
  
  # create data info and dictionary####
  out$dict <- create_dict(out)
  out$data_info <- data_info(out)
  

  
  if(!attach_data){
    # only keep data, if 'attach_data = TRUE'
    # (was needed to create data info)
    out$data <- NULL
  }
  
  out

}


# test area  ####
if(FALSE){

  file      = '../admh.sas7bdat'

  adam_spec_occds(file = file)
  adam_spec_occds(file = file, value = "MHPRESP")

}
