#' Create specification object for ADaM data sets of type `occds`
#'
#' Given a file containing a occds data set (e.g. admh or adcm), 
#' \code{\link{adam_spec_occds}()}
#' will create a specification object for use in \code{\link{build_occds}()}
#'  to prepare the data 
#' to be used in machine learning. The main task is to collect the key columns
#'  for reshaping the 
#' data into wide format and prepare the data filter.  
#'    
#' @param data tibble with the data in occds format for which the specification
#'  is created
#' @param file the path of the sas(7bdat) or rds file to process, 
#' ignored if `data` is provided
#' @param id name of id column to be kept and used for merge of data sets
#' @param label name of the column that identifies the occurrence labels. 
#' Defaults to NULL, will be guessed if not set (see Details). 
#' @param value optional value column (e.g. AE severity). Defaults to NULL, 
#' which leads to an Y/N coding of the event.
#' @param valuen optional numeric coding column for `value`. Defaults to NULL,
#'  ignored if `value` is NULL.
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has 
#' positive number of rows. Defaults to NULL. 
#' @param count boolean, defaults to FALSE. 
#' @param attach_data boolean. attach the imported raw data in \code{data} 
#' slot of output object
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the 
#' generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, `NULL` otherwise}
#' \item{`data_info`}{a list containing the number of subjects `nsubj` and 
#' columns `ncol` in the data after applying `filter`}
#' \item{`type`}{character string \code{occds}, generally giving the type of 
#' ADaM data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields valid and non-empty 
#' result when applied individually (using \code{\link{check_filter}())}}
#' \item{`id`}{passing unchanged input}  
#' \item{`label`, `value`, `valuen`}{names of the key columns to be used in
#'  \code{\link{build_occds}()} for reshaping}
#' \item{`spec_id`}{character string, generally the name of the domain} 
#' \item{`dict`}{a tibble with unique combinations within the `param` and 
#' `label` column (if present in the data set) to be used as a data dictionary}
#' 
#' @details 
#' For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat', 
#' values for
#' arguments \code{label} will be guessed if not provided.
#' Please refer to \code{adam_guess()} for details on guessing procedure.  
#' Function will exit if \code{label} is neither provided nor can be guessed.
#' Note that the original values in the \code{label} column will end up being 
#' the parameter labels, 
#' not the parameters in the ML feature matrix. These might be modified later 
#' using \code{make.names()} or the like in \code{prepare_ml()}.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 


adam_spec_occds <- function(
    file        = NULL,
    data        = NULL,
    id          = "USUBJID", 
    label       = NULL,
    value       = NULL,
    valuen      = NULL,
    filter      = NULL,
    count       = TRUE, # NOTE: add further options (weights, scoring matrix, ...)
    attach_data = FALSE
){
  
  
  # initial check(s)  ####
  
  if (all(c(is.null(data), is.null(file)))) {
    usethis::ui_stop(
      paste0(
        'At least one of ', usethis::ui_code('data'), ' or ',
        usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  # deprecation ####
  # if (lifecycle::is_present(pre_study)) {
  #   
  #   # Signal the deprecation to the user
  #   lifecycle::deprecate_warn(
  #     "0.7.0", 
  #     "adam_spec_occds(pre_study = )", 
  #     "adam_spec_occds(filter = )"
  #   )
  #   
  #   # Deal with the deprecated argument for compatibility
  #   pre_study <- FALSE
  # }
  
  # import ####
  if (is.null(data)) {
    imported <- import_info(file)
    data     <- imported$data
    md5      <- imported$md5
    size     <- imported$size
    domain   <- basename(file) %>% tools::file_path_sans_ext()
  }else{
    md5    <- NULL
    size   <- NULL
    domain <- domain %||% "custom"
  }
  
  if(!is.null(value) && is.null(valuen)){
    # works only for nchar(value) < 8 and standard coding
    if(paste0(value, "N") %in% colnames(data)){
      valuen <- paste0(value, "N")
    }
  }
  
  # collect column name parameters ####
  prepared_cols <- prepare_col_selection(
    data = data, 
    id, label, value, valuen, 
    type = c("occds"), 
    call = rlang::caller_env(n = 5)
  )
  
  use_for_build <- prepared_cols$use_for_build
  col_select    <- prepared_cols$col_select
  
  # filter check ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(data, filter, data_id = domain)$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]
  
  # --OCCUR check ####
  check_occds_occur(
    data = data,
    filters = actual_filter,
    domain = domain
  )
  
  # output ####
  
  create_spec_out(
    file, data, md5, size, actual_filter, domain, col_select, count,
    use_for_build, type = "occds", attach_data = attach_data
  )

}

