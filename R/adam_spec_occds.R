#' Create specification object for adam data sets of type `occds`
#'
#' Given a file containing a occds data set (e.g. admh or adcm), \code{\link{adam_spec_occds}()}
#' will create a specification object for use in \code{\link{build_occds}()} to prepare the data 
#' to be used in machine learning. The main task is to collect the key columns for reshaping the 
#' data into wide format and prepare the data filter.     
#'
#' @param file the path of the sas file to process
#' @param id name of id column to be kept and used for merge of data sets
#' @param label name of the column that identifies the occurrence labels. Defaults to NULL, will be guessed if not set (see Details). 
#' @param time name of the column that is used for time filtering. Defaults to NULL, will be guessed if not set (see Details).
#' @param value optional value column (e.g. AE severity). Defaults to NULL, which leads to an Y/N coding of the event
#' @param valuen optional numeric coding column for `value`. Defaults to NULL, ignored if `value` is NULL.
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param count  boolean, defaults to FALSE. 
#' @param pre_study  boolean. filter the data set to pre_study observations based on non-negative values in `time`
#' @param attach_data boolean. attach the imported raw data in \code{data} slot of output object
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{`data_info`}{a list containing the number of subjects `nsubj` and columns `ncol` in the data after applying `filter`}
#' \item{`type`}{character string \code{occds}, generally giving the type of adam data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields valid and non-empty result when applied individually (using \code{\link{check_filter}())}}
#' \item{`id`}{passing unchanged input}  
#' \item{`label`, `value`, `valuen`}{names of the key columns to be used in \code{\link{build_occds}()} for reshaping}
#' \item{`spec_id`}{character string, generally the name of the domain} 
#' \item{`dict`}{a tibble with unique combinations within the `param` and `label` column (if present in the data set) to be used as a data dictionary}
#' 
#' @details 
#' For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat', values for
#' arguments \code{label} and \code{time} will be guessed if not provided. 
#' Please refer to \code{adam_guess()} for details on guessing procedure.  
#' Function will escape if one of label or value are neither provided nor can be guessed.
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
  time        = NULL,
  value       = NULL,
  valuen      = NULL,
  filter      = NULL,
  count       = TRUE, # NOTE: add further options (weights, scoring matrix, ...)
  pre_study   = FALSE,
  attach_data = FALSE
){
  
  
  # READ occds ####
  occds      <- haven::read_sas(file) %>% 
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

  
  md5        <- tools::md5sum(file) %>%  as.character()
  size       <- fs::file_size(file)
  
  guesses    <- adam_guess(file)
  coln_occds <- colnames(occds)
  domain     <- stringr::str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% 
    stringr::str_remove_all('^ad|[.]sas7bdat$') %>% 
    stringr::str_to_upper()
  
  # check input validity ####
  # ... mandatory columns ####
  if (! id %in% coln_occds){
    usethis::ui_stop(
      paste0("The column id = ", id, " is not present in the data set.\n")
    )
  }
  
  if (!is.null(label)) if (! label %in% coln_occds){
    usethis::ui_stop(
      paste0("The column label = ", label, " is not present in the data set.\n")
    )
  } 
  
  # ... optional columns ####
  if (!is.null(value)) if( !value  %in% coln_occds){ 
    usethis::ui_info(paste0("'", value,  "' not found in data set and ignored.\n"))
  }
  
  if (!is.null(valuen)) if( !valuen  %in% coln_occds){ 
    usethis::ui_info(paste0("'", valuen, "' not found in data set and ignored.\n"))
  }
  
  
  # GUESS label ####
  if (is.null(label)){
    
    label <- guesses$label %>% 
      intersect(coln_occds) %>% 
      head(1)
    
    if (length(label) == 0){
      usethis::ui_stop(
        "Parameter 'label' needs to be provided.\n"
      )
    }
  }
  
  # GUESS time ####
  if (is.null(time) && pre_study){
    
    time <- guesses$time %>% 
      intersect(coln_occds) %>% 
      head(1)
    
    if (length(time) == 0){
      usethis::ui_stop(
        "Parameter 'time' could not be guessed and needs to be provided to build the `pre_study` filter.\n"
      )
    }
  }
  
  # if requested, build and add pre-study filter ####
  if(pre_study){
    if(!time %in% coln_occds) usethis::ui_stop('pre_study filter could not be built. The provided parameter "time" is not present in the data.')
    filter_time <- paste0( time , ' < 0 | is.na(', time, ')')
    filter      <- filter %>% append(filter_time)
  }      
  
  # filter check ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(occds, filter, data_id = domain)$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]

  
  # dictionary   ####
  
  # use unfiltered data 
  dict  <- occds %>% 
    dplyr::select( label = !!rlang::sym(label) ) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(source = domain) %>% 
    dplyr::mutate(type   = 'occds') 
  
  # remove occds data set label automatically created by haven::read_sas()
  attr(dict, 'label') <- NULL
  
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
  
  # create data info ####
  
  data_info <- list(
    nsubj = occds %>% 
      dplyr::filter(!!! rlang::parse_exprs(actual_filter)) %>% 
      dplyr::select(tidyselect::all_of(id)) %>% 
      dplyr::n_distinct(),
    ncol  = dict %>% nrow()
  )
  
  # output ####
  
  out <- list(
    file      = file,
    md5       = md5,
    size      = size, 
    data      = NULL,
    data_info = data_info,
    type      = "occds",
    id        = id,
    filter    = actual_filter,
    count     = count,
    dict      = dict,
    spec_id   = domain
  ) %>% 
    append(
      col_select %>% as.list()
    )
  
  
  if(attach_data){
    out$data <- occds
  }
  
  out

}


# test area  ####
if(FALSE){

  file      = '../admh.sas7bdat'

  adam_spec_occds(file = file)
  adam_spec_occds(file = file, value = "MHPRESP")

}
