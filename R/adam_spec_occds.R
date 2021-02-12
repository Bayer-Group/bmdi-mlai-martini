#' Create spec object for occurrence data sets 
#' 
#' 
#' @param file the path of the sas file to process
#' @param id name of id column to be kept and used for merge of data sets
#' @param label name of the column that identifies the occurrence labels. Defaults to NULL, will be guessed if not set (see Details). 
#' @param time name of the column that is used for time filtering. Defaults to NULL, will be guessed if not set (see Details).
#' @param value optional value column (e.g. AE severity). Defaults to NULL, which leads to an Y/N coding of the event
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param count  boolean, defaults to FALSE. # NOTE: add further options (weights, scoring matrix, ...)
#' @param pre_study   = FALSE, 
#' @param attach_data boolean. attach the imported raw data in \code{data} slot of output object
#' 
#' @description 
#' For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat', values for
#' arguments \code{label} and \code{time} will be guessed if not provided. 
#' Please refer to \code{adam_guess()} for details on guessing procedure.  
#' Function will escape if one of label or value are neither provided nor can be guessed.
#' Note that the original values in the \code{label} column will end up being the parameter labels, 
#' not the parameters in the ML feature matrix. These might be modified later using \code{make_names()} or the like \code{prepare_ml()}.

# function adam_spec_occds() ####
adam_spec_occds <- function(
  file,
  id          = 'SUBJID', 
  label       = NULL,
  time        = NULL,
  value       = NULL,
  filter      = NULL,
  count       = TRUE, # NOTE: add further options (weights, scoring matrix, ...)
  pre_study   = FALSE,
  attach_data = FALSE
){
  
  
  # test area  ####
  if(FALSE){
    
    require(tidyverse)
    require(haven)
    require(labelled)
    
    file   = '../admh.sas7bdat'
    id     = 'SUBJID'
    label  = NULL
    time   = NULL 
    value  = NULL 
    filter = NULL
    pre_study = TRUE
    count  = TRUE    }
  

  # READ occds ####
  occds <- haven::read_sas(file)
  
  md5 <- tools::md5sum(file) %>%  as.character()
  
  # GUESS label ####
  if (is.null(label)){
    guess_options <- adam_guess(file)$label
    label <- guess_options %>% 
      intersect(colnames(occds)) %>% 
      head(1)
    if (length(label) == 0){
      usethis::ui_stop(
        paste0("Parameter '", "label", "' needs to be provided.\n")
      )
    }
  }
  
  # GUESS time ####
  if (is.null(time) && pre_study){
    guess_options <- adam_guess(file)$time
    time <- guess_options %>% 
      intersect(colnames(occds)) %>% 
      head(1)
    if (length(time) == 0){
      usethis::ui_stop(
        paste0("Parameter '", "time", "' needs to be provided.\n")
      )
    }
  }
  
  # if requested, build and add pre-study filter
  if(pre_study){
    if(!time %in% colnames(occds)) usethis::ui_stop('pre_study filter could not be built.')
    filter_time <- paste0( time , ' < 0 | is.na(', time, ')')
    filter      <- filter %>%  append(filter_time)
  }      
  
  
  
  
  # filter check ####
  
  keep_filter <- purrr::map_lgl(filter, function(x){
    try_it <- try(
      {occds %>% dplyr::filter(!! rlang::parse_expr(x))},
      silent = TRUE
    )
    is_error <- "try-error" %in% class(try_it)
    is_norow <- FALSE
    if (!is_error) is_norow <- nrow(try_it) == 0
    !(is_error || is_norow)
    
  })
  
  actual_filter <- filter[keep_filter]

  
  
  # dictionary   ####
  source <- adam_domain_type(file)$domain
  
   
  # use unfiltered data 
  dict  <- occds %>% 
    dplyr::select( label = !!rlang::sym(label) ) %>% 
    distinct() %>%
    dplyr::mutate(source = source) %>% 
    dplyr::mutate(type   = 'occds') 
  
  
  
  col_select <- c('label' = label)
  if(!is.null(value)) {
    if (value %in% colnames(occds)){
      col_select <- c(col_select, value = value)
      if (paste0(value, "N") %in% colnames(occds) ||
          paste0(labelled::var_label(occds)$value, " (N)") %in% labelled::var_label(occds) ){
        col_select <- c(col_select, valuen = paste0(value, "N"))
      }
    } else {
      usethis::ui_info(paste0("'", value, "' not found in data set and ignored.\n"))
    }
  }
  
  # output ####
  
  out <- list(
    file    = file,
    data    = NULL,
    md5     = md5,
    type    = "occds",
    id      = id,
    filter  = actual_filter,
    count   = count,
    dict    = dict,
    spec_id = source
  ) %>% 
    append(
      col_select %>% as.list()
    )
  
  
  if(attach_data){
    out$data <- occds
  }
  
  out
  
  

  
}
