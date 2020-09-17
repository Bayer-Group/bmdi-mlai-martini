
#' spec bds (basic data structure)
#' 
#' @param file the sas file 
#' @param id name of id column to be kept and used for merge of data sets
#' @param param name of the column that identifies the parameter. Defaults to NULL, will be guessed if not set (see Details).
#' @param label name of the column that gives column labels. Defaults to NULL.
#' @param unit Defaults to NULL, will be guessed if not set (see Details).
#' @param time Defaults to NULL, will be guessed if not set (see Details).
#' @param value Defaults to NULL, will be guessed if not set (see Details).
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data
#' 
#' @description 
#' Values for arguments param, label, unit, time, value will be guessed if not provided. 
#' Guess will be the first of the following options that matches a column name (exact match).
#' \itemize{
#'      \item[param] 'PARAMCD', paste0(dom,'TESTCD')
#'      \item[label] substring of param with the last two characters removed
#'      \item[time]  'AVISIT', 'VISIT'
#'      \item[value] 'AVAL',   paste0(dom, c("STRESN", "ORRES")
#'      \item[unit]  'AVALU',  paste0(dom, c("STRESU", "ORRESU") 
#' }
#' Function will escape if one of param or value are neither provided nor can be guessed.
#' A parameter dictionary will be created: A tibble with unique combinations of param, label, unit (or the provided subset)


# function adam_spec_bds() ####
adam_spec_bds <- function(
  file,
  id = 'SUBJID', 
  param  = NULL , # 'PARAMCD',
  label  = NULL,  # PARAMLAB
  unit   = NULL, # AVALU, xxSTRESU, ORESSU
  time   = NULL, 
  value  = NULL, #c(AVAL, CHG)
  filter = NULL,
  attach_data = FALSE,
  ...
){
  
  
  # test area  ####
  if(FALSE){
    
    require(tidyverse)
    require(haven)
    require(labelled)
    
    file = 'real_world_data/99999/adegf.sas7bdat'
    id = 'SUBJID'
    param  =  NULL
    label  = NULL
    unit   = NULL # AVALU, xxSTRESU, ORESSU
    time   = NULL 
    value  = NULL #c(AVAL, CHG)
    filter = NULL
    
  }
  
  #  read bds ####
  #'adsl.sas7bdat' %in% list.files(path)
  bds <- haven::read_sas(file)
  
  
  #  guess stuff ...####
  
  
  # ... guess domain ####
  dom <- stringr::str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% 
    stringr::str_sub(3,4) %>%  # rm trailing 'ad'
    # str_split('[:punct:]') %>% 
    # .[[1]] %>% 
    # .[1] %>%  
    stringr::str_to_upper()
  
  guesses <- list(
    # ... guess param ####
    param = c('PARAMCD', paste0(dom,'TESTCD')),
    
    # ... guess visit  ####
    time = c('AVISIT', 'VISIT'),
    
    # ... guess value  ####
    value = c(ifelse(stringr::str_split( file, '/|\\\\')[[1]] %>% 
                       tail(1) %>% 
                       stringr::str_remove('.sas7bdat$') %>%  {. %in%  c('adegf')}, 
                     'AVALC', 'AVAL'),  
              paste0(dom, "STRESN" ), paste0(dom, "ORRES")),
    
    # ... guess unit ####
    unit = c('AVALU',  paste0(dom, 'STRESU'),  paste0(dom,'ORRESU'))
    
  )
  
  guesses$label <- stringr::str_sub(guesses$param, 1, -3)
  
  col_select <- c(
    "value" = value,
    "param" = param,
    "time"  = time,
    "unit"  = unit,
    "label" = label
  )
  
  col_required <- c('value', 'param')
  
  coln_bds     <- colnames(bds)

  for (i in 1:length(guesses)){ # i=4
    
    col_i      <- col_select[i]
    name_col_i <- names(guesses)[i]
    
    if (is.null(col_i) || !(col_i %in% coln_bds)){
      choices <- guesses[[name_col_i]] %>% 
        intersect(coln_bds)
      
      # escape if required columns cannot be identified
      if (length(choices) == 0 && 
          (name_col_i %in% col_required) ) return(NULL)
      
      col_select[i] <- choices[1]
    }
    
    names(col_select)[i] <- name_col_i
    
  }
  
  # filter check ####
  
  keep_filter <- purrr::map_lgl(filter, function(x){
    try_it <- try(
      {bds %>% dplyr::filter(!! rlang::parse_expr(x))},
      silent = TRUE
    )
    is_error <- "try-error" %in% class(try_it)
    is_norow <- FALSE
    if (!is_error) is_norow <- nrow(try_it) == 0
    !(is_error || is_norow)
    
  })
  
  actual_filter <- filter[keep_filter]

 
  # dictionary
  source <- stringr::str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% stringr::str_remove('.sas7bdat')
  
  # use unfiltered data 
  dict  <- bds %>% 
    dplyr::select( tidyselect::any_of(
      c("param" = col_select[['param']], 
        "label" = col_select[['label']], 
        "unit"  = col_select[['unit']]) %>%  na.omit)) %>% 
    distinct() %>%
    dplyr::mutate(source = source)
 
 
 
 
 
  # output ####
 
  out <- list(
    file = file,
    data = NULL,
    type = "bds",
    id = id,
    filter = actual_filter,
    dict = dict,
    spec_id = source
  ) %>% 
    append(
      col_select %>% as.list()
    )

  
  if(attach_data){
    out$data <- bds
  }
  
  out
  
}
