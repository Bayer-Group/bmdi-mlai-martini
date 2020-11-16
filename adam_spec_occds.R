
#' spec occ 
#' 
#' @param file the sas file 
#' @param id name of id column to be kept and used for merge of data sets
#' @param param name of the column that identifies the parameter. Defaults to NULL, will be guessed if not set (see Details).
#' @param time Defaults to NULL, will be guessed if not set (see Details).
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data
#' 
#' @description 
#' For file names 'adae.sas7bdat', 'adcm.sas7bdat' and 'admh.sas7bdat', values for
#' arguments \code{param} and \code{time} will be guessed if not provided. 
#' Guesses will be the first of the following options that matches a column name (exact match).
#' @section \code{param}
#' \itemize{
#'      \item[adae] ...
#'      \item[adcm] ...
#'      \item[admh] ...
#' }
#' @section \code{time}
#' \itemize{
#'      \item[adae] ...
#'      \item[adcm] ...
#'      \item[admh] ...
#' }
#' Function will escape if one of param or value are neither provided nor can be guessed.

# function adam_spec_occds() ####
adam_spec_occ <- function(
  file,
  id     = 'SUBJID', 
  param  = NULL,
  time   = NULL,
  filter = NULL,
  pre    = FALSE,
  attach_data = FALSE,
  ...
){
  
  
  # test area  ####
  if(FALSE){
    
    require(tidyverse)
    require(haven)
    require(labelled)
    
    file = 'real_world_data/99999/admh.sas7bdat'
    id = 'SUBJID'
    param  =  NULL
    label  = NULL
    unit   = NULL # AVALU, xxSTRESU, ORESSU
    time   = NULL 
    value  = NULL #c(AVAL, CHG)
    filter = NULL
    
  }
  

  # READ occds ####
  occds <- haven::read_sas(file)
  
  # GUESS param ####
  if (!is.null(param)){
    guess_options <- adam_guess(file)$param
    param <- guess_options %>% 
      intersect(colnames(occds)) %>% 
      head(1)
    if (length(param) == 0){
      usethis::ui_stop(
        paste0("Parameter '", key, "' needs to be provided.\n")
      )
    }
  }
  
  

  
  
  

  
}
