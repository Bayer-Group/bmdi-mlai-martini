#' @title adam spec
#' 
#' \code{adam_spec()} is a wrapper for the \code{adam_spec_*()} functions.
#' It creates a list of specifications on how to extract and process data from adam data sets in a given location. 
#' The resulting list can be passed to \code{build()}, where the data sets are combined into a single wide format data set.
#'
#' @param path the path to the ads files
#' @param filter a character vector of conditions to be passed to \code{dplyr::filter()}, e.g. regarding visits, treatment arms or parameters. Defaults to NULL.
#' @param keep character vector defining the subset of data sets in the given `path` to create the specification for (e.g. \code{c('adsl', 'advs'))}).
#'  If both \code{keep} and \code{drop} are specified, \code{keep} overrides \code{drop}. Defaults to NULL.
#' @param drop character vector defining a subset of data sets in the given `path` to be excluded from the list of specifications (e.g. \code{'adqseq5d')}). Defaults to NULL.
#' @param attach_data boolean. attach the imported raw data
#' 
#' @description  \code{adam_spec()} matches file names in the given path against an internal library to decide on which \code{adam_*_spec()} function to use for which data set.
#'  Only files in the library will be processed, the rest will be ignored. Names of unprocessed files will be printed to the console.
#'  For those, specifications may be created manually using the appropriate \code{adam_spec_*()} function and appended to the specification list created by \code{adam_*_spec()}. 
#'
#' Individual filters are only applied if the resulting data set has a positive number of rows (ignoring those causing errors or yielding a 0-row data set). 
#'
#' Please refer to the documentations of the \code{adam_spec_*()} functions for full details.
#'
#' @return  \code{adam_spec()} returns named list of specifications that can be passed to the \code{adam_prep()} function. 
#'         Each element contains the specification for a single data set and is named with the domain abbreviation (e.g. adsl, adqskccq).
#'         The list can be manually adjusted if required, e.g. adding further specifications or altering existing ones.
#' 
#' @seealso \code{\link{adam_spec_adsl()}}, \code{\link{adam_spec_bds()}}
#'
#' @usage 
#' 

library(crayon)

adam_spec <- function(
  path, 
  filter      = NULL,
  keep        = NULL,
  drop        = NULL,
  pre_study   = FALSE,
  attach_data = FALSE){
  
  if(FALSE){
    # path = 'real_world_data/99999/'
   path = '//by-xa221/Statdb/Ginger/Studies/BAY106-7197_Neladenosone_99999_PANTHEON/Data/Original/ads/'
    filter = c("SEX == 'F'", "AVISIT == 'BASELINE'")
    
  }
  
  file_info <- adam_domain_type(path, keep, drop)
  
  spec <- list()
  
  # adsl spec ####
  
  if ( any(file_info$type == "adsl") ){
    
    files_adsl <- file_info %>% 
      dplyr::filter(type == "adsl") %>% 
      dplyr::select(dom, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_adsl, ~ adam_spec_adsl(file = .x, filter = filter, attach_data = attach_data))
      )
    
  }
  
  # bds spec ####
  
  if ( any(file_info$type == "bds") ){
    
    files_bds <- file_info %>% 
      dplyr::filter(type == "bds") %>% 
      dplyr::select(dom, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_bds, ~ adam_spec_bds(file = .x, filter = filter, attach_data = attach_data))
      )
    
  }
  
  # occds spec ####
  
  if ( any(file_info$type == "occds") ){
    
    files_occds <- file_info %>% 
      dplyr::filter(type == "occds") %>% 
      dplyr::select(dom, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_occds, ~ adam_spec_occds(file = .x, filter = filter, attach_data = attach_data, pre_study = pre_study))
      )
    
  }
  
  spec
}