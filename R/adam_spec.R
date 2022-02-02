#' Creating a specification for building a wide format data set from adam data
#' 
#' @description 
#' `r lifecycle::badge('maturing')`
#' 
#' \code{\link{adam_spec}()} is a wrapper for the `adam_spec_*()` functions.
#' It creates a list of specifications on how to extract and process data from adam data sets in a given location. 
#' The resulting list can be passed to \code{\link{build}()}, where the 
#' created specs are applied and the generated data sets are combined into a single wide format data set.  
#'
#' @param path path to a directory containing ads files
#' @param filter a character vector of conditions to be passed to \code{dplyr::filter()}, 
#' e.g. regarding visits, treatment arms or parameters. Defaults to NULL.
#' @param keep character vector defining the subset of data sets in the given \code{path} to create the specification for (e.g. \code{c('adsl', 'advs'))}).
#'  If both \code{keep} and \code{drop} are specified, \code{keep} overrides \code{drop}. Defaults to NULL.
#' @param drop character vector defining a subset of data sets in the given \code{path} to be 
#' excluded from the list of specifications (e.g. \code{'adqseq5d')}). Defaults to NULL.
#' @param attach_data boolean indicating whether the imported raw data is included in the output. Defaults to FALSE.
#' @param id,trt id and treatment column names (see e.g. \code{\link{adam_spec_adsl}()} for details).
#' @param pre_study boolean. Include only pre-study events from occurrence data sets (see \code{\link{adam_spec_occds}()} for details). Defaults to FALSE.
#' @param add_bds character vector of domain names of type bds that are not included in the package library of ADaM types (yet), but should be processed as per usual, e.g. 'adfapr' 
#' 
#' @details 
#' \code{adam_spec()} matches file names in the given path against an internal library
#' to decide on which `adam_spec_*()` function to use for which data set.
#' Only files in the library will be processed, the rest will be ignored. Names of unprocessed files will be printed to the console.
#' For those, specifications may be created manually using the appropriate `adam_spec_*()` function and appended to the specification list created by \code{adam_*_spec()}. 
#'
#' Individual filters are only applied if the resulting data set has a positive number of rows (ignoring those causing errors or yielding a 0-row data set). 
#'
#' Please refer to the documentations of the `adam_spec_*()` functions for full details.
#'
#' @return  
#' \code{adam_spec()} returns named list of specifications that can be passed to the \code{\link{build}()} function. 
#' Each element contains the specification for a single data set and is named with the domain abbreviation (e.g. adsl, adqskccq).
#' The list can be manually adjusted if required, e.g. adding further specifications or altering existing ones. See the documentation
#' of the `adam_spec_*()` for a detailed description of the output object.
#' 
#' @seealso \code{\link{adam_spec_adsl}()}, \code{\link{adam_spec_bds}()},  \code{\link{adam_spec_occds}()}
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export

adam_spec <- function(
  path, 
  filter      = NULL,
  keep        = NULL,
  drop        = NULL,
  pre_study   = FALSE,
  attach_data = FALSE,
  id          = "SUBJID",
  trt         = "TRT01A",
  add_bds     = NULL 
){
  
  # identify type for selected files in path (adsl/bds/occds) #####
  file_info <- adam_domain_type(path, keep, drop) %>% 
    {
      if(!is.null(add_bds)){ 
        dplyr::mutate(., type = dplyr::case_when(
          domain %in% add_bds ~ "bds",
          TRUE ~ type
        ))
      }else{.} 
    } %>%    
    dplyr::filter(type != "none")
  
  spec <- list()
  
  # adsl spec ####
  
  if ( any(file_info$type == "adsl") ){
    
    files_adsl <- file_info %>% 
      dplyr::filter(type == "adsl") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_adsl, ~ adam_spec_adsl(
          file = .x, id = id, trt = trt, filter = filter, attach_data = attach_data
        ))
      )
    
  }
  
  # bds spec ####
  
  if ( any(file_info$type == "bds") ){
    
    files_bds <- file_info %>% 
      dplyr::filter(type == "bds") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_bds, ~ adam_spec_bds(
          file = .x, id = id, filter = filter, attach_data = attach_data
        ))
      )
    
  }
  
  # occds spec ####
  
  if ( any(file_info$type == "occds") ){
    
    files_occds <- file_info %>% 
      dplyr::filter(type == "occds") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_occds, ~ adam_spec_occds(
          file = .x, id = id, filter = filter, attach_data = attach_data, pre_study = pre_study
        ))
      )
    
  }
  
  class(spec) <- c("martini_spec", class(spec))
  
  spec
  
}
