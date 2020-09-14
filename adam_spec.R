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

adam_spec <- function(
  path, 
  filter = NULL,
  keep   = NULL,
  drop   = NULL ,
  attach_data = FALSE){
  
  if(FALSE){
    # path = 'real_world_data/99999/'
   path = '//by-xa221/Statdb/Ginger/Studies/BAY106-7197_Neladenosone_99999_PANTHEON/Data/Original/ads/'
    filter = c("SEX == 'F'", "AVISIT == 'BASELINE'")
    
  }
  
  # library of data sets to be processed automatically
  type_adsl <- c("adsl") %>% 
    paste0(".sas7bdat$") %>% 
    paste(collapse = "|")
  type_bds <- c(
      paste0(c("adlb",  "advs",   "adxb", "adxl") , ".sas7bdat$"),
      "adqs.*[.]sas7bdat$"
    ) %>% 
    paste(collapse = "|")
  
  # list all files in given directory ####
  all_files <- list.files(path, pattern = ".sas7bdat", full.names = TRUE)
  doms      <- str_split( all_files, '/|\\\\')  %>%  
    map( ~ .[length(.)]) %>% 
    unlist() %>%
    str_remove('.sas7bdat')
  names(all_files) <- doms
    
  # subset according to user selection ####
  import_files <- all_files
  if(!is.null(keep)){
    import_files <- all_files[ intersect(keep, names(all_files))]
  }else{ 
    if (!is.null(drop)){
    import_files <- all_files[ names(all_files)[ !names(all_files) %in% drop]]
    }
  } 
  
  spec <- list()
  
  # adsl spec ####
  
  if ( any(str_detect(import_files, type_adsl)) ){
    
    files_adsl <- import_files[ str_detect(import_files, type_adsl) ]
    
    spec <- spec %>% 
      append(
        map(files_adsl, ~adam_spec_adsl(file = .x, filter = filter, attach_data = attach_data))
      )
    
  }
  
  # bds spec ####
  
  if (any(str_detect(import_files, type_bds))){
    
    files_bds <- import_files[ str_detect(import_files, type_bds) ] 
    
    spec <- spec %>% 
      append(
        map(files_bds, ~adam_spec_bds(file = .x, filter = filter, attach_data = attach_data))
      )
    
  }
  
  files_ignored <- import_files [ ! names(import_files)  %in%  names(c(files_adsl, files_bds))  ]
  
  
  if(length(files_ignored) >0 ){
    usethis::ui_info(paste0(
    'The following files were not processed as they are currently not in the library: \n', 
    paste(names(files_ignored), collapse=', '),
    '\nYou can use the adam_spec_*() functions as appropriate.')
    )
  }
  
  spec
}