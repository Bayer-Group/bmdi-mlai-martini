#' identify the data set type (ads, bds or occds) of ads files by file name
#' 
#' \code{adam_domain_type()} returns the look up table that is used for determining the type of an ads data set. 
#' Note that this table contains regular expressions when searching for a particular domain 
#' (\code{adqs.*} will match e.g. adqskccq and adqsnyha).
#' 
#' Files are read from the given \code{path} and file names are matched to their corresponsing type (ads, bds or occds) using a look up table. 
#' This information is e.g. used to determine which versions of \code{adam_spec_*} and \code{build_*} to use for further processing
#' Parameters \code{keep} and \code{drop} allow control over which files to use and ignore, resp. 
#' (If both are provided files are kept if they are \code{kept} but not in \code{drop}.)
#'
#' @param path ads path 
#' @keep only keep the domains provided, e.g. \code{keep = 'adsl'}
#' @drop exclude the domains provided, e.g. \code{drop = 'adxb'} 

adam_domain_type <- function(
  path = NULL , 
  keep = NULL, 
  drop = NULL){
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # library of data sets to be processed automatically   ####
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  type_adsl <- c("adsl") %>% 
    paste0(".sas7bdat$") 
  type_bds <- c(   
    paste0( c(  "adegf",   "adpc",
                "adlb",  "advs",  "adxb", "adxl", 
                "adqskccq", "adqsnyha", 'adqseq5d') , ".sas7bdat$"),
    "adqs.*[.]sas7bdat$" ) #%>% 
  type_occds <- c("adae", "adcm", "admh") %>% 
    paste0(".sas7bdat$")
  
  ads_library <- bind_rows(
    tibble(domain = type_adsl)  %>%  mutate(`adam_spec_*` = 'adsl' ) ,
    tibble(domain = type_bds)   %>%  mutate(`adam_spec_*` = 'bds'  ) ,
    tibble(domain = type_occds) %>%  mutate(`adam_spec_*` = 'occds') 
  )  %>%  
    mutate_at('domain',~  stringr::str_remove_all(.x, '(\\[.\\]|[.])sas7bdat\\$'))
  
  type_adsl  <- type_adsl  %>%  paste(collapse = "|")
  type_bds   <- type_bds   %>%  paste(collapse = "|")
  type_occds <- type_occds %>%  paste(collapse = "|")
  
  
  if(is.null(path)){ 
    return(ads_library) }
  else{
  
      all_files <- list.files(path, pattern = ".sas7bdat", full.names = TRUE, recursive = TRUE)
      
      # if length == 0, 'path' might be a single file
      if (length(all_files) == 0) all_files <- path
      
      
      file_name  <- stringr::str_split(all_files, '/|\\\\')  %>%  
        purrr::map( ~ .[length(.)]) %>% 
        unlist()
      
      all_types <- purrr::map_chr(file_name, ~{
        if (stringr::str_detect(., type_adsl)){
          "adsl"
        } else if (stringr::str_detect(., type_bds)){
          "bds"
        } else if (stringr::str_detect(., type_occds)){
          "occds"
        } else {
          "none"
        }
      })
      
      all_doms  <- stringr::str_split( all_files, '/|\\\\')  %>%  
        purrr::map( ~ .[length(.)]) %>% 
        unlist() %>%
        stringr::str_remove('.sas7bdat')
      
      file_info <- tibble::tibble(
        "file" = all_files,
        "type" = all_types,
        "dom"  = all_doms
      )
      
      if (!is.null(keep)){
        file_info <- file_info %>% dplyr::filter(dom %in% keep)
      } else {
        if(!is.null(drop)){
          file_info <- file_info %>% dplyr::filter(!dom %in% drop)
        }
      }
      
      if(nrow(file_info) == 0){
        usethis::ui_stop("No files to process")
      }
      
      doms_ignored <- file_info %>% 
        dplyr::filter(type == "none") %>% 
        dplyr::pull(dom)
      
      if(length(doms_ignored) > 0 ){
        usethis::ui_info( paste0(
          crayon::silver('The following domains were not processed as they are currently not in the library: \n\t'), 
          crayon::blue(paste(doms_ignored, collapse=', ')),
          crayon::silver( '\nYou can use the adam_spec_*() functions as appropriate.'))
        )
      }
      
      if(all(file_info$type == "none")){
        usethis::ui_stop("No supported files in selected path.")
      }
      
      file_info
  }   
}
