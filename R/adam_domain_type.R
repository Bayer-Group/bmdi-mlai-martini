#' Identify the data set type of ads files by file name
#' 
#' Files are read from the given \code{path} and file names are matched
#' to their corresponding adam data type (ads, bds or occds) using a look up table. 
#'
#' @param path ads path to the file of interest
#' @param keep only keep the domains provided, e.g. \code{keep = 'adsl'}
#' @param drop exclude the domains provided, e.g. \code{drop = 'adxb'} 
#' @param quiet whether to suppress printing info on unknown domains to the console, defaults to \code{FALSE}
#' 
#' @details
#' 
#' The derived information is e.g. used to determine which version of `adam_spec_*()` and `build_*()`
#' to use for further processing.
#' Parameters \code{keep} and \code{drop} allow control over which files to use and ignore, resp. 
#' (If both are provided, \code{drop} is ignored and only information in \code{keep} is used.)
#' 
#' Without any arguments given, *\code{adam_domain_type()}* returns the look up table that is used
#' for determining the type of an ads data set (ads, bds or occds). The column \code{domain} does not only contain 
#' explicit domains (e.g. `adqskccq`) for human readability, but also regular expressions 
#' (`adqs.*` matches e.g. `adqskccq`, `adqsnyha`, `adqseq5d`, `adqspad`, ...)
#' 
#' 
#' 
#' @return 
#' 
#' A tibble with one row for each matched \code{.sas7bdat} file in the specified folder and the following columns
#' 
#' \item{file}{File path of the individual selected files}
#' \item{type}{File type: *adsl*, *bds*, *occds* or *none* (if no matches are found in the look up table, see \code{adam_domain_type()})}
#' \item{domain}{Name of the adam domain, i.e. the file name without its extension}
#' 
#' If unknown domains are found in \code{path} that cannot be matched to a type, 
#' these can be found in the \code{unknown_domains} attribute of the outcome table. 
#' In addition, a message is printed to the console, unless \code{quiet} is set to \code{TRUE}.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export 

adam_domain_type <- function(
  path  = NULL , 
  keep  = NULL, 
  drop  = NULL,
  quiet = FALSE
  ){
  
  # define look-up table ####
  # library of data sets to be processed automatically
  
  type_adsl <- c("adsl") %>% 
    paste0(".sas7bdat$") 
  type_bds <- c(   
    paste0( c(  
      "adegf", "adpc",
      "adlb", "advs", "adxb", "adxl", 
      "adqskccq", "adqsnyha", 'adqseq5d', 'adqspad', 'adqswimp'
      ) , 
      ".sas7bdat$"),
    "adqs.*[.]sas7bdat$" ) #%>% 
  type_occds <- c("adae", "adcm", "admh") %>% 
    paste0(".sas7bdat$")
  
  ads_library <- dplyr::bind_rows(
    tibble::tibble(domain = type_adsl)  %>% dplyr::mutate(`adam_spec_*` = 'adsl' ),
    tibble::tibble(domain = type_bds)   %>% dplyr::mutate(`adam_spec_*` = 'bds'  ),
    tibble::tibble(domain = type_occds) %>% dplyr::mutate(`adam_spec_*` = 'occds') 
  )  %>%  
    dplyr::mutate_at('domain',~  stringr::str_remove_all(.x, '(\\[.\\]|[.])sas7bdat\\$'))
  
  type_adsl  <- type_adsl  %>% paste(collapse = "|")
  type_bds   <- type_bds   %>% paste(collapse = "|")
  type_occds <- type_occds %>% paste(collapse = "|")
  
  
  # EITHER return look-up table  ####
  if(is.null(path)){ 
    return(ads_library) 
    
  # OR process path...  ####  
  }else{
    # ... check path ###
    path <- normalizePath(path)

    if( ! dir.exists(path) && ! file.exists(path)){
      usethis::ui_stop(paste0(
        crayon::silver("The provided path does not exist. \n\t "), 
        crayon::blue(path)
      ))
    }
      
    # ... determine all file paths, file names ####
    all_files <- list.files(path, pattern = ".sas7bdat$", full.names = TRUE, recursive = TRUE)
    
    # length(all_files) == 0 -> 'path' might be a single file
    
    
    if (length(all_files) == 0) all_files <- path
    
    file_name  <- stringr::str_split(all_files, '/|\\\\')  %>%  
      purrr::map( ~ .[length(.)]) %>% 
      unlist()
    
    # ... determine types and domains from look-up table defined above ####
    all_types <- purrr::map_chr(file_name, ~{
      if (       stringr::str_detect(., type_adsl )){ "adsl"
      } else if (stringr::str_detect(., type_bds  )){ "bds"
      } else if (stringr::str_detect(., type_occds)){ "occds"
      } else {                                        "none"
      }
    })
    
    all_doms  <- stringr::str_split( all_files, '/|\\\\')  %>%  
      purrr::map( ~ .[length(.)]) %>% 
      unlist() %>%
        stringr::str_remove('.sas7bdat')
      
    # ... file_info: create full mapping table ####
    file_info <- tibble::tibble(
      "file"   = all_files,
      "domain" = all_doms,
      "type"   = all_types
    )
      
      
    # ... check selection options ###
    if( !is.null(keep) && !is.null(drop) ){
      usethis::ui_info(crayon::silver( 
        "Please specify only one of 'keep' or 'drop'. Only 'keep' will be used for subsetting here. \n\t " 
       ))
    }
      
    if (!is.null(keep)){
        # strip file extension, in case the user provided the file name instead of domain
        keep      <- stringr::str_remove(keep, '.sas7bdat$')
        file_info <- file_info %>% dplyr::filter( domain %in% keep)
    } else {
      if(!is.null(drop)){
          drop      <- stringr::str_remove(drop, '.sas7bdat$')
          file_info <- file_info %>% dplyr::filter(!domain %in% drop)
      }
    }
    
    
    # ... ui_stop ###
    if(nrow(file_info) == 0){
         usethis::ui_stop("No files to process. Please check your file selection (keep/drop).")
    }
    
    if(all(file_info$type == "none")){
      usethis::ui_stop("The data type is unknown for all files in the given file selection.")
    }
      
    # doms_ignored: domains without match in look-up table  ####
    doms_ignored <- file_info %>% 
      dplyr::filter(type == "none") %>% 
      dplyr::pull(domain)
      
    if(length(doms_ignored) > 0 && !quiet){
      usethis::ui_info( paste0(
        crayon::silver('The following domains were not processed as they are currently not in the library: \n\t'), 
        crayon::blue(paste(doms_ignored, collapse=', ')),
        crayon::silver( '\nYou can use the adam_spec_*() functions as appropriate.'))
      )
    }
      
    attr(file_info, 'unknown_domains') <- doms_ignored
      
    file_info %>% dplyr::relocate(file, .after = last_col())
  }   
}

# test area ####
if(FALSE){
 
 paths <- paste0('../../../',
         c('', 'adcm.sas7bdat'))
 
 # print look-up table
 adam_domain_type()
 
 # process path with unknown domains (adpr)
 adam_domain_type(path = paths[1])
 adam_domain_type(path = paths[1], quiet = TRUE)
 adam_domain_type(path = paths[1], quiet = TRUE) %>%  attr('unknown_domains')
 
 # process single file
 adam_domain_type(path = paths[2])
 
 # keep: files actually in path
 adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs'))  
 adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs.sas7bdat'))  
 
 # keep: files not found in path (typo, missing file selected)
 adam_domain_type(path = paths[1], keep = c('adqs'))  
 
 # keep/drop: keep  
 # info: Please specify only one of 'keep' or 'drop'. Only 'keep' will be used for subsetting here.
 adam_domain_type(path = paths[1], keep = c('adqseq5d', 'advs'), drop = 'advs')  
 
 # path: path doesn't exist
 # Error: The provided path does not exist
 adam_domain_type(path = str_remove(paths[1], 'Original/') )  

}
