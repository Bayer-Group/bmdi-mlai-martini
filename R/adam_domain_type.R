#' Identify the data set type of ads files by file name
#' 
#' Files are read from the given \code{path} and file names are matched
#' to their corresponding ADaM data type (ads, bds or occds) using a look up table. 
#'
#' @param path ads path to the file of interest
#' @param keep only keep the domains provided, e.g. \code{keep = 'adsl'}
#' @param drop exclude the domains provided, e.g. \code{drop = 'adxb'} 
#' @param add_bds,add_occds character vector of domain names of type bds or 
#' occds that are not included in the package library of ADaM types (yet), but 
#' should be processed as per usual.
#' @param quiet whether to suppress printing info on unknown domains to the console, defaults to \code{TRUE}
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
#' \item{domain}{Name of the ADaM domain, i.e. the file name without its extension}
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
  path      = NULL , 
  keep      = NULL, 
  drop      = NULL,
  add_bds   = NULL,
  add_occds = NULL,
  quiet     = TRUE
  ){
  
  
  ambiguous_add <- intersect(add_bds, add_occds)
  if (length(ambiguous_add) > 0) {
    cli::cli_bullets(c(
      "x" = "The following domain{?s} {?was/were} specified to be added as both bds and occds: {add_ambiguous}.",
      "*" = "Please check your input to {.fun prepare_ml} arguments {.arg add_bds} and {.arg add_occds}."
    ))
  }
  

  # define look-up table ####
  # library of data sets to be processed automatically
  
  file_ext <- c('[.]sas7bdat$', '[.]rds$')
  
  type_list <- list(
    adsl  = c("adsl"),
    bds   = c("adegf", "adpc",
             "adlb", "advs", 
             "adxb", "adxl",
             "adfapr",
             "adxkpa",
             "adxksl",
             "admicro",
             "adtrr",
             "adxt",
             "adqskccq", "adqsnyha", 'adqseq5d', 'adqspad', 'adqswimp', 'adqsqolb', 'adqssgrq',
             "adqs.*"),
    occds = c("adae", "adcm", "admh", "adxa")
  ) 
  
  ads_library <- dplyr::bind_rows(
    tibble::tibble(domain = type_list$adsl)  %>% dplyr::mutate(`adam_spec_*` = 'adsl' ),
    tibble::tibble(domain = type_list$bds)   %>% dplyr::mutate(`adam_spec_*` = 'bds'  ),
    tibble::tibble(domain = type_list$occds) %>% dplyr::mutate(`adam_spec_*` = 'occds') 
  )  
  
  type_list_regex <- type_list %>% 
    purrr::map(~{paste0(rep(.x, each = length(file_ext)), file_ext) %>% 
        paste(collapse = "|")})
  
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
    all_files <- list.files(path, pattern = paste(file_ext, collapse = '|'), full.names = TRUE, recursive = FALSE)
    
    # length(all_files) == 0 -> 'path' might be a single file
    
    
    if (length(all_files) == 0) all_files <- path
    
    file_name  <- basename(all_files)
    

    # ... file_info: create full mapping table ####
    file_info <- tibble::tibble(
      file = all_files
    ) %>% 
    # ... determine types and domains from look-up table defined above ####
    dplyr::mutate(
      domain = basename(file) %>% tools::file_path_sans_ext(),
      type   = dplyr::case_when(
        stringr::str_detect(basename(file), type_list_regex$adsl ) ~ "adsl",
        stringr::str_detect(basename(file), type_list_regex$bds  ) ~ "bds",
        domain %in% add_bds                                        ~ "bds",
        stringr::str_detect(basename(file), type_list_regex$occds) ~ "occds",
        domain %in% add_occds                                      ~ "occds",
        TRUE ~ "none"
      ), 
      file_ext = tools::file_ext(file)
    )
      
    # ... check selection options ###
    if( !is.null(keep) && !is.null(drop) ){
      usethis::ui_info(crayon::silver( 
        "Please specify only one of 'keep' or 'drop'. Only 'keep' will be used for subsetting here. \n\t " 
       ))
    }
      
    if (!is.null(keep)) {
      # strip file extension, in case the user provided the file name instead of domain
      keep        <- stringr::str_remove(keep, paste(file_ext, collapse = '|'))
      file_info   <- file_info %>% dplyr::filter(domain %in% c(keep, add_bds, add_occds))
    } else {
      if(!is.null(drop)){
        drop      <- stringr::str_remove(drop, paste(file_ext, collapse = '|'))
        file_info <- file_info %>% dplyr::filter(!domain %in% drop)
      }
    }
    
    
    # ... ui_stop ###
    if(nrow(file_info) == 0){
      usethis::ui_stop("No files to process. Please check your file selection (keep/drop).")
    }
    
    if(all(file_info$type == "none") && !quiet){
      usethis::ui_stop("The data type is unknown for all files in the given file selection.")
    }
      
    # doms_ignored: domains without match in look-up table  ####
    doms_ignored <- file_info %>% 
      dplyr::filter(type == "none") %>% 
      dplyr::pull(domain)
      
    if(length(doms_ignored) > 0 && !quiet){
      cat('\n')
      usethis::ui_info( paste0(
        crayon::silver(
          'The following domains were not processed as they are currently not in the library: \n  '
        ), 
        crayon::blue(paste(doms_ignored, collapse = ', ')) %>% crayon::bold(),
        crayon::silver(sep = '',
          '\nYou may consider using the ', usethis::ui_code('add_bds'),
          ' argument in ', usethis::ui_code('adam_spec()'),
          ' to add bds-type data.\n'
        )
      ))
      cat('\n')
    }
      
    attr(file_info, 'unknown_domains') <- doms_ignored
      
    file_info %>% 
      dplyr::relocate(file, .after = tidyselect::last_col()) %>% 
      dplyr::arrange(domain)
  }   
}

