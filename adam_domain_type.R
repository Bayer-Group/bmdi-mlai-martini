#'
#' @param path ads path 
#'

adam_domain_type <- function(path, keep, deop){
  
  all_files <- list.files(path, pattern = ".sas7bdat", full.names = TRUE)
  
  # library of data sets to be processed automatically
  type_adsl <- c("adsl") %>% 
    paste0(".sas7bdat$") %>% 
    paste(collapse = "|")
  type_bds <- c(   
    paste0( c(  "adegf",   "adpc",
                "adlb",  "advs",  "adxb", "adxl") , ".sas7bdat$"),
    "adqs.*[.]sas7bdat$" ) %>% 
    paste(collapse = "|")
  
  file_name  <- stringr::str_split(file, '/|\\\\')  %>%  
    purrr::map( ~ .[length(.)]) %>% 
    unlist()
  
  all_types <- purrr::map_chr(file_name, ~{
    if (stringr::str_detect(., type_adsl)){
      "adsl"
    } else if (stringr::str_detect(., type_bds)){
      "bds"
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
