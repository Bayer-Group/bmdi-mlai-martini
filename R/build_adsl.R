#' @rdname build_x
#'

# (see 'build_x.R' for documentation details)


build_adsl <- function(
  spec
){
  
  # check/import data ####
  
  md5 <- NULL
  
  if (!(is.null(spec$md5))){
    md5 <- spec$md5
  } else if (!(is.na(spec$file)||is.null(spec$file))) {
    md5 <- tools::md5sum(spec$file) %>% as.character()
  }
  
  if(is.null(spec$data)){
    
    # ... no data attached ####
    
    file_name <- spec$file 
    file_ext  <- stringr::str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      stringr::str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1)
    
    if(file_ext == 'sas7bdat'){
      
      adsl_full <- haven::read_sas(file_name) %>% 
        dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))      
      
    } else {
      
      # TODO refactor - warning is also used in other build_*() functions
      cli::cli_warn(c(
        "{.fn build_adsl} expects a sas7bdat file to read, but was provided {.path {file_name}}.",
        "i" = "The provided file in the spec entry {.code {spec$spec_id}} is not of type sas7bdat, but {file_ext}.",
        "i" = "No data set was built from spec entry {.code {spec$spec_id}} and NULL was returned.",
        "*" = "Please check your input or attach the data set instead."
      ))
      
      return(NULL)
      
    }
  } else {
    
    # ... data attached ####
    
    adsl_full <- spec$data %>% 
      dplyr::mutate_if(is.character,  ~ dplyr::na_if(., ""))
    
    if(md5 != spec$md5){
      
      # TODO refactor - warning is also used in other build_*() functions
      cli::cli_warn(c(
        "i" = "The spec entry {.code {spec$spec_id}} was created from a file with a 
        different md5 checksum than the one that is provided in the {.arg file}
        entry of the spec.",
        "*" = "Check the provided file path or consider recreating the spec."
      ))

    }  
    
  }
  
  # reorder / set factor levels  ####
  clmns <- names(spec$factor_levels) %>% 
    intersect(names(adsl_full))
  
  if(length(clmns)>0){

    for(clmn in clmns){ 
      adsl_full[[clmn]] <- adsl_full[[clmn]] %>% 
        factor(levels = spec$factor_levels[[clmn]])
    }
  }
  
  
  # apply spec: filter, select and standardize column names ####
  
  filter_txt <- paste(
    '(', paste(  spec$filter, collapse= ') & (' ), ')'
  ) 
  
  adsl <- adsl_full %>% 
    {if(length(spec$filter) > 0){ 
      dplyr::filter(., !! rlang::parse_expr(filter_txt))
    }else{.}
    } %>%  
    dplyr::select(tidyselect::any_of(spec$select)) %>% 
    dplyr::rename(".id" = spec$id) %>% 
    {if(!is.null(spec$trt)){
      dplyr::rename(., ".trt"= spec$trt)
    }else{.}
    }
    
  # set 'spec_id' if missing (required for dictionary) ####
  # this is e.g. the case, if spec was not created with 'adam_spec_adsl'
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  } else {
    spec$spec_id <- 'user'
  }
  
  # update dictionary   ####
  if(!is.null(spec$dict)){
    dict <- spec$dict %>% 
      
      # treatment variable was renamed to standard name '.trt' (same for '.id')
      dplyr::filter(param %in% c(spec$trt, spec$id, colnames(adsl))) %>% 
      {if (!is.null(spec$trt)){
        dplyr::mutate(., param = dplyr::case_when(
          param == spec$trt ~ ".trt",
          TRUE              ~ param
        ))
      } else {.}
      } %>% 
      {if (!is.null(spec$id)){
        dplyr::mutate(., param = dplyr::case_when(
          param == spec$id ~ ".id",
          TRUE              ~ param
        ))
      } else {.}
      } %>% 
      dplyr::mutate(column = param) %>% 
      dplyr::select(-selected)
    
  }else{
    
    lab_list  <- adsl %>% labelled::var_label() 
    labs      <- purrr::map_chr(lab_list, ~ {ifelse(is.null(.x), NA, .x)})

    dict      <- tibble::tibble(
      column = colnames(adsl),
      param  = column ) %>%  
      dplyr::mutate(
        label = dplyr::case_when(
            !is.na(labs) ~ labs,
            TRUE ~ param),
        source = spec$spec_id,
        type   = 'adsl'
      )  
    
  }
  
  # output ####
  list(
    data       = adsl,
    dict       = dict,
    source     = list(file = spec$file, md5 = md5),
    flag_table = spec$flag_table
  )
  
}


