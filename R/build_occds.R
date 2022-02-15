#' @rdname build_x
#'

# (see 'build_x.R' for documentation details)

build_occds <- function(
  spec
){
  
  md5 <- NULL
  
  if (!(is.na(spec$md5)||is.null(spec$md5))){
    md5 <- spec$md5
  } else if (!(is.na(spec$file)||is.null(spec$file))) {
    md5 <- tools::md5sum(spec$file) %>% as.character()
  } 
  
  if(is.null(spec$data)){
    
    # read data   ####
    file_name <- spec$file 
    file_ext  <- stringr::str_split(file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      stringr::str_split(., '[.]') %>% 
      {.[[1]]} %>%  
      tail(1) 
    
    if(file_ext == 'sas7bdat'){
      occds_full <- haven::read_sas(file_name) %>% 
        dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))       
      
      if(md5 != spec$md5){
        usethis::ui_info(crayon::silver(paste0('\t',  
          spec$spec_id, 
          ': The spec was created from a file with a different md5 checksum. \n'))
        )
      }  
      
    }else return(NULL)
  }else{
    occds_full <- spec$data
  }
  
  col_select <- spec[c("label", "value", "valuen")] %>% 
    unlist() %>%  na.omit() %>% as.character()
  
  
  occds <- occds_full %>% 
    {if(length(spec$filter) > 0){ 
      filter_txt <- paste( '(',
                            paste(spec$filter, collapse= ') & (' ),
                            ')') 
      
      dplyr::filter(., !! rlang::parse_expr(filter_txt))
    }else{.}} %>% 
    dplyr::select(tidyselect::any_of(c(spec$id, col_select)) ) %>% 
    dplyr::rename(c(`.id` = spec$id, label = spec$label)) %>% 
    {if(is.null(spec$value)){
      dplyr::mutate(., value = factor("yes"))
    }else{
      {if(is.null(spec$valuen)){
        dplyr::mutate(., value = factor(!!rlang::sym(spec$value)))
      }else{
        dplyr::mutate(., value = forcats::fct_reorder(!!rlang::sym(spec$value), !!rlang::sym(spec$valuen))) %>% 
          dplyr::select(- tidyselect::any_of( spec$valuen))
      }}
    }} %>% 
    # remove observations with empty label
    dplyr::filter(stringr::str_squish(label) != "") %>% 
    dplyr::mutate(param = make.names(label) %>% 
      stringr::str_replace_all("[.]", "_") %>%
      stringr::str_to_lower()) 
    
  
  
  
  
  # TODO for pivoting: add parameter values_fn, currently maximum is chosen 
  values_fn <- function(x){
    # if numeric = max
    # if factor  = highest level
    sort(x) %>% tail(1)
  }
  
  # pivot   ####
  
  occds_wide <- occds %>% 
      dplyr::select(tidyselect::all_of(c('value', 'param', '.id'))) %>% 
      {if(spec$count){
         dplyr::count(., .id, param, name = 'value' )  
      }else{.}
      }  %>%   
      tidyr::pivot_wider(
        names_from  = param, 
        values_from = value,
        values_fn   = values_fn
      ) 
    
      
  # transform all character columns to factors except for .id, which is kept as-is
  char2fct <- occds_wide %>% 
    dplyr::select_if(is.character) %>% 
    colnames() %>% 
    setdiff('.id')
  
  if(length(char2fct) > 0) occds_wide <- occds_wide %>% dplyr::mutate_at(char2fct, factor)

  # dictionary ####
  # overwrite dictionary from spec
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  }else{
    spec$spec_id <- 'user'
  }
  
  dict <- occds %>% 
    dplyr::select(param, label) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(column = param) %>% 
    dplyr::mutate(source = spec$spec_id) %>% 
    dplyr::mutate(type   = 'occds')
  #TODO: add '(count {label})' to label column
  
  # output ####
  list(
    data   = occds_wide,
    dict   = dict,
    source = list(file = spec$file, md5 = md5) 
  )
  
}




# test area####
if(FALSE){
  # '../adegf.sas7bdat'
  #study <- c(99999, 99999, 99999)[3]
  
  #file = paste0('data/ads/',
  #              c('adqseq5d', 'advs', 'adegf')[3],'.sas7bdat')
  
  file =  '../admh.sas7bdat'
  spec <- adam_spec_occds(
    file = file,
    pre_study = TRUE,
    attach_data = FALSE
  )
  
}
