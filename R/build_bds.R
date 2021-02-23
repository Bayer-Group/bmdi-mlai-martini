#' @rdname build_x
#'
#' @export

# (see 'build_x.R' for documentation details)

build_bds <- function(
  spec
){
  
  md5 <- tools::md5sum(spec$file) %>%  as.character()
  
  if(is.null(spec$data)){
    
    # read data   ####
    file_name <- spec$file 
    file_ext  <- stringr::str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      stringr::str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1) 
    
    if(file_ext == 'sas7bdat'){
      bds_full <- haven::read_sas(file_name)
      
      if( md5 != spec$md5){
        usethis::ui_info(crayon::silver(
          paste0('\t',  spec$spec_id, ': The spec was created from a file with a different md5 checksum. \n'))
        )
      }  
      
      
    }else return(NULL)
  } else {
    bds_full <- spec$data
  }

  col_select <- spec[c("param",  "time" ,  "value",  "unit",   "label" )] %>% 
    unlist() %>%  na.omit() %>%  as.character()
  

  bds <- bds_full %>% 
    {if(length(spec$filter) > 0){ 
       filter_txt <-  paste( '(',
                            paste(  spec$filter, collapse= ') & (' ),
                            ')') 
      
        dplyr::filter(., !! rlang::parse_expr(filter_txt))
      }else{.}} %>% 
    dplyr::filter( ! is.na(!! rlang::sym(spec$value )) ) %>% 
    dplyr::select( tidyselect::any_of( c(spec$id, col_select  ))) %>% 
    dplyr::rename( `.id` = spec$id ) # 
  
 
  # prior to pivoting,  create key column (PARAM or PARAM/TIME)
  # check if multiple time points are present after subsetting
  n_time <- ifelse(! is.na(spec$time),
                   bds %>%  dplyr::pull(spec$time) %>%  dplyr::n_distinct() ,
                   1)
  if(n_time > 1){
    bds <- bds %>% 
      tidyr::unite(.key, spec$param, spec$time, remove = FALSE, sep='_') %>% 
      dplyr::mutate(.key = stringr::str_replace_all(.key, '[:punct:]|[:space:]', '_'))
  }else{
    bds <- bds %>% 
      dplyr::mutate( '.key' = stringr::str_replace_all( !! rlang::sym(spec$param), '[:punct:]|[:space:]', '_'))
  }

  # pivot   ####
 
  
  bds_wide <- bds %>% 
    dplyr::select(tidyselect::all_of(c(spec$value, '.key', '.id'))) %>% 
    dplyr::filter(.key != "") %>% 
    tidyr::pivot_wider(
      names_from  = '.key', 
      values_from = spec$value,
      values_fn   = function(x) {ifelse(all(is.numeric(x)), mean(x), x[1])}
    ) 
  
  # transform all character columns to factors except for .id, which is kept as-is
  char2fct <-   bds_wide %>% 
    dplyr::select_if(is.character) %>% 
    colnames() %>% 
    setdiff('.id' )
  
  bds_wide <- bds_wide  %>% 
    dplyr::mutate_at(char2fct, factor) %>%  
    {if(spec$spec_id == 'adegf'){
      dplyr::mutate_at(., char2fct, ~ fct_explicit_na(., na_level = 'missing') )
    }else{.}
    }
  
  
    

  
  # dictionary ####
  # overwrite dictionary from spec
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  } else {
    spec$spec_id <- 'user'
  }
    
  dict <- bds %>% 
    dplyr::select(tidyselect::any_of(
      c("param" = spec$param, 
        "label" = spec$label,
        "unit"  = spec$unit, 
        "time"  = spec$time, 
        '.key') %>% na.omit)) %>% 
    dplyr::distinct() %>% 
    dplyr::rename('column' = '.key') %>% 
    dplyr::mutate(source = spec$spec_id) %>% 
    dplyr::mutate(type   = 'bds') 
  
  # output ####
  list(
    data   = bds_wide,
    dict   = dict,
    source = list(file = spec$file, md5 = md5) 
  )
  
  
  
  
}

# test area ####
if(FALSE){
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  #study <- c(99999, 99999, 99999)[3]
  
  #file = paste0('real_world_data/99999/',
  #              c('adqseq5d', 'advs', 'adegf')[3],'.sas7bdat')
  
  file =  '../adegf.sas7bdat'
  id = 'SUBJID'
  param  =  NULL
  label  = NULL
  unit   = NULL # AVALU, xxSTRESU, xxORESSU
  time   = NULL 
  value  = NULL #c(AVAL, CHG)
  filter = 'AVISIT == Screening'
  spec_0 <- adam_spec_bds(file = file, id = id, filter = filter,
                          param = param, unit = unit, time = time)
  spec <- spec_0
  
  spec_  <- spec_0
  spec_c <- spec_
  spec_c$value <- 'AVALC'
  
  spec <- spec_c
  
}

