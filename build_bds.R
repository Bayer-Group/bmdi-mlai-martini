
#' build bds
#' 
#' Extract and reshape data from a single bds-type data set according to the given specification as created by \code{adam_spec_bds()}.
#' 
#' @param spec result of \code{adam_spec_bds()}
#' 
#' @return 
#' A list with two elements \code{data} and \code{dict}, where 
#' \code{data} a tibble in wide format which one row per \code{id} 
#' \code{dict} a tibble listing the distinct combinations of columns \code{param}, \code{label}, \code{unit}, \code{time}, \code{column}, \code{source} (if provided). 

#' @description Note that the output dictionary differs from the dictionary created by \code{adam_spec_*}, as multiple features may be derived from a single parameter at different time points.  




# test area####
if(FALSE){
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  #study <- c(99999, 99999, 99999)[3]
  
  file = paste0('real_world_data/99999/',
                c('adqseq5d', 'advs')[1],'.sas7bdat')
  id = 'SUBJID'
  param  =  NULL
  label  = NULL
  unit   = NULL # AVALU, xxSTRESU, xxORESSU
  time   = NULL 
  value  = NULL #c(AVAL, CHG)
  filter = NULL
  spec <- adam_spec_bds(file = file, id = id, filter = filter,
                   param = param, unit = unit, time = time)
  spec
  
}



# bds_prep() ####

build_bds <- function(
  spec,
  ...
  
){
  
  
  if(is.null(spec$data)){
    # read data   ####
    file_name <- spec$file 
    file_ext <- str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1) 
    if(file_ext == 'sas7bdat'){
      bds_full <- haven::read_sas(file_name)
    }else return(NULL)
  } else {
    bds_full <- spec$data
  }

  col_select <- spec[c("param",  "time" ,  "value",  "unit",   "label" )] %>% 
    unlist() %>%  na.omit() %>%  as.character()
  
  bds <- bds_full %>% 
    {if(!is.null(spec$filter)){ 
       filter_txt <-  paste( '(',
                            paste(  spec$filter, collapse= ') & (' ),
                            ')') 
      
        dplyr::filter(., !! rlang::parse_expr(filter_txt))
      }else{.}} %>% 
    filter( ! is.na(!! sym(spec$value )) ) %>% 
    select( any_of( c(spec$id, col_select  ))) %>% 
    rename( `.id` = spec$id ) 
 
  # prior to pivoting,  create key column (PARAM or PARAM/TIME)
  # check if multiple time points are present after subsetting
  n_time <- ifelse(! is.na(spec$time),
                   bds %>%  pull(spec$time) %>%  n_distinct() ,
                   1)
  if(n_time > 1){
    bds <- bds %>% 
      unite(.key, spec$param, spec$time, remove = FALSE, sep='_') %>% 
      mutate(.key = str_replace_all(.key, '[:punct:]', '_'))
  }else{
    bds <- bds %>% 
      mutate( '.key' = !! sym(spec$param))
  }

  # pivot 
  
  bds_wide <- bds %>% 
    select(all_of(c(spec$value, '.key', '.id'))) %>% 
    filter(.key != "") %>% 
    pivot_wider(
      names_from  = '.key', 
      values_from = spec$value,
      values_fn   = mean
    )
  
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
    select(any_of(c("param" = spec$param, "label" = spec$label, "unit" = spec$unit, "time" = spec$time, '.key') %>% na.omit)) %>% 
    distinct() %>% 
    rename( 'column' = '.key') %>% 
    mutate(source = spec$spec_id) 
  
  # output ####
  list(
    data = bds_wide,
    dict = dict
  )
  
  
  
  
}
