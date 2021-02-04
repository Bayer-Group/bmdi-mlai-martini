
#' build occds
#' 
#' Extract and reshape data from a single occds-type data set according to the given specification as created by \code{adam_spec_occds()}.
#' 
#' @param spec result of \code{adam_spec_occds()}
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
  
  #file = paste0('real_world_data/99999/',
  #              c('adqseq5d', 'advs', 'adegf')[3],'.sas7bdat')
  
  file =  '../admh.sas7bdat'
  spec <- adam_spec_occds(
    file = file,
    pre_study = TRUE,
    attach_data = FALSE
  )
  
}



# occds_prep() ####

build_occds <- function(
  spec,
  ...
  
){
  
  
  if(is.null(spec$data)){
    # read data   ####
    file_name <- spec$file 
    file_ext  <- stringr::str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      stringr::str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1) 
    if(file_ext == 'sas7bdat'){
      occds_full <- haven::read_sas(file_name)
    }else return(NULL)
  } else {
    occds_full <- spec$data
  }
  
  col_select <- spec[c("label", "value", "valuen")] %>% 
    unlist() %>%  na.omit() %>% as.character()
  
  
  occds <- occds_full %>% 
    {if(length(spec$filter) > 0){ 
      filter_txt <-  paste( '(',
                            paste(  spec$filter, collapse= ') & (' ),
                            ')') 
      
      dplyr::filter(., !! rlang::parse_expr(filter_txt))
    }else{.}} %>% 
    dplyr::select( tidyselect::any_of( c(spec$id, col_select  ))) %>% 
    dplyr::rename( c(`.id` = spec$id , label =  spec$label)) %>% 
    {if(is.null(spec$value)){
      dplyr::mutate(., value = factor("yes"))
    } else {
      {if (is.null(spec$valuen)){
        dplyr::mutate(., value = factor(!!rlang::sym(spec$value)))
      }else{
        dplyr::mutate(., value = forcats::fct_reorder(!!rlang::sym(spec$value), !!rlang::sym(spec$valuen))) %>% 
          select(- any_of( spec$valuen))
      }}
    }} %>% 
    # dplyr::distinct() %>% 
    dplyr::filter(!spec$label %in% c("", " ")) %>% 
    dplyr::mutate(param = make.names(label) %>% 
                    stringr::str_replace_all("[.]", "_") %>%
                    stringr::str_to_lower()) 
  
  
  
  
  # TODO for pivoting: add parameter values_fn, currently maximum is chosen 
  values_fn <- function(x) base::max(as.numeric(x))
  # pivot   ####
  
  
  occds_wide <- occds %>% 
      dplyr::select(tidyselect::all_of(c('value', 'param', '.id'))) %>% 
      { if(spec$count){
         dplyr::count(., .id, param, name = 'value' )  
      } else {.}
      }  %>%   
      tidyr::pivot_wider(
        names_from  = param, 
        values_from = value,
        values_fn   = values_fn
      ) 
    
      
  # transform all character columns to factors except for .id, which is kept as-is
  char2fct <- occds_wide %>% 
    select_if(is.character) %>% 
    colnames() %>% 
    setdiff('.id' )
  
  if (length(char2fct) > 0) occds_wide <- occds_wide %>% mutate_at(char2fct, factor)

  # dictionary ####
  # overwrite dictionary from spec
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  } else {
    spec$spec_id <- 'user'
  }
  
  dict <- occds %>% 
    dplyr::select(param, label) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(column = param) %>% 
    dplyr::mutate(source = spec$spec_id) %>% 
    dplyr::mutate(type   = 'occds')
  
  # output ####
  list(
    data = occds_wide,
    dict = dict
  )
  
  
  
  
}

