
#' build adsl
#' 
#' @param spec preparation specification for a single file (as provided by result of adam_spec_adsl()) 

#' 

# test area####
if(FALSE){
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  #study <- c(99999, 99999, 99999)[1]
  #file  <- paste0('real_world_data/', study, '/adsl.sas7bdat')
  
  file  <- here::here('data', '99999', 'ads','adsl.sas7bdat')
  
  id = 'SUBJID'
  trt = NULL
  keep = NULL
  drop = NULL
  filter = c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
  
  spec <- adam_spec_adsl(file = file, id = id, filter = filter)
  
}



# ads_prep() ####

build_adsl <- function(
  spec,
  ...
  
){
  
 
  if(is.null( spec$data )){

    # read data   ####
    file_name <- spec$file 
    file_ext  <- stringr::str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      stringr::str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1) 
    if(file_ext == 'sas7bdat'){
      adsl_full <- haven::read_sas(file_name) %>% 
        dplyr::mutate_if(is.character,  ~ dplyr::na_if(., ""))
    
      
    }else return(NULL)
    
  } else {
    
    adsl_full <- spec$data %>% 
      dplyr::mutate_if(is.character,  ~ dplyr::na_if(., ""))
    
  }
  
  
  filter_txt <- paste( '(',
                      paste(  spec$filter, collapse= ') & (' ),
                      ')') 
  
  
  
  # reorder factor levels  ####
  clmns <- names(spec$factor_levels)
  for(c in 1:length(clmns)){ # c=1
    clmn <- clmns[c]
    levs <- spec$factor_levels[[c]]
    
    adsl_full[, clmn, drop = TRUE]  <-  adsl_full[, clmn, drop = TRUE] %>%  
      factor(levels = levs)
    # shift to prepare_ml:   map(levs, ~ (str_to_lower(.x) %>%  str_replace_all( clean_char) )))
  }
  
  
  # apply spec: filter, select and standardize column names ####
  adsl <- adsl_full %>% 
    {if(length(spec$filter) > 0){ 
      dplyr::filter(., !! rlang::parse_expr(filter_txt))
    }else{.}
    } %>%  
    dplyr::select(any_of(spec$select )) %>% 
    dplyr::rename(".id" = spec$id) %>% 
    {if(!is.null(spec$trt)){
      dplyr::rename(., ".trt"= spec$trt)
    }else{.}
    }
    
  # set 'spec_id' if missing (required for dictionary)
  # this is e.g. the case, if spec was not created with 'adam_spec_adsl'
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  } else {
    spec$spec_id <- 'user'
  }
  
  # update dictionary   ####
  if (!is.null(spec$dict)){
    dict <- spec$dict %>% 
      # treatment variable was renamed to standard name
      dplyr::filter(param %in% c(spec$trt, colnames(adsl))) %>% 
      {if (!is.null(spec$trt)){
        dplyr::mutate(., param = dplyr::case_when(
          param == spec$trt ~ ".trt",
          TRUE              ~ param
        ))
      } else {.}}
      dplyr::mutate(column = param) %>% 
      dplyr::select(-selected)
    
  }else{
    
    lab_list  <- adsl %>% labelled::var_label() 
    labs      <- purrr::map_chr(lab_list, ~ {ifelse(is.null(.x), NA, .x)})

    dict      <- tibble::tibble(
      column = colnames(adsl),
      param  = column ) %>%  
      mutate(
        label = dplyr::case_when(
            !is.na(labs) ~ labs,
            TRUE ~ param),
        source = spec$spec_id ,
        type   = 'adsl'
      )  
    
  }
  
  
  
  
  # output   ####
  list(
    data = adsl,
    dict = dict 
  )
  
}
