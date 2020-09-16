
#' build adsl
#' 
#' @param spec preparation specification for a single file (as provided by result of adam_spec_adsl()) 

#' 

# test area####
if(FALSE){
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  study <- c(99999, 99999, 99999)[3]
  file  <- paste0('real_world_data/adsl/', study, '/adsl.sas7bdat')
  
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
  
  if(is.null(spec$data)){
    
    # read data   ####
    file_name <- spec$file 
    file_ext <- str_split( file_name, '/|\\\\')[[1]] %>%  
      tail(1) %>%  
      str_split(., '[.]') %>% 
      .[[1]] %>%  
      tail(1) 
    if(file_ext == 'sas7bdat'){
      adsl_full <- haven::read_sas(file_name) %>% 
        dplyr::mutate_if(is.character, ~dplyr::na_if(., ""))
    }else return(NULL)
    
  } else {
    
    adsl_full <- spec$data
    
  }
  
  filter_txt <-paste( '(',
                      paste(  spec$filter, collapse= ') & (' ),
                      ')') 
  
  adsl <- adsl_full %>% 
    {if(!is.null(spec$filter)){ 
      dplyr::filter(., !! rlang::parse_expr(filter_txt))
    }else{.}
    } %>% 
    select( any_of(spec$select )) %>% 
    rename( `.id` = spec$id ) 
  # mutate_at(vars( .id),
  #  ~ 'a')
  # ~ as_label(enquo(.x))
  # ~ rlang::as_name(quo(.x))
  #         ) 
  
  # reorder factor levels  ####
  clmns <- names(spec$factor_levels)
  for(c in 1:length(clmns)){ # c=1
    clmn <- clmns[c]
    levs <- spec$factor_levels[[c]]
    adsl[, clmn, drop = TRUE]  <-  adsl[, clmn, drop = TRUE] %>%  
      factor( levels = levs)
  }
  
  # update dictionary
  if (!is.null(spec$dict)){
    dict <- spec$dict %>% 
      mutate(column = param) %>% 
      filter(selected) %>% 
      select(-selected)
  }else{
    
    if(!is.null(spec$spec_id)){
      if(spec$spec_id == ''){ 
        spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
      }
    } else {
      spec$spec_id <- 'user'
    }
    
    lab_list  <- adsl %>% labelled::var_label() 
    labs      <- map_chr(lab_list, ~{ifelse(is.null(.x), NA, .x)})

    dict      <- tibble(
      column = colnames(adsl),
      param  = column ) %>%  
      mutate(
        label = case_when(
            !is.na(labs) ~ labs,
            TRUE ~ param),
        source = spec$spec_id 
      )  
    
  }
  
  
  
  
  # output   ####
  list(
    data = adsl,
    dict = dict 
  )
  
}
