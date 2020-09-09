
#' bds spec (basic data structure)
#' 
#' @param file the sas file 
#' @param id name of id column to keep
#' @param filter_subj
#' @param 





# function adam_bds_spec() ####
adam_bds_spec <- function(
  file,
  id = 'SUBJID', 
  param  = NULL , # 'PARAMCD',
  label  = NULL,  # PARAMLAB
  unit   = NULL, # AVALU, xxSTRESU, ORESSU
  time   = NULL, 
  value  = NULL, #c(AVAL, CHG)
  filter = NULL
){
  
  
  # test area  ####
  if(FALSE){
    file = 'real_world_data/bds/99999/adqseq5d.sas7bdat'
    id = 'SUBJID'
    param  =  NULL
    label  = NULL
    unit   = NULL # AVALU, xxSTRESU, ORESSU
    time   = NULL 
    value  = NULL #c(AVAL, CHG)
    filter = NULL
    
  }
  
  
  require(tidyverse)
  require(haven)
  require(labelled)
  
  #  read bds ####
  #'adsl.sas7bdat' %in% list.files(path)
  bds <- haven::read_sas(file)
  
  
  #  guess stuff ...####
  
  
  # ... guess domain ####
  dom <- str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% 
    str_sub(3,4) %>%  # rm trailing 'ad'
    # str_split('[:punct:]') %>% 
    # .[[1]] %>% 
    # .[1] %>%  
    str_to_upper()
  
  guesses <- list(
    # ... guess param ####
    param = c('PARAMCD', paste0(dom,'TESTCD')),
    
    # ... guess visit  ####
    time = c('AVISIT', 'VISIT'),
    
    # ... guess value  ####
    value = c('AVAL',   paste0(dom, "STRESN" ),  paste0(dom, "ORRES")),
    
    # ... guess unit ####
    unit = c('AVALU',  paste0(dom, 'STRESU'), paste0(dom,'ORRESU'))
    
  )
  
  guesses$label <- str_sub(guesses$param, 1, -3)
  
  col_select <- c(
    "value" = value,
    "param" = param,
    "time" = time,
    "unit" = unit,
    "label" = label
  )
  
  col_required <- c('value', 'param')
  
  coln_bds <- colnames(bds)

  for (i in 1:length(guesses)){ # i=4
    
    col_i <- col_select[i]
    name_col_i <- names(guesses)[i]
    
    if (is.null(col_i) || !(col_i %in% coln_bds)){
      choices <- guesses[[name_col_i]] %>% 
        intersect(coln_bds)
      
      # escape if required columns cannot be identified
      if (length(choices) == 0 && 
          (name_col_i %in% col_required) ) return(NULL)
      
      col_select[i] <- choices[1]
    }
    
    names(col_select)[i] <- name_col_i
    
  }
  
  # filter check ####
  
  keep_filter <- map_lgl(filter, function(x){
    try_it <- try(
      {bds %>% dplyr::filter(!! rlang::parse_expr(x))},
      silent = TRUE
    )
    is_error <- "try-error" %in% class(try_it)
    is_norow <- FALSE
    if (!is_error) is_norow <- nrow(try_it) == 0
    !(is_error || is_norow)
    
  })
  
  actual_filter <- filter[keep_filter]

 

  # dictionary
  source <- str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% str_remove('.sas7bdat')
  
  dict <- bds %>% 
    select( any_of(c(col_select['param'], col_select['label'], col_select['unit']) %>%  na.omit)) %>% 
    distinct() %>%
    mutate(source = source)
 
 
 
 
 
  # output ####
 
  out <- list(
    file = file,
    type = "bds",
    id = id,
    filter = actual_filter,
    dict = dict
  ) %>% 
    append(
      col_select %>% as.list()
    )

  
  out
 
  
}
