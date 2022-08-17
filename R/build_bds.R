#' @rdname build_x
#'
utils::globalVariables(c("guess", "var"))

# (see 'build_x.R' for documentation details)

build_bds <- function(
  spec,
  dupl_ctrl = list(
    values_fn = NULL,
    arrange   = NULL
  )
){
  
  ## TODO input check spec

  md5 <- NULL
  
  if (!(is.null(spec$md5))){
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
      .[[1]] %>%  
      tail(1) 
    
    if(file_ext == 'sas7bdat'){
      bds_full <- haven::read_sas(file_name) %>% 
        dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
      
      if( md5 != spec$md5){
        usethis::ui_info(crayon::silver(
          paste0('\t',  spec$spec_id, 
                 ': The spec was created from a file with a different md5 checksum. \n'))
        )
      }  
      
      
    }else return(NULL)
  }else {
    bds_full <- spec$data
  }

  col_select <- spec[c("param", "time", "value", "unit", "label")] %>% 
    unlist() %>% na.omit() %>% as.character()
  
  # COMBAK
  
  # use duplicated controls from 'dupl_ctrl' argument over duplicated controls in 'spec', if not NULL
  values_fn <- dupl_ctrl$values_fn %||% spec$dupl_ctrl$values_fn
  arrange   <- dupl_ctrl$arrange   %||% spec$dupl_ctrl$arrange
  
  if (is.null(values_fn)){
    values_fn <- function(x) {ifelse(all(is.numeric(x)), mean(x, na.rm = TRUE), na.omit(x)[1])}
  }
  
  pivot_input <- pivot_prepare_bds(
    bds_full  = bds_full,
    filter    = spec$filter,
    arrange   = arrange,
    value     = spec$value, 
    param     = spec$param, 
    time      = spec$time,
    id        = spec$id,
    values_fn = values_fn
    #,  single_row = TRUE
  )
  
  
  # pivot   ####
  bds_pivot <- pivot_input$data
  
    # check for duplicates
  any_dupes <- bds_pivot %>% 
    dplyr::count(spec$id, !!! rlang::syms(pivot_input$names_from)) %>% 
    dplyr::pull(n) %>% 
    magrittr::is_greater_than(1) %>% 
    any()
  
  if (any_dupes){
    usethis::ui_info(crayon::silver(paste0(
      'Duplicates were identified in ', usethis::ui_value(spec$spec_id), '.\n',  
      'Please refer to the documentation of ', usethis::ui_code('dupl_ctrl'), ' for details.\n'
    )))
  }
  
  bds_wide <- do.call(pivot_wider, pivot_input)
  
  
  # transform all created columns according to guessed type (char to factor, num as numeric)
  # guess types
  var_types <- bds_wide %>% 
    dplyr::select(-tidyselect::any_of(colnames(bds_pivot))) %>% 
    purrr::map_chr(readr::guess_parser) %>% 
    tibble::enframe('var', 'guess') 
  
  char2fct <- var_types %>% 
    dplyr::filter(guess == 'character') %>% 
    dplyr::pull(var)
  
  char2num <- var_types %>% 
    dplyr::filter(guess == 'double') %>% 
    dplyr::pull(var)
  
  bds_wide <- bds_wide %>% 
    dplyr::mutate_at(char2fct, factor) %>%  
    {if(spec$spec_id == 'adegf'){
      dplyr::mutate_at(., char2fct, ~ forcats::fct_explicit_na(., na_level = 'missing'))
    }else{.}
    } %>% 
    dplyr::mutate_at(char2num, as.numeric)
    
  # rename to standardized column names ####
  renaming <- c(
    '.id' = spec$id  
  )
  
  bds_wide <- bds_wide %>% 
    dplyr::rename(tidyselect::any_of(renaming))
  
  
  # dictionary ####
  # overwrite dictionary from spec
  if(!is.null(spec$spec_id)){
    if(spec$spec_id == ''){ 
      spec$spec_id <- ifelse(is.null(spec$file), 'user', spec$file)
    }
  } else {
    spec$spec_id <- 'user'
  }
    
  # COMBAK .key nonexistent, param and time columns modified (stringr clean up in refactor) 
  dict <- bds_pivot %>% 
    dplyr::select(tidyselect::any_of(
      c("param" = spec$param, 
        "label" = spec$label,
        "unit"  = spec$unit, 
        "time"  = spec$time 
        #'.key'
      ) %>% na.omit)) %>% 
    dplyr::distinct() %>% 
    #dplyr::rename('column' = '.key') %>% 
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

