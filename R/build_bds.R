#' @rdname build_x
#'
utils::globalVariables(c("guess", "var"))

# (see 'build_x.R' for documentation details)

build_bds <- function(
  spec,
  dupl_ctrl = list(
    values_fn = NULL,
    arrange   = NULL
  ),
  names_ctrl = list(
    clean_fn  = ~ stringr::str_replace_all(.x, '[:punct:]|[:space:]', '_'), 
    names_sep = '_'
  )
){
  
  ## TODO input check spec
  # stopifnot("martini_spec" %in% class(spec) )

  md5 <- NULL
  
  if (!(is.null(spec$md5))){
    md5 <- spec$md5
  } else if (!(is.na(spec$file) || is.null(spec$file))) {
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
      
      if(md5 != spec$md5){

        # TODO refactor - warning is also used in other build_*() functions
        cli::cli_warn(c(
          "i" = "The spec entry {.code {spec$spec_id}} was created from a file with a 
          different md5 checksum than the one that is provided in the {.arg file}
          entry of the spec.",
          "*" = "Check the provided file path or consider recreating the spec."
        ))
        
      }  
      
      
    }else{
      
      # TODO refactor - warning is also used in other build_*() functions
      cli::cli_warn(c(
        "{.fn build_adsl} expects a sas7bdat file to read, but was provided {.path {file_name}}.",
        "i" = "The provided file in the spec entry {.code {spec$spec_id}} is not of type sas7bdat, but {file_ext}.",
        "i" = "No data set was built from spec entry {.code {spec$spec_id}} and NULL was returned.",
        "*" = "Please check your input or attach the data set instead."
      ))
      
      return(NULL)
    }
  }else {
    bds_full <- spec$data
  }
  
  pivot_input <- pivot_prepare_bds(
    bds_full  = bds_full,
    spec      = spec,
    values_fn = dupl_ctrl$values_fn,
    arrange   = dupl_ctrl$arrange,
    clean_fn  = names_ctrl$clean_fn, 
    names_sep = names_ctrl$names_sep
    #,  single_row = TRUE
  )
  
  
  # pivot   ####
  bds_pivot <- pivot_input$data
  
  bds_wide <- do.call(tidyr::pivot_wider, pivot_input)
  
  
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
    
  dict <- bds_pivot %>% 
    dplyr::mutate_at(pivot_input$names_from, names_ctrl$clean_fn) %>% 
    tidyr::unite(column, pivot_input$names_from, 
      remove = FALSE, 
      sep    = names_ctrl$names_sep
    ) %>% 
    dplyr::select(tidyselect::any_of(
      c("param" = spec$param, 
        "label" = spec$label,
        "unit"  = spec$unit, 
        "time"  = spec$time ,
        "column"
      ) %>% na.omit())
    ) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(source = spec$spec_id) %>% 
    dplyr::mutate(type   = 'bds') 
  
  # output ####
  list(
    data   = bds_wide,
    dict   = dict,
    source = list(file = spec$file, md5 = md5) 
  )
  
  
  
  
}


#' Prepare bds data for pivoting step in build
#'
#'Preparation of dataset bds_full as well as parameters to be passed to pivot_wider
#' in build_bds to allow for appropriate unit testing
#'
#' @param bds_full original bds-type data set
#' @param spec
#' @param filter subsetting
#' @param arrange 
#' @param value,param,time
#' @param id additional columns to reduce data set to actual columns required for pivoting
#' @param values_fn,names_sep simply written to the `pivot_args` output for the sake of completeness 
#' @param clean_fn
#'
#' @return
#' A list containing the pivot_wider arguments (pivot_args) as well as the function
#'  to clean column names (clean_fn). The `pivot_args` list includes the prepared data set (filtered, arranged) 
#'  as well as pivot_wider params (key(s), value, values_fn, names_sep)
#' 
#' @details 
#' Data preparation of bds_full for pivoting includes filtering and arranging
#' the data set before relevant columns are selected and renamed using `clean_fn` (`param`, `time` only)
#' If the prepared data set has more than one level in the `time` column, 
#' names_from will be a vector of the form `c(param, time)`
#'


pivot_prepare_bds <- function(
    # COMBAK add spec argument and deduce values_fn, currently done in build bds
    # then TEST IF values_fn in pivot_args is correct
    
    bds_full,
    spec, 
    # filter,
    # value, 
    # param, 
    # time,
    # id,
    values_fn = NULL,
    arrange   = NULL,
    clean_fn  = ~ stringr::str_replace_all(.x, '[:punct:]|[:space:]', '_'), 
    names_sep = '_'
    #,  single_row = TRUE
){
  
  
  # use duplicated control parameters from 'dupl_ctrl' argument in build_bds()
  # over duplicated controls in 'spec', if not NULL
  values_fn <- values_fn %||% spec$dupl_ctrl$values_fn
  arrange   <- arrange   %||% spec$dupl_ctrl$arrange
  
  # if(is.null(values_fn)){
  #   values_fn <- function(x) {ifelse(all(is.numeric(x)), mean(x, na.rm = TRUE), na.omit(x)[1])}
  # }
  

  
  # filter (and arrange) data set ####
  
  # columns to keep after filtering and arranging
  col_select <- spec[c("param", "time", "value", "id", "label", "unit")] %>% 
    unlist() %>% na.omit() %>% as.character()
  
  bds <- bds_full %>% 
    
    {if(length(spec$filter) > 0){ 
      dplyr::filter(., !!! rlang::parse_exprs(spec$filter))
    }else{.}
    } %>% 
    # TODO  reconsider, may lead to undocumented parameter exclusion if only NAs are present 
    #       (instead of documented by prepare_ml())
    dplyr::filter(! is.na(!! rlang::sym(spec$value))) %>% 
    dplyr::filter(stringr::str_squish(spec$param) != "") %>% 
    
    {if(!is.null(arrange)){ 
      dplyr::arrange(., !!! rlang::parse_exprs(arrange)) 
    }else{.}
    } %>% 
    
    dplyr::select(tidyselect::any_of(col_select)) %>% 
    # clean up columns potentially used for column names after pivoting
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c(spec$time, spec$param)),
        clean_fn  
      )
    )
  
  
  # names_from / multiple (?) time points ####
  # check if multiple time points are present after subsetting
  n_time <- ifelse(
    ! is.na(spec$time),
    bds %>% dplyr::pull(spec$time) %>% dplyr::n_distinct(),
    1
  )
  # TODO Later: adjust for single_row argument
  if(n_time > 1){
    names_from <- c(spec$param, spec$time)
  } else {
    names_from <- spec$param
    bds        <- bds %>% dplyr::select(-spec$time)
  }
  
  # check for duplicates
  # TODO move duplicate check and msg into pivot_prepare_bds()
  # and clarify that pivot_wider() defaults are used, if values_fn is NULL
  clmn_dupl <- c(spec$id, names_from)
  
  any_dupes <- bds %>% 
    dplyr::count(!!! rlang::syms(clmn_dupl)) %>% 
    dplyr::pull(n) %>% 
    magrittr::is_greater_than(1) %>% 
    any()
  
  if(any_dupes){
    
    msg_dupl <- c(
      'Duplicates wrt {.code {clmn_dupl}} were identified in {.arg {spec$spec_id}}.'
    )
    
    if(is.null(values_fn)){
      msg_dupl <- c(msg_dupl,
        '*' = 'Please refer to the {.fn build_bds} documentation of {.code dupl_ctrl} for details on duplicate handling.'            
      )
      
      values_fn <- function(x) {ifelse(all(is.numeric(x)), mean(x, na.rm = TRUE), na.omit(x)[1])}
    }else{
      msg_dupl <- c(msg_dupl, 
      'v' = 'The duplicate handling was applied as specified.')
    }
    
    cli::cli_inform(msg_dupl)
  
  }
  
  pivot_input <- list(
    data        = bds,
    id_cols     = spec$id,
    values_from = spec$value,
    names_from  = names_from,
    values_fn   = values_fn,
    names_sep   = names_sep
  )
  
  pivot_input
  
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

