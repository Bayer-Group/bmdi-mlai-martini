#' Prepare outcome object from adtte 
#'
#' @param data adtte
#' @param file path to adam-like tte data set, e.g. files/adtte.sas7bdat, ignored if data is provided
#' @param filter character vector to be applied to the data set (e.g. to select PARAMCD or subset to particular population)
#' @param cut optional numeric, required for binarized version from tte data. Note that unit must provided in the same unit as the time column (e.g. AVAL)
#' @param unit only relevant if outcome should be binarized, i.e. if `cut` is provided. 
#' `unit` should be original unit of numeric (time) column in data set, to be included in final label (event/no event after `cut unit`(s)).
#' @param columns list defining the mapping of required information to columns in the data with default entries. See details for defaults. 
#' @param label optional for the binarized outcome variable, defaults to '.out'
#' 
#' @details 
#' Column mapping defaults are chosen to match adam adtte data sets: 
#' id = SUBJID, time = 'AVAL', censor = 'CNSR'
#' If status instead of censoring indicator should be used, specify e.g. status = 'event'. If both are defined, status is used, censor is ignored.
#'
#' @return
#' 
#' @export
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @md

build_out_tte <- function(
  data    = NULL,
  file    = NULL,
  filter  = NULL,
  cut     = NULL,
  unit    = 'unit',
  columns = NULL
){
 
  columns_used <- list(
    id     = 'SUBJID',
    time   = 'AVAL',
    censor = 'CNSR',
    status = NULL
  )
  
  # input checks
  if(all(c(is.null(data), is.null(file)))){
    usethis::ui_stop(paste0('At least one of ', usethis::ui_code('data'), ' or ', usethis::ui_code('file'), ' need to be provided.'))
  }
  
  if(!is.numeric(cut)){
    usethis::ui_stop(paste0(usethis::ui_code('cut'), ' needs to be numeric.'))
  }
  
  
  
  #  data prep
  if (is.null(data)){
    data <- haven::read_sas(file)
  }
  
  columns_used <- purrr::list_modify(columns_used, !!!columns)
  
  data %>% 
    dplyr::filter(!!! rlang::parse_exprs(filter)) %>% 
    {if(!is.null(columns_used$status)){
      dplyr::mutate(., .status =     !!rlang::sym(columns_used$status))
    }else{
      dplyr::mutate(., .status = 1 - !!rlang::sym(columns_used$censor))
    }} %>% 
    dplyr::rename(
      .id   = !!rlang::sym(columns_used$id),
      .time = !!rlang::sym(columns_used$time)
    ) %>% 

    
    { if(!is.null(cut)){ 
        dplyr::mutate(.,
         .out = case_when(
            # censored within cut -> drop
            .time <= cut & .status == 0 ~ NA_character_,
            .time <= cut & .status == 1 ~ paste0("event",    " in first ", cut, " ", unit, "(s)"),
            .time >  cut                ~ paste0("no event", " in first ", cut, " ", unit, "(s)")
          )) %>% 
        dplyr::filter(!is.na(.out)) %>% 
        dplyr::select(-.time, -.status)
      }else{.}
    } %>% 
    
    dplyr::select(tidyselect::any_of(c('.id', '.out', '.time', '.status'))) %>% 
  
    labelled::set_variable_labels(
      .,
      .labels = list(
        .out = ifelse(is.null(label), '.out', label),
        .status = 'event indicator'
      ),
      .strict = FALSE
    )
  
}

