#' Prepare outcome from adtte for MARTINI
#'
#' Convenience function to prepare an outcome object from adtte-like data sets, either for tte or binarized endpoint.
#'
#' @param data adtte-like tibble, e.g. output of haven::read_sas('adtte.sas7bdat')
#' @param file path to ADaM-like tte data set, e.g. files/adtte.sas7bdat, ignored if data is provided
#' @param filter character vector to be applied to the data set (e.g. to select PARAMCD or subset to particular population)
#' @param cut optional numeric, required for binarized version from tte data. Note that cut must provided in the same unit as the time column (e.g. AVAL)
#' @param unit only relevant if outcome should be binarized, i.e. if `cut` is provided. 
#' `unit` should describe the unit of the numeric (time) column in data set ass well as of `cut`, and will be included in outcome values  
#' @param columns list defining the mapping of required information to columns in the data with default entries. See details for defaults. 
#' @param label optional label for the binarized outcome variable, defaults to '.out'
#' 
#' @details 
#' Note that `unit` is solely a description to be included in the outcome value (e.g. "event in first 2 year(s)"). 
#' No conversion of the time is done to `unit`.
#'
#' Column mapping defaults are chosen to match ADaM adtte data sets: 
#' \describe{
#'   \item{`id`}{`'SUBJID'`}
#'   \item{`time`}{`'AVAL'`}
#'   \item{`censor`}{`'CNSR'`}
#' }
#' If `status` instead of `censoring` indicator should be used, specify e.g. status = 'event' (defaults to NULL). 
#' If both are defined, `status` is used, `censor` is ignored.
#'
#'
#' @return
#' A tibble with column `.id` and either an additional character column `.out` for the binarized version 
#' or with the addition of the pair `.time` and `.status` for tte outcomes.
#' 
#' Observations with missing values in either `.time` or `.status` will be removed. 
#' In this case, a notification with the number of removed observations will be printed to the console.
#' 
#' 
#' Note that sample sizes may differ for binarization and tte outcome, as subjects are dropped if the observed time is censored and below `cut`.
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
  label   = '.out',
  columns = NULL
){
 
  columns_used <- list(
    id     = 'SUBJID',
    time   = 'AVAL',
    censor = 'CNSR',
    status = NULL
  )
  
  # INPUT ####
  # ... checks ####
  if(all(c(is.null(data), is.null(file)))){
    usethis::ui_stop(paste0('At least one of ', usethis::ui_code('data'), ' or ', usethis::ui_code('file'), ' need to be provided.'))
  }
  
  if(!is.null(cut) && !is.numeric(cut)){
    usethis::ui_stop(paste0(usethis::ui_code('cut'), ' needs to be numeric.'))
  }
  
  
  
  #  ... data ####
  if (is.null(data)){
    data <- haven::read_sas(file)
  }
  
  # ... columns ####
  columns_used <- purrr::list_modify(columns_used, !!!columns)
  
  # filter and prepare tte data ####
  data_prep <- data %>% 
    dplyr::filter(!!! rlang::parse_exprs(filter)) %>% 
    {if(!is.null(columns_used$status)){
      dplyr::mutate(., .status =     !!rlang::sym(columns_used$status))
    }else{
      dplyr::mutate(., .status = dplyr::if_else(!!rlang::sym(columns_used$censor) == 0, 1, 0))
    }} %>% 
    dplyr::rename(
      .id   = !!rlang::sym(columns_used$id),
      .time = !!rlang::sym(columns_used$time)
    )
  
  id_na <- data_prep %>% 
    dplyr::filter(is.na(.time) | is.na(.status)) %>% 
    dplyr::pull(.id)
  
  if (length(id_na) > 0){
    
    usethis::ui_info(paste0(
      length(id_na), " rows were removed due to missings.\n"
    ))
    
  }
  
  # OUTPUT ####
  out <- data_prep %>% 
    dplyr::filter(!is.na(.time) & !is.na(.status)) %>% 
    # ... binarization (optional) ####
    { if(!is.null(cut)){ 
        dplyr::mutate(.,
          .out = dplyr::case_when(
            # censored within cut -> drop
            .time <= cut & .status == 0 ~ NA_character_,
            .time <= cut & .status == 1 ~ paste0("event",    " in first ", ifelse(cut == 1, '', cut), " ", unit, "(s)"),
            .time >  cut                ~ paste0("no event", " in first ", ifelse(cut == 1, '', cut), " ", unit, "(s)"),
            TRUE                        ~ NA_character_
          )) %>% 
        dplyr::filter(!is.na(.out)) %>% 
        dplyr::select(-.time, -.status)
      }else{.}
    } %>% 
    
    # ... clean up ####
    dplyr::select(tidyselect::any_of(c('.id', '.out', '.time', '.status'))) %>% 
  
    labelled::set_variable_labels(
      .,
      .labels = list(
        .out    = label,
        .status = 'event indicator'
      ),
      .strict = FALSE
    )
  
  out
  
}

