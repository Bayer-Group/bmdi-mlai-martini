#' Prepare outcome object from adtte 
#'
#' @param data adtte
#' @param file path to adam-like tte data set, e.g. files/adtte.sas7bdat, ignored if data is provided
#' @param filter character vector to be applied to the data set (e.g. to select PARAMCD or subset to particular population)
#' @param cut optional, create binarized version from tte data
#' @param unit only relevant if outcome should be binarized, i.e. if `cut` is provided. 
#' `unit` should be original unit of numeric (time) column in data set, to be included in final label (event/no event after `cut unit`(s)).
#' @param columns list defining the mapping of required information to columns in the data with default entries. See details for defaults. 
#' 
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
  # NOTE further columns to be added for binarization
  
){
 
  columns_used <- list(
    id     = 'SUBJID',
    time   = 'AVAL',
    censor = 'CNSR',
    status = NULL, 
    LVDT   = 'LVDT'
  )
  
  # input checks
  if (all(c(is.null(data), is.null(file)))){
    usethis::ui_stop(paste0('At least one of ', usethis::ui_code('data'), ' or ', usethis::ui_code('file'), ' need to be provided.'))
  }
  
  #  data prep
  if (!is.null(data)){
    data <- haven::read_sas(file)
  }
  
  columns_used <- purrr::list_modify(columns_used, !!!columns)
  
  tte <- data %>% 
    dplyr::filter(!!! rlang::parse_exprs(filter)) %>% 
    {if(!is.null(columns_used$status)){
      dplyr::mutate(., .status = !!rlang::sym(columns_used$status))
    }else{
      dplyr::mutate(., .status = 1 - !!rlang::sym(columns_used$censor))
    }} 
  
  if (is.null(cut)){
    
    tte %>% 
      dplyr::select(
        .id   = !!rlang::sym(columns_used$id),
        .time = !!rlang::sym(columns_used$time),
        .status
      )
    
  } else {
    
  }
   
}


### Prepare

# ```{r}
# # date of last visit
# vsmax <- d_out$LVDT %>% as.Date %>% max
# 
# d_out_cat <- d_out %>% 
#   mutate(EVENT = case_when(
#     CNSR == 1 ~ "no event",
#     CNSR == 0 ~ "event"
#   )) %>% 
#   mutate(EV36M = case_when(
#     AVAL <= 36 & CNSR == 1 ~ "censored in first 36 months",
#     AVAL <= 36 & CNSR == 0 ~ "event in first 36 months",
#     AVAL >  36             ~ "no event in first 36 months"
#   )) %>% 
#   # potential full years at risk
#   mutate(FYATRISK = floor(interval(as.Date(STARTDT), vsmax)/years(1))) %>% 
#   mutate_at("EVENT", factor) %>% 
#   mutate_at(c("EV36M"), ~{factor(.) %>% fct_shift(., n = 1)}) %>% 
#   select(any_of(c("SUBJID", "FYATRISK", "EVENT", "EV36M")))
# 
# 
# var_label(d_out_cat) <- list(
#   FYATRISK  = "Number of full years until last visit",
#   EVENT     = "CV death or non-fatal CV event (yes/no)",
#   EV36M     = "CV death or non-fatal CV event in first 36 months (yes/no)"
# )
# 
# outcome_lab <- var_label(d_out_cat) %>% 
#   unlist() %>% 
#   enframe(name = "PARAM", value = "OUTCOME")
# 
# tbl_out <- d_out_cat %>% 
#   pivot_longer(-any_of(c("SUBJID", "FYATRISK")), names_to = "PARAM", values_to = "CLASS") %>% 
#   count(PARAM, CLASS) %>% 
#   right_join(outcome_lab, ., by = "PARAM") %>% 
#   add_count(OUTCOME, name = "N", wt = n) %>% 
#   mutate(prop = round(n/N, 3)) %>% 
#   select(-PARAM, -N)
# 
# tbl_out %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = c("hover", "responsive"), font_size = 13, full_width = FALSE, position = "left") %>% 
#   column_spec(1, bold = TRUE) %>% 
#   collapse_rows(1, valign = "top")
# ```
# 
# ***
#   
#   ```{r}
# d_out_cat <- d_out_cat %>% 
#   rename(.id = SUBJID) %>% 
#   mutate(EV36M = as.character(EV36M)) %>% 
#   mutate(EV36M = if_else(EV36M == "censored in first 36 months", NA_character_, EV36M) %>% factor()) %>% 
#   droplevels() %>% 
#   select(-FYATRISK)
# 
# var_label(d_out_cat)[c("EVENT", "EV36M")] <- outcome_lab %>% deframe() %>% .[c("EVENT", "EV36M")]
# ```

