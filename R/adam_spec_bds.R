#' Create specification object for adam data sets of type 'bds'
#' 
#' Given a file containing a bds data set (e.g. adlb or advs), \code{\link{adam_spec_bds}()} will create a specification 
#' object for use in \code{\link{build_bds}()} to prepare the data to be used in machine learning. 
#' The main task is to collect the key columns
#' for reshaping the data into wide format and prepare the data filter.
#' 
#' @param file the path of the sas file to process
#' @param id name of id column to be kept and used for merge of data sets
#' @param param name of the column that identifies the parameter. Defaults to NULL, will be guessed if not set (see Details).
#' @param label name of the column that gives column labels. Defaults to NULL.
#' @param unit Defaults to NULL, will be guessed if not set (see Details).
#' @param time Defaults to NULL, will be guessed if not set (see Details).
#' @param value Defaults to NULL, will be guessed if not set (see Details).
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data
#' 
#' @details
#' 
#' Values for arguments `param`, `label`, `unit`, `time` and `value` will be guessed if not provided. 
#' Guess will be the first of the following options that matches a column name (exact match).
#' 
#' \describe{
#'   \item{`param`}{`PARAMCD`, `**TESTCD`, with `**` reflecting the two letter domain abbrevation (e.g. `LB`)}
#'   \item{`label`}{substring of `param` with trailing `CD` removed}
#'   \item{`time`}{`AVISIT`, `VISIT`}
#'   \item{`value`}{`AVAL`, `**STRESN`, `**ORRES`, with `**` reflecting the two letter domain abbrevation}
#'   \item{`unit`}{`AVALU`, `**STRESU`, `**ORRESU`, with `**` reflecting the two letter domain abbrevation}
#' }
#' 
#' Function will escape if one of `param` or `value` are neither provided nor can be guessed. The other columns are optional.
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{`type`}{character string \code{bds}, generally giving the type of adam data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields valid and non-empty result when applied individually (using \code{\link{check_filter}()})}
#' \item{`id`}{passing unchanged input}  
#' \item{`param`, `label`, `value`, `unit`, `time`}{names of the key columns to be used in \code{\link{build_bds}()} for reshaping}
#' \item{`spec_id`}{character string, generally the name of the domain} 
#' \item{`dict`}{a tibble with unique combinations within the `param` and `label` column (if present in the data set) 
#' to be used as a data dictionary}
#' \item{`dupl_ctrl`}{a list of length 2 with parameters `values_fn` and `arrange` that are passed to \code{\link{build_bds}()} to handle pivoting for duplicated values. Both default to NULL.}
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 

adam_spec_bds <- function(
  file,
  id          = 'SUBJID', 
  param       = NULL,
  label       = NULL,
  unit        = NULL,
  time        = NULL, 
  value       = NULL,
  filter      = NULL,
  attach_data = FALSE
){
  
  # read bds ####
  bds      <- haven::read_sas(file) %>% 
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))

  
  md5      <- tools::md5sum(file) %>% as.character()
  coln_bds <- colnames(bds)
     
  
  if (! id %in% coln_bds){
    usethis::ui_stop(
      paste0("The column id = ", id, " is not present in the data set.\n")
    )
  }
  
  
  # identify domain ####
  domain <- stringr::str_split( file, '/|\\\\') [[1]] %>%  
    tail(1) %>% 
    stringr::str_remove_all('^ad|[.]sas7bdat$') %>% 
    stringr::str_to_upper()
  dom <- stringr::str_sub(domain, 1, 2) # used in e.g. EGTEST (instead of EGFTEST)
  
  # check user input: columns available in data set? ####
  col_select <- c(
    "value" = value,
    "param" = param,
    "time"  = time,
    "unit"  = unit,
    "label" = label
  )
  
  purrr::iwalk(col_select, ~{
    if (!is.null(.x) && (length(intersect(.x, coln_bds)) == 0)) {
      usethis::ui_info(crayon::silver(paste0(
        'AD', domain, ": Column '", .x, "' is not available in the data set. '",
        .y, "' will be guessed.\n")))
    }
  })
  
 
  # define candidates for relevant columns accordingly ####
  
  guesses <- list(
    
    # ... candidates param ####
    param = c('PARAMCD', paste0(dom, 'TESTCD')),
    
    # ... candidates time  ####
    time = c('AVISIT', 'VISIT', 'AVISITN', 'VISITN'),
    
    # ... candidates value  ####
    value = c('AVAL', 'AVALC',
              paste0(dom, c("STRESN", "STRESC", "ORRES"))),
    
    # ... candidates unit ####
    unit = c('AVALU', paste0(dom, 'STRESU'),  paste0(dom, 'ORRESU'))
    
  )
  
  # ... candidates label ####
  
  guesses$label <- stringr::str_remove(c(param, guesses$param), 'CD$')
  # TODO move guessing candidates to adam_guess()
  
  # check data for candidate columns ####
  
  col_required <- c('value', 'param')
  
  
   
  for (i in names(guesses)){ 
    
    if (is.null(col_select[i]) || !(col_select[i] %in% coln_bds)){
      
      choices <- guesses[[i]] %>% intersect(coln_bds)
      
      if (length(choices) == 0){
        # escape if required columns cannot be identified
        if (i %in% col_required) {
          usethis::ui_info(crayon::silver(paste0(
            'AD', domain, ": No column could be identified to be used as ", i, ". No spec will be provided.\n")))
          return(NULL)
          # else set to NULL (instead of character vector of length 0) -> throws error for replacement of length 0
        }# else {
        #  choices <- NULL
        #}
      }
      
      col_select[i] <- choices[1]
      
    }
    
  }
  
  # filter check ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(bds, filter)
  actual_filter <- filter[keep_filter]
 
  # dictionary  ####
  
  # use unfiltered data 
  dict  <- bds %>% 
    dplyr::select( tidyselect::any_of(
      col_select[c('param', 'label', 'unit')] %>%  na.omit() 
    )) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(source = domain) %>% 
    dplyr::mutate(type   = 'bds') 
 
  
  # remove bds data set label automatically created by haven::read_sas()
  attr(dict, 'label') <- NULL
 
 
  # output ####
 
  out <- list(
    file      = file,
    data      = NULL,
    md5       = md5,
    type      = "bds",
    id        = id,
    filter    = actual_filter,
    dict      = dict,
    spec_id   = domain
  ) %>% 
    append(
      col_select %>% as.list()
    ) %>% 
    c(
      list(dupl_ctrl = list(
        values_fn = NULL,
        arrange   = NULL
      ))
    )

  
  if(attach_data){
    out$data <- bds
  }
  
  out
  
}

# test area  ####
if(FALSE){
  
  require(tidyverse)
  require(haven)
  require(labelled)
  
  file = '../adegf.sas7bdat'
  id = 'SUBJID'
  param  =  NULL
  label  = NULL
  unit   = NULL # AVALU, xxSTRESU, ORESSU
  time   = NULL 
  value  = NULL #c(AVAL, CHG)
  filter = NULL
  
  # basic function call
  spec_res <- adam_spec_bds(file = file, id = id)
  spec_res %>%  str()
  
  # specify filter that is partially not applicable
  filter_test <- c("AVISIT == 'BASELINE'", "LBTESTCD == 'RHYNOS'")
  spec_res <- adam_spec_bds(file = file, id = id, filter = filter_test)
  spec_res$filter
  
  # specify value column that is not in the data
  spec_res <- adam_spec_bds(file = file, id = id, value = "VALUE")
  
}
