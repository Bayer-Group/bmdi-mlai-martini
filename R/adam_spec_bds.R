#' Create specification object for adam data sets of type 'bds'
#' 
#' Given a file containing a bds data set (e.g. adlb or advs), \code{\link{adam_spec_bds}()} will create a specification 
#' object for use in \code{\link{build_bds}()} to prepare the data to be used in machine learning. 
#' The main task is to collect the key columns
#' for reshaping the data into wide format and prepare the data filter.
#' 
#' @param data tibble with the data for which the specification is created
#' @param file the path of the sas file to process, ignored if data is provided
#' @param id name of id column to be kept and used for merge of data sets
#' @param param name of the column that identifies the parameter. Defaults to NULL, will be guessed if not set (see Details).
#' @param label name of the column that gives column labels. Defaults to NULL.
#' @param unit Defaults to NULL, will be guessed if not set (see Details).
#' @param time Defaults to NULL, will be guessed if not set (see Details).
#' @param value Defaults to NULL, will be guessed if not set (see Details).
#' @param filter character vector of filters to be applied to the bds data set. 
#' Individual filters will only be considered if the resulting data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data
#' @param domain character string to be included in dictionary. automatically derived for standard adam data sets. If not set for `data` provided, dictionary entry will be 'custom'.
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
#' @export

adam_spec_bds <- function(
  file        = NULL,
  data        = NULL,
  id          = 'SUBJID', 
  param       = NULL,
  label       = NULL,
  unit        = NULL,
  time        = NULL, 
  value       = NULL,
  filter      = NULL,
  attach_data = FALSE,
  domain      = NULL
){
  
  # initial check(s) ####
  
  if (all(c(is.null(data), is.null(file)))){
    usethis::ui_stop(paste0('At least one of ', usethis::ui_code('data'), ' or ', usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  # collect column name parameters ####
  
  col_select <- list(
    "id"    = id,
    "value" = value,
    "param" = param,
    "time"  = time,
    "unit"  = unit,
    "label" = label
  )
  
  # check/modify 'col_select' and set 'domain' ####
  
  if (!is.null(data)){
  
    # ... if 'data' is used ####
      
    bds      <- data
    md5      <- NULL
    coln_bds <- colnames(bds)
    
    # check if required columns are provided by user
    missing_params <- purrr::map_lgl(list(param = param, value = value, id = id), is.null) %>% which() %>% names()
    
    if (length(missing_params)>0){
      usethis::ui_stop(paste0(paste(usethis::ui_code(missing_params), collapse = ", "), " need(s) to be provided.\n"))
    }
    
    # set 'domain' to default, if not provided
    if(is.null(domain)) domain <- 'custom'
        
  }else{
    
    # ... if 'file' is used ####
    
    if (!file.exists(file)){
      usethis::ui_stop(paste0(usethis::ui_code("file"), " not found."))
    }
    
    if (tools::file_ext(file) != "sas7bdat") {
      usethis::ui_stop(paste0(usethis::ui_code("file"), " is not of type 'sas7bdat'."))
    }
    
    # ... ... import data ####
    bds      <- haven::read_sas(file) %>% dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
    coln_bds <- colnames(bds)
    
    md5  <- tools::md5sum(file) %>% as.character()
    size <- fs::file_size(file)
    
    
    # ... ... identify domain ####
    domain <- stringr::str_split( file, '/|\\\\') [[1]] %>%  
      tail(1) %>% 
      stringr::str_remove_all('^ad|[.]sas7bdat$') %>% 
      stringr::str_to_upper()
    
    dom <- stringr::str_sub(domain, 1, 2) # used in e.g. EGTEST (instead of EGFTEST)
    
    
    # ... ... define candidates for column names (based on 'dom') ####
    
    # TODO move guessing candidates to adam_guess()
    
    guess_value_num <- c('AVAL', 'AVALC',
                         paste0(dom, c("STRESN", "STRESC", "ORRES")))
    guess_value_cat <- c('AVALC', 'AVAL',
                         paste0(dom, c("STRESC", "STRESN", "ORRES")))
    
    dom_cat <- c("TR", "EGF") #
    
    guess_value <- if(dom %in% dom_cat){
      guess_value_cat
    } else {
      guess_value_num
    }
    
    guesses <- list(
      
      # candidates 'param'
      param = c('PARAMCD', paste0(dom, 'TESTCD')),
      
      # candidates 'time'
      time = c('AVISIT', 'VISIT', 'AVISITN', 'VISITN'),
      
      # candidates 'value'
      value = guess_value,
      
      # candidates 'unit'
      unit = c('AVALU', paste0(dom, 'STRESU'),  paste0(dom, 'ORRESU'))
      
    )
    
    # candidates 'label'
    guesses$label <- stringr::str_remove(c(param, guesses$param), 'CD$')
    
    
    # ... ... check data for candidate columns ####
    
    col_required <- c('value', 'param')
    
    # ... ... ... guessed columns ####
    
    for (i in names(guesses)){ 
      
      if (is.null(col_select[[i]])){
        
        choices <- guesses[[i]] %>% intersect(coln_bds)
        
        if (length(choices) == 0 && i %in% col_required){
          # add 'domain' for information when used within adam_spec()
          usethis::ui_info(crayon::silver(paste0(
            'AD', domain, ": No column could be identified to be used as ", i, ". No spec will be provided.\n")))
          return(NULL)
        }
        
        col_select[[i]] <- choices[1]
        
        if(is.na(col_select[[i]])) col_select[i] <- list(NULL)
        
      }
      
    }
    
    # ... ... ... 'id' (input parameter with default, not guessed) ####
    
    if (!id %in% coln_bds){
      usethis::ui_stop(
        paste0("AD", domain, ": The ", usethis::ui_code("id"), " column '", id, "' is not present in the data set.\n")
      )
    }
    
  }

  # column check ####
  
  purrr::iwalk(col_select, ~{
    if (!is.null(.x) && (length(intersect(.x, coln_bds)) == 0)) {
      usethis::ui_stop(crayon::silver(paste0(
        "The ", usethis::ui_code(.y), " column '", .x, "' is not available in the data set.\n")))
    }
  })
  
  # filter check ####
  
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(bds, filter, data_id = domain)$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]
 
  # dictionary ####
  
  # use unfiltered data 
  dict  <- bds %>% 
    dplyr::select( tidyselect::any_of(
      col_select[c('param', 'label', 'unit')] %>% unlist() %>% na.omit() 
    )) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(source = domain) %>% 
    dplyr::mutate(type   = 'bds') 
 
  # remove bds data set label automatically created by haven::read_sas()
  attr(dict, 'label') <- NULL
 
 
  # OUTPUT ####
 
  out <- list(
    file      = file,
    data      = NULL,
    md5       = md5,
    size      = size, 
    type      = "bds",
    filter    = actual_filter,
    dict      = dict,
    spec_id   = domain
  ) %>% 
    append(
      col_select
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
