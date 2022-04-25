#' Build the feature matrix from various sources according to a specification object
#' 
#' @description 
#' `r lifecycle::badge('maturing')`
#' 
#' The `build()` function allows to build a machine learning data set from a specification object as provided
#' by \code{\link{adam_spec}()} (with or without data already attached). 
#' 
#' @param spec a specification object as provided by \code{\link{adam_spec}()} (either \code{spec} or \code{path} has to be provided)
#' @param join either function to join data sets (e.g. \code{dplyr::full_join()} or a character (vector) giving the names
#' of the data sets containing the .ids to keep (e.g. \code{join = c('adxb', 'adlb')}). defaults to \code{dplyr::inner_join}
#' 
#'
#' @return
#' 
#' \code{build()} returns a wide data set with one row per subject and standardized column names for the subject id (`.id`)
#' and the treatment variable (`.trt`), if it is provided in the \code{spec} object. Objects with additional information on
#' the data are provided in the attributes of the returned object.
#' 
#' \item{`dict`}{
#' \describe{
#'   \item{`param`}{original parameter name in the source data}
#'   \item{`column`}{column name of the variable in the returned data. `column` is derived from `param` by transforming
#'   it into a valid file name and possibly adding a time extension, if multiple time points are considered for a particular parameter.}
#'   \item{`label`}{parameter label}
#'   \item{`source`}{source id provided by the specification object. If created with \code{\link{adam_spec}()}, this is the name of the domain.}
#'   \item{`type`}{adam data type of the source data (adsl, bds or occds)}
#'   \item{`unit`}{parameter unit (if applicable)}
#'   \item{`time`}{measurement time point (if applicable)}
#'   \item{`spec_id`}{name of the corresponding spec entry (if applicable)}
#' }}
#' \item{`source`}{file path and md5 checksums of the source data sets}
#' 
#' @details 
#' Missing values in variables from occurrence data sets are interpreted as 'absence of event', 
#' whereas NAs in adsl and bds data are considered to be true missing values. 
#' For missing values in occds data after joining with other data sets, 
#' missing values are replace by 0 for numerics, an additional level 'none' is introduced for 
#' for factors.
#' 
#' 
#' @seealso \code{\link{build_adsl}()}, \code{\link{build_bds}()}, \code{\link{build_occds}()}
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export

build <- function(
  spec, 
  join        = dplyr::inner_join
){
  
  # add names to the spec if none are provided
  if(is.null(names(spec))) names(spec) <- rep('', length(spec))
  
  for (i in 1:length(spec)){
    if(is.null(spec[[i]]$"spec_id")) {
      spec[[i]]$"spec_id" <- names(spec)[i]
    }
  }
  
  # call the appropriate build_*() function
  built_data <- purrr::map(spec,  ~{
    
    do.call( paste0('build_', .x[['type']]), list(.x))
    
  })
  
  
  # create output object ####
    
  # ... handle duplicate variable names across domains/sources ####
  rename_dupes <- purrr::imap_dfr(built_data, ~{
    .x[['data']] %>% names() %>% tibble::as_tibble_col() %>% dplyr::mutate(spec_id = .y)
  })  %>% 
    tidyr::unite(new_name, value, spec_id, sep = '_', remove = FALSE) %>% 
    dplyr::rename('old_name' = 'value') %>% 
    dplyr::add_count(old_name) %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::filter(! old_name %in% c('.id')) %>% 
    dplyr::select(-n)
  
  built_data <- purrr::imap(built_data, ~{
    
    out <- .x
    
    out[['dict']] <- out[['dict']] %>% mutate(spec_id = .y)
    
    rename_y <- rename_dupes %>% 
      dplyr::filter(spec_id == .y) %>% 
      dplyr::select(new_name, old_name) %>% 
      tibble::deframe()
    
    if(length(rename_y) > 0){
      out[['data']] <- out[['data']] %>% dplyr::rename(tidyselect::any_of(rename_y))
      out[['dict']] <- out[['dict']] %>% 
        dplyr::left_join(rename_dupes, by = c("column" = "old_name", "spec_id")) %>% 
        dplyr::mutate(column = dplyr::case_when(
          !is.na(new_name) ~ new_name,
          TRUE             ~ column
        )) %>% 
        dplyr::mutate(param = dplyr::case_when(
          !is.na(new_name) ~ paste0(param, "_", .y),
          TRUE             ~ param
        )) %>% 
        dplyr::mutate(label = dplyr::case_when(
          !is.na(new_name) ~ paste0(label, " (", .y, ")"),
          TRUE             ~ label
        )) %>% 
        dplyr::select(-new_name)
    }
    
    out
    
  })
  
  
  # ... dict  #### 
  prepped_dict <- purrr::map_dfr(built_data, 'dict') 
  
  # ... source  #### 
  prepped_source <- purrr::imap(built_data, ~{

    source_lst <- .x[["source"]]
    
    # file and md5 are NULL if spec was built from data instead of file
    if(source_lst %>% purrr::map_lgl(is.null) %>% all()){
      source_tbl <- tibble(file = NA_character_, md5 = NA_character_)
    }else{
      source_tbl <- .x[["source"]] %>% tibble::as_tibble_row()
    }
    
    source_tbl %>% 
      dplyr::mutate(spec_id = .y, .before = 1)
  }) %>% 
    purrr::reduce(dplyr::bind_rows) 
  
  # ... data ####
  # identify subjects from selected data sets to filter prepped_join
  # (if join is not a fct)
  if(! is.function(join)){
    if(any(join %in% names(built_data) )) {
      join_ids <- purrr::map(built_data[join %>%  intersect(names(built_data))], ~.[['data']]) %>% 
        purrr::reduce(dplyr::full_join, by = '.id') %>% 
        dplyr::pull(.id)
      join_filter <- ' .id %in% join_ids'
    }else{
      join_filter <- 'TRUE'
      usethis::ui_info("The domain(s) specified for 'join' are not available in the given spec. dplyr::full_join was used without additional filters.\n")
    }  
  }
  
  
  # combine and filter
  prepped_join <- purrr::map(built_data, 'data') %>% 
    {if(is.function(join)){
      purrr::reduce(., join, by = '.id') 
    }else{
      purrr::reduce(., dplyr::full_join, by = '.id') %>% 
        dplyr::filter(!! rlang::parse_expr(join_filter))  
    }}
  
  # NOTE 
  # extract all occds columns for explicit factor na
  # missing values occurring from occurrence data mean 'absence of event', whereas NAs in bds data are true missing values
  # -> replace missings by 0 for numerics, level 'none' for factors
  vars_fct_expl_na <- prepped_dict %>% 
    dplyr::filter(type == 'occds') %>% 
    dplyr::pull(column)
  
  prepped_join <- prepped_join %>%  
    dplyr::mutate_at(dplyr::vars(tidyselect::any_of(vars_fct_expl_na)), ~{
      if(is.numeric(.x)){
        tidyr::replace_na(.x, replace = 0L )
      }else{
        forcats::fct_explicit_na(.x, na_level = 'no') %>% 
          forcats::fct_shift(n = -1)
      }  
    }) %>% 
    droplevels()
  
  out                 <- prepped_join
  attr(out, "dict")   <- prepped_dict
  attr(out, "source") <- prepped_source
  
  out
  
}

# test area ####
if(FALSE){
  
  path <- "data/99999/ads"
  filter <- c("SEX == 'F'", "AVISIT == 'BASELINE'")
  keep <- c("adsl", "adxb")  
  wide <- build(path = path, keep = keep)
  
}




