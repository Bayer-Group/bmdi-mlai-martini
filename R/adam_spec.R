#' Creating a specification for building a wide format data set from ADaM data
#' 
#' @description 
#' `r lifecycle::badge('maturing')`
#' 
#' \code{\link{adam_spec}()} is a wrapper for the `adam_spec_*()` functions.
#' It creates a list of specifications on how to extract and process data from 
#' ADaM data sets in a given location. 
#' The resulting list can be passed to \code{\link{build}()}, where the 
#' created specs are applied and the generated data sets are combined into a single wide format data set.  
#'
#' @param path path to a directory containing ads files in `.sas7bdat` or `.rds` format
#' @param filter a character vector of conditions to be passed to \code{dplyr::filter()}, 
#' e.g. regarding visits, treatment arms or parameters. Defaults to `NULL`.
#' @param keep,drop character vectors controlling the subset of data sets in the given \code{path} 
#' to create the specification for (e.g. \code{c('adsl', 'advs'))}).
#'  If both \code{keep} and \code{drop} are specified, only \code{keep} will be used.
#'  Both default to NULL, which means that all (known) domains are included.
#' @param attach_data boolean indicating whether the imported raw data is included in 
#' the output. Defaults to `TRUE`.
#' @param id,trt id and treatment column names (see e.g. \code{\link{adam_spec_adsl}()} 
#' for details).
#' @param pre_study `r lifecycle::badge("deprecated")`. boolean.
#' Include only pre-study events from occurrence data sets 
#' (see \code{\link{adam_spec_occds}()} for details). Defaults to `FALSE`.
#' @param file_ext only rds and sas7bdat data sets are allowed (e.g. \code{file_ext = 'rds'}). User may select
#' only sas7bdat, only rds or set a priorization rule (\code{file_ext = c('rds', 'sas7bdat')}, see Details).
#' Defaults to c('rds', 'sas7bdat'), i.e. rds if available, sas7bdat else.
#' @param fct_levels optional list of named vectors providing code-decode pairs
#' and/or setting the level order for factors in an adsl data set
#' (see details section of [adam_spec_adsl()] for structure).
#' @param catalog_file path to the catalog file to be passed to
#' [haven::read_sas()] for adsl. Defaults to `NULL`.
#' Ignored if `file` is not a `.sas7bdat` file.
#' @inheritParams adam_domain_type
#' 
#' @details 
#' \code{adam_spec()} matches file names in the given path against an internal library
#' to decide on which `adam_spec_*()` function to use for which data set.
#' Only files in the library will be processed, the rest will be ignored. Names of unprocessed files will be printed to the console.
#' For those, specifications may be created manually using the appropriate `adam_spec_*()` function and appended to the specification list created by \code{adam_*_spec()}. 
#' 
#' By specifying e.g. \code{file_ext = 'rds'}, only rds data will be considered 
#' for building the specification. To use only sas7bdat, analogously specify 
#' by file extension \code{file_ext = 'sas7bdat'}.
#' Preferred file types can be specified using a character vector 
#' \code{file_ext = c('rds', 'sas7bdat')}: If the same file name is found in 
#' \code{path} with both extensions, the file with the former extension is used, 
#' the one with the latter ignored. For unambiguous file names (either only 
#' `.sas7bdat` or only `.rds`) 
#' both are used.
#'
#'
#' Individual filters are only applied if the resulting data set has a 
#' positive number of rows (ignoring those causing errors or yielding 
#' a 0-row data set). 
#'
#' Please refer to the documentations of the `adam_spec_*()` functions 
#' for full details.
#'
#' @return  
#' \code{adam_spec()} returns named list of specifications that can be
#'  passed to the \code{\link{build}()} function. 
#' Each element contains the specification for a single data set and 
#' is named with the domain abbreviation (e.g. adsl, adlb).
#' The list can be manually adjusted if required,
#' e.g. adding further specifications or altering existing ones. 
#' See the documentation
#' of the `adam_spec_*()` for a detailed description of the output object.
#' 
#' @seealso \code{\link{adam_spec_adsl}()}, 
#' \code{\link{adam_spec_bds}()},  \code{\link{adam_spec_occds}()}
#'
#' @examples
#' ads_path <- system.file("martini_example_study/ads", package = "martini")
#' adam_spec(ads_path)
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export

adam_spec <- function(
  path, 
  filter      = NULL,
  keep        = NULL,
  drop        = NULL,
  pre_study   = lifecycle::deprecated(),
  attach_data = TRUE,
  id          = "USUBJID", 
  trt         = "TRT01A",
  add_bds     = NULL,
  add_occds   = NULL,
  file_ext    = c("rds", "sas7bdat"),
  fct_levels  = NULL,
  catalog_file = NULL
){
  
  file_ext <- rlang::arg_match(file_ext, c("rds", "sas7bdat"), multiple = TRUE)
  stopifnot(length(file_ext) > 0)
  
  # deprecation ####
  if (lifecycle::is_present(pre_study)) {
    
    # Signal the deprecation to the user
    lifecycle::deprecate_warn(
      "0.6.5", 
      "adam_spec(pre_study = )", 
      "adam_spec(filter = )"
    )
    
    # Deal with the deprecated argument for compatibility
    pre_study <- FALSE
  }
  
  # identify type for selected files in path (adsl/bds/occds) #####
  file_info <- adam_domain_type(
    path, keep, drop, 
    add_bds = add_bds, 
    add_occds = add_occds,
    quiet = FALSE
  ) %>% 
    dplyr::filter(type != "none") %>% 
    dplyr::mutate(file_ext_fct = factor(file_ext, levels = !!file_ext)) %>% 
    dplyr::filter(!is.na(file_ext_fct)) %>% 
    dplyr::arrange(file_ext_fct) %>% 
    dplyr::distinct(domain, .keep_all = TRUE)
  
  if (!is.null(add_bds) && any(!add_bds %in% file_info$domain)) {
    
    usethis::ui_oops(paste0(
      "\nThe following domain(s) specified in ", "`add_bds`",
      " were not found in ", "`path`", ":\n  ",
      paste(setdiff(add_bds, file_info$domain), collapse = ", ") %>% 
        cli::style_bold() %>%  
        cli::col_blue()
    ))
  }
  
  if (!is.null(add_occds) && any(!add_occds %in% file_info$domain)) {
    
    usethis::ui_oops(paste0(
      "\nThe following domain(s) specified in ", "`add_occds`",
      " were not found in ", "`path`", ":\n  ",
      paste(setdiff(add_occds, file_info$domain), collapse = ", ") %>% 
        cli::style_bold() %>%  
        cli::col_blue()
    ))
  }
  
  spec <- list()
  
  # adsl spec ####
  
  if (any(file_info$type == "adsl")) {
    
    files_adsl <- file_info %>% 
      dplyr::filter(type == "adsl") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_adsl, ~ adam_spec_adsl(
          file = .x, id = id, trt = trt, 
          filter = filter, attach_data = attach_data,
          fct_levels = fct_levels,
          catalog_file = catalog_file
        ))
      )
    
  }
  
  # bds spec ####
  
  if (any(file_info$type == "bds")) {
    
    files_bds <- file_info %>% 
      dplyr::filter(type == "bds") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_bds, ~ adam_spec_bds(
          file = .x, id = id,
          filter = filter, attach_data = attach_data
        ))
      )
    
  }
  
  # occds spec ####
  
  if (any(file_info$type == "occds")) {
    
    files_occds <- file_info %>% 
      dplyr::filter(type == "occds") %>% 
      dplyr::select(domain, file) %>% 
      tibble::deframe()
    
    spec <- spec %>% 
      append(
        purrr::map(files_occds, ~ adam_spec_occds(
          file = .x, id = id,
          filter = filter, attach_data = attach_data
          #, pre_study = pre_study
        ))
      )
    
  }
  
  # filter messages ####
  info_filter(spec, filter = filter)
  
  # output ####
  
  # NOTE attribute will not be explicitly created, if filter is NULL
  attr(spec, "filter") <- filter
  
  purrr::walk(names(spec), ~{
    attr(spec[[.x]], "filter_ok")    <<- TRUE
    attr(spec[[.x]], "data_info_ok") <<- TRUE
  })
  
  class(spec) <- c("martini_spec", class(spec))
  
  spec
  
}
