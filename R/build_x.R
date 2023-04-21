#' Create wide format data following a given spec
#' 
#' Extract and reshape data from a single data set according to the given specification 
#' as created by `adam_spec_*()`. In addition, an (updated) dictionary is returned along with 
#' the md5 checksum of the specified file.
#' 
#' @param spec result of `adam_spec_*()`
#' @param dupl_ctrl bds only. A list with two entries
#' \itemize{
#' \item{\code{values_fn}} function to handle duplicates in pivoting step. see details section for default.
#' \item{\code{arrange}} expression passed to \code{arrange()} optional sorting of data set prior to pivoting, 
#' e.g. in order to select the first/last value by date. defaults to NULL. 
#' }
#' @param names_ctrl bds only. A list with two entries handling cleaning and renaming of columns after pivoting
#' \itemize{
#' \item{\code{clean_fn}} defaults to `stringr::str_replace_all(.x, '[:punct:]|[:space:]', '_')`.
#' \item{\code{names_sep}} defaults to '_' 
#' }
#'
#' @return 
#' A list with the following entries
#' \itemize{
#' \item{\code{data}}{a tibble in wide format with one row per \code{id}}
#' \item{\code{dict}}{a tibble listing the distinct combinations of columns
#' \code{param}, \code{label}, \code{unit}, \code{time}, \code{column}, \code{source} (if provided).} 
#' \item{\code{source}}{a list passing the \code{file} slot from the given \code{spec} that the created data set is based
#'  upon along with the md5 checksum of this file if `file` was provided, NULL otherwise}
#' \item{`flag_table`}{`build_adsl()` only. flag table is passed from `spec$flag_table` slot}
#' }
#' 
#' @details 
#' Note that the output dictionary may differ from the dictionary created by `adam_spec_*()`, 
#' as multiple features may be derived from a single parameter at different time points.  
#' 
#' \code{values_fn} is passed to \code{pivot_wider()}. The default is \code{function(x) {ifelse(all(is.numeric(x)), mean(x, na.rm = TRUE), na.omit(x)[1])}}
#'
#' @section Authors: 
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @name build_x
#' 
NULL