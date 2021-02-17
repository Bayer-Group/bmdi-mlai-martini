#' Create wide format data following a given occds spec
#' 
#' Extract and reshape data from a single occds-type data set according to the given specification 
#' as created by \code{\link{adam_spec_occds}()}. In addition, an (updated) dictionary is returned along with 
#' the md5 checksum of the specified file.
#' 
#' @param spec result of \code{\link{adam_spec_occds}()}
#' 
#' @return 
#' A list with the following entries
#' \code{data} a tibble in wide format which one row per \code{id} 
#' \code{dict} a tibble listing the distinct combinations of columns \code{param}, \code{label}, \code{unit}, \code{time}, \code{column}, \code{source} (if provided). 
#' \code{source} a list passing the \code{file} slot from the given \code{spec} that 
#' the created data set is based upon along with the md5 checksum of this file
#' 
#' @details 
#' Note that the output dictionary may differ from the dictionary created by \code{\link{adam_spec_occds}()}, 
#' as multiple features may be derived from a single parameter at different time points.  
#'
#' @section Authors:
#' 
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @name build_x
#' 
NULL