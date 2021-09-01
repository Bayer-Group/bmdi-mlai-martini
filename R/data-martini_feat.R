#' Example feature matrix.
#' 
#' Feature matrix of the included example study in \code{system.file("ads", package = "martini")}
#' created by \code{\link{build}()} from \code{\link{martini_spec}}.
#' 
#' @format 
#' A tibble with one row per subject and the features in the column. See the documentation of
#' \code{\link{build}()} for details on the object structure.
#' 
#' @source 
#' Simulated data
#'

"martini_feat"

data(martini_spec)

martini_feat <- build(martini_spec)

usethis::use_data(martini_feat, overwrite = TRUE)
