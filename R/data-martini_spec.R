#' Example specification object.
#' 
#' Specification object of the included example study in \code{system.file("ads", package = "martini")}
#' as created by \code{\link{adam_spec()}}.
#' 
#' @format 
#' A list with the entries \code{adsl}, \code{admh} and \code{advs}. See the documentation of
#' \code{\link{adan_spec}()} for further details on the object structure.
#' 
#' @source 
#' Simulated data
#'

"martini_spec"

# path to the sas-files of the example study
ads_path <- system.file("martini_example_study/ads", package = "martini")
if(is.null(ads_path)) ads_path <- "inst/martini_example_study/ads"

filters  <- c(
  # intent-to-treat population
  "ITTFL  == 'Y'",
  # only baseline data
  "AVISIT == 'Baseline'",
  # some MH entries have a Y/N coding (but not all)
  "MHOCCUR != 'N'"
)

martini_spec <- adam_spec(ads_path, filter = filters, attach_data = TRUE, pre_study = TRUE)

martini_spec$admh$count <- FALSE
martini_spec$admh$label <- "MHDECOD"

usethis::use_data(martini_spec, overwrite = TRUE)
