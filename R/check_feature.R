#' check feature matrix
#' 
#' Running `check_feature()` is by default run in [prepare_ml()] on the 
#' input `feature` to notify the user on sources of potential issues. 
#' 
#'
#' @param x feature matrix to check, such as the output of [build()].
#' @param check_low_freq,check_other,check_missing,check_count 
#' logicals to control which checks to include. All default to TRUE.
#' @param quiet logical controlling whether any informative messages are 
#' printed to the console
#' @param verbose TRUE by default, FALSE results in messages used for 
#' [prepare_ml()]
#' @param thres_count [check_count()]
#' @param thres_low_freq passed to [check_freq()]
#' @param thres_missing passed to [check_non_missing()]
#' @param ... arguments to be passed to other methods
#'
#'
#' @return a nested list, where each entry has a `vars` entry that contains
#' column names that do not pass the respective check (if check is included), 
#' character of length 0 for empty sets.
#' NULL if not tested at all.
#' @export
#'
check_feature <- function(
  x,
  check_low_freq = TRUE,
  check_other    = TRUE,
  check_missing  = TRUE,
  check_count    = TRUE,
  quiet          = TRUE,
  verbose        = TRUE,
  thres_count    = 30,
  thres_low_freq = NULL,
  thres_missing  = NULL,
  ...
){
  

all_args <- rlang::dots_list(..., .homonyms = "error")
  
out <- list()

# check_freq ####
if(check_low_freq){
  out$low_freq <- check_freq(x, thres = thres_low_freq, quiet = quiet)
} else {
  out$low_freq <- list(NULL)
}


# other_ml ####
# inform about potentially ambiguous 'other_ml' group 
if (check_other) {
  out$other <- check_other_class(x, other2_class = NULL, quiet = quiet)
} else {
  out$other <- NULL
} 

# proportion NA ####
if (check_missing) { 
  out$missing <- check_non_missing(x, thres = thres_missing, quiet = quiet)
} else {
  out$missing <-  NULL
}

# 
# # counts ####
if (check_count) {
  out$count <- check_count(x, thres = thres_count, quiet = quiet)
} else {
  out$count <-  NULL
}


# RETURN ####


# cli::cli_inform(c(
# "Note that {other2_class} is already a value in column{?s} {cols_with_class_other2}.",
# "*" = "Run {.fn check_feature} on your feature matrix for details."
# ))
out

}
