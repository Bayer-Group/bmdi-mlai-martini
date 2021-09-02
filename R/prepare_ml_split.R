#' Split a prepared ML data set by factor
#' 
#' \code{prepare_ml_split()} allows to split a \code{\link{prepare_ml}} object by a factor variable, e.g. treatment. 
#' This approach is preferable over independent preparations of each data part if 
#' comparability of resulting models is required (e.g. between treatment groups or studies).
#' Note that the data preparation recipe is trained on the complete data set 
#' (instead of independent preparation) and the split happens after preparation is completed.
#' 
#' @param ml_obj Result of \code{\link{prepare_ml}()}.
#' @param by character. Name of the variable to split the ml object by. Must be a factor in 
#' \code{ml_obj$data_raw$train}.
#' 
#' @return 
#' 
#' A named list of length 'number of levels' of the \code{by} variable where each entry contains the parts 
#' of the \code{ml_obj} that correspond the respective factor level. 
#' Each entry has the same structure as the original \code{ml_obj}
#' and thus can be used in subsequent MARTINI modules.
#' As the \code{by} variable is constant in each data part per definition, 
#' it is removed from the prepared data, while being kept in raw versions.
#' 
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#'@export
#'


prepare_ml_split <- function(
  ml_obj, 
  by = ".trt"
){
  
  if(! is.factor(ml_obj$data_raw$train[[by]])){
    usethis::ui_stop(
      paste0("'", by, "' is not a factor.")
    )
  }
  levs  <- ml_obj$data_raw$train[[by]] %>% levels()
  
  purrr::map(1:length(levs), ~{ml_obj}) %>% 
    rlang::set_names(levs) %>% 
    purrr::imap(~{
      
      # remove 'by' from dictionary
      .x$dict <- .x$dict %>% dplyr::filter(column != by)
      
      #
      .x$data_raw$train <- .x$data_raw$train %>% dplyr::filter(!! rlang::sym(by) == .y)
      .x$data_raw$test  <- .x$data_raw$test  %>% dplyr::filter(!! rlang::sym(by) == .y)
      
      # add removal step to end of recipe, to remove 'by' after all prep steps are conducted
      .x$prep_recipe <- .x$prep_recipe %>% 
        recipes::step_rm(tidyselect::any_of(by), trained = TRUE, removals = by)
      
      .x$data_prep$train <- recipes::bake(.x$prep_recipe, new_data = .x$data_raw$train)
      .x$data_prep$test  <- recipes::bake(.x$prep_recipe, new_data = .x$data_raw$test )
      
      # split object contains raw data only (no removal of 'by' required)
      .x$split$data <- .x$split$data %>% 
        dplyr::filter(!! rlang::sym(by) ==.y) 
      
      .x$split$in_id  <- which( .x$split$data$.id %in% .x$data_raw$train$.id)
      
      .x
      
    })
  
}

