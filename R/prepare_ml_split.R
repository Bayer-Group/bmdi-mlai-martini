#' Split a prepared ML data set by factor
#'
#' @description 
#' `r lifecycle::badge('maturing')`
#' 
#' \code{prepare_ml_split()} allows to split a \code{\link{prepare_ml}()} 
#' object by a factor variable, e.g. treatment. 
#' This approach is preferable over independent preparations of each data part 
#' if comparability of resulting models is required 
#' (e.g. between treatment groups or studies).
#' Note that the data preparation recipe is trained on the complete data set 
#' (instead of independent preparation) and the split happens after
#'  preparation is completed.
#' 
#' @param ml_obj Result of \code{\link{prepare_ml}()}.
#' @param by character. Name of the variable to split the ml object by.
#' Must be a factor in \code{ml_obj$data$raw$train}.
#' 
#' @return 
#' 
#' A named list of length 'number of levels' of the \code{by} variable 
#' where each entry contains the parts 
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
#' @export

prepare_ml_split <- function(
  ml_obj, 
  by = ".trt"
){
  
  d_raw_train  <- ml_obj$data$raw$train
  d_prep_train <- ml_obj$data$prep$train
  
  var_names_raw <- names(d_raw_train)
  
  if(!by %in% var_names_raw){
    cli::cli_abort(c(
      "Variable {.code by} needs to be a column in the raw data of the ml object.",
      'x' = 'You provided {.code by = {by}} which could not be found.'
    ))
  }
  
  if(! is.factor(d_raw_train[[by]])){
    
    cli::cli_abort(c(
      "Variable {.code by} needs to be a factor in the raw data of the ml object.",
      'x' = 'You provided {.code by = {by}} of class {.cls {class(ml_obj$data$raw$train[[by]])}}.'
    ))
    
  }
  
  levs  <- d_raw_train[[by]] %>% levels()
  
  # split variables to remove in the preparation process
  # ('by' or dummy-/one-hot encoded version of 'by')
  vars_remove <- paste0(by, "_", levs) %>% 
    c(by) %>% 
    intersect(names(d_prep_train))
  
  # start by duplicating the full ml object...
  purrr::map(1:length(levs), ~{ml_obj}) %>% 
    rlang::set_names(levs) %>% 
    
    # ... then filter and adjust
    # .x <- ml_obj 
    purrr::imap(~{
      
      # remove 'by' from dictionary
      .x$dict <- .x$dict %>% dplyr::filter(param != by)
      
      # split raw data
      .x$data$raw$train <- .x$data$raw$train %>% dplyr::filter(!! rlang::sym(by) == .y)
      if(!is.null(.x$data$raw$test)){
        .x$data$raw$test  <- .x$data$raw$test %>% dplyr::filter(!! rlang::sym(by) == .y)
      }else{.x$data$raw$test <- NULL}
      
      # add removal step to end of recipe, to remove 'by' after all prep steps are conducted
      .x$prep_recipe <- .x$prep_recipe %>% 
        recipes::step_rm(tidyselect::any_of(vars_remove), removals = vars_remove) %>% 
        recipes::prep()
        # from ?prep(): Also, if a recipe has been trained using prep() and then steps are added, prep() will only update the new operations. 
        
      .x$data$prep$train <- recipes::bake(.x$prep_recipe, new_data = .x$data$raw$train)
      if(!is.null(.x$data$prep$test)){
       .x$data$prep$test  <- recipes::bake(.x$prep_recipe, new_data = .x$data$raw$test)
      }else{
        .x$data$prep$test  <- NULL}
      
      # split object contains raw data only (no removal of 'by' required)
      if(!is.null(.x$split)){
        .x$split$data <- .x$split$data %>% 
          dplyr::filter(!! rlang::sym(by) ==.y) 
      
        .x$split$in_id  <- which( .x$split$data$.id %in% .x$data$raw$train$.id)
      } 
      
      
      .x
      
    })
  
}

