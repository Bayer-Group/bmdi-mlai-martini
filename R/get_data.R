#' Extract data from an ml object
#' 
#' Combine splitted data (training and test, if available) from either prepared or raw data
#'
#' @param ml_obj ml object as returned by \code{\link{prepare_ml}()}
#' @param type either 'prep' or 'raw', determining which state of the data should be extracted. Defaults to `prep`.
#' @param split_id Add column indicating split origin (train/test). Omitted if NULL (default)
#'
#' @return 
#' result of `dplyr::bind_rows()` of data sets in `ml_obj` of the chosen type, either with or without an added `train_test` column.
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @export

get_data <- function(
  ml_obj, 
  type = c('prep', 'raw'),
  split_id = NULL
  ){
  
  type <- rlang::arg_match(type)
  
  d_type <- ml_obj[[paste0('data_', type)]]
  
  dplyr::bind_rows(
    train = d_type$train, 
    test  = d_type$test,  # is NULL if no split was done
    .id   = split_id
  )
  
}