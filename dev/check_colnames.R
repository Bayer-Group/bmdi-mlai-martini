#' Check column names
#' 
#' Check for presence of column names in a data set
#'
#' @param colnames_check character vector, possibly named. column names to check 
#' against `colnames_data`
#' @param colnames_data character vector. column names of the data set to check
#' against
#' @param spec_id id of the spec that is checked
#' @param call the execution environment of a currently running function
#'
#' @return
#' An informative error, if an entry of `colnames_check` does not appear in
#'  `colnames_data`
#' 
check_colnames <- function(
    colnames_data, 
    colnames_check, 
    spec_id = NULL, 
    call = rlang::caller_env()
){
  
  purrr::iwalk(colnames_check, ~{
    
    if(!isTRUE(.x %in% colnames_data)){
      
      msg_spec <- if(is.null(spec_id)){
        NULL
      }else{
        paste0(spec_id, ": ")
      }
      
      msg_start <- if(is.null(.y)){
        "Column"
      }else{
        "The {.code {.y}} column"
      }
      
      cli::cli_abort(
        "{msg_spec}{msg_start} '{.x}' is not available in the data set.",
        call = call
      )
      
    }
    
  })
  
}
