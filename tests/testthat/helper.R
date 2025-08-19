tibble_to_JSON <- function(x, drop = FALSE){
  if("tbl_df" %in% class(x)){
    jsonlite::toJSON(x, pretty = TRUE)
  }else{
    if(drop){NULL}else{x}
  }
}

# input: character vector of lines
# output: modified character vector of the same length
rm_rlang_msg_head <- function(x){
  dplyr::if_else(
    stringr::str_detect(x, "Filter"),
    "", x
  )
}

