tibble_to_JSON <- function(x, drop = FALSE){
  if("tbl_df" %in% class(x)){
    jsonlite::toJSON(x, pretty = TRUE)
  }else{
    if(drop){NULL}else{x}
  }
}

