tibble_to_JSON <- function(x){
  if("tbl_df" %in% class(x)){
    jsonlite::toJSON(x, pretty = TRUE)
  }else{
    x
  }
}

