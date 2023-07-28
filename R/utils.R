#' skewness ####
#' e1071 was only used for e1071::skewness in prepare_ml_vars,default type = 3 
#' @param x a numeric vector containing the values whose skewness is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return The estimated skewness of x.
#' 
skw <- function(x, na.rm = FALSE){
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  y <- y * ((1 - 1/n))^(3/2)
  y
} 


#' create alias for fct_explicit_na and fct_na_value_to_level based on
#' available forcats version
#'
#' @param f A factor (or character vector)
#' @param level Level to use for missing values: this is what NAs will be changed to
#'
#' @return a factor 
#'

fct_na_to_level <- function(f, level){
  
  v_forcats <- packageVersion('forcats')
  
  if(v_forcats < as.package_version('1.0.0')){
    forcats::fct_explicit_na(f, na_level = level)
  }else{
    forcats::fct_na_value_to_level(f, level = level)
  }
  
}



#' qsave mlobj and data.table() dictionary
#'
#' @param d_ml result of prepare_ml() 
#' @param file_prefix defaults to "mlai_mlobj_"
#' @param path,file_id used to build file path for result to be written to
#' @param show_list show DT of dictionary, default true
#'
#' @return
#' invisibly returns file path that results is `qs::qsave()`d to, build as path, file_prefix, file_id.
#' shows `DT::datatable()` of dictionary if `show_list = TRUE`
#' 
#' @export
#'
#'
list_and_export <- function(
    d_ml,
    file_prefix = "mlai_mlobj_",
    file_id,
    path,
    show_list = TRUE
){
  
  res_path <- file.path(
    path,
    paste0(file_prefix, file_id, ".qs")
  )
  
  qs::qsave(
    d_ml, 
    res_path
  )
  
  if(show_list){
    
    d_ml$dict %>% 
      filter(column %in% colnames(d_ml$data_prep$train)) %>% 
      mutate(source = factor(source)) %>% 
      DT::datatable(
        rownames = FALSE,
        filter = "top",
        selection = "single",
        extensions = c("Buttons"),
        options = list(
          lengthMenu = c(10,25,50),
          pageLength = 10,
          dom        = "lfrtBpi",
          scrollX    = TRUE,
          buttons    = list("excel")
        )
      )
    
  }
  
  invisible(res_path)
}

