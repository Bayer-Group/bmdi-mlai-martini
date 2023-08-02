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
  
  v_forcats <- utils::packageVersion('forcats')
  
  if(v_forcats < as.package_version('1.0.0')){
    forcats::fct_explicit_na(f, na_level = level)
  }else{
    forcats::fct_na_value_to_level(f, level = level)
  }
  
}
