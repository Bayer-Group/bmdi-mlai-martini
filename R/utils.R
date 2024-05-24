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
    forcats::fct_explicit_na(f, level = level)
  }else{
    forcats::fct_na_value_to_level(f, level = level)
  }
  
}


#' get fct defaults
#'
#' @param fun function name
#' @param arg character (vector) of names of arguments to show default values for (exact match required)
#' @param unlist,unname logicals
#'
#' @return depending on values unlist and unname either a list or a vector
#'
get_default <- function(
    fun, 
    arg = NULL,
    unlist = TRUE, 
    unname = TRUE
  ){

  # pmatch arg to out names
  out <- formals(fun) %>% as.list() %>% purrr::compact()  # freq_cut default in step_nzv is call 95/5
  if(!is.null(arg)) out <- out %>% purrr::keep_at(arg) 
  out <- purrr::map(out, ~{if(is.call(.x)) eval(.x) else .x})
  
  if(unlist) out <- unlist(out)
  if(unname) out <- unname(out)
  
  out 
  
}

if(FALSE){
  
  # TODO  improve and test get_default()
  
  get_default(prepare_ml_feature, unname = FALSE, arg =  "level_other")
  get_default(prepare_ml_feature, unname = FALSE, arg = c("level_other", 'vars_fct_expl_na'), unlist = FALSE)
  
}




# TODO argument, shave (upper/lower)
# TODO document

corrr_mini <- function(
    x,
    method = 'pearson',
    use    = 'pairwise.complete.obs'
){
  
  # keep numeric columns only
  x <- x %>% 
    purrr::keep_at(purrr::map_lgl(x, is.numeric) %>% which())
  
  # compute correlations
  res_cor <- stats::cor(
    x = x, 
    method = method, 
    use    = use
  )
  
  # upper and lower triangle individually, 
  index_upper <- combn(seq_along(colnames(res_cor)), 2) %>% t()
  index_lower <- rev(index_upper)
  
  stretch_upper <- tibble::tibble(
    x = rownames(res_cor)[index_upper[ , 1]], 
    y = colnames(res_cor)[index_upper[ , 2]],  
    r = res_cor[index_upper]
  )
  stretch_lower <- tibble::tibble(
    x = rownames(res_cor)[index_lower[ , 1]], 
    y = colnames(res_cor)[index_lower[ , 2]],  
    r = res_cor[index_lower]
  )
  
  corr_tibble <- bind_rows(stretch_upper, stretch_lower)
  
  corr_tibble
}
# 
# # for all (numeric) variables identify highly correlated variables from d_train_nocorr
# corr_tibble <- corrr::correlate(
#   d_train_nocorr %>% 
#     dplyr::select(tidyselect::any_of(
#       rcp_prep_nocorr$var_info %>% 
#         dplyr::filter(role == "predictor") %>% 
#         dplyr::pull(variable)
#     )) %>% 
#     dplyr::select_if(is.numeric),
#   method = corr_method, 
#   use    = corr_use,
#   quiet  = TRUE
# ) %>% 
#   # corrr::shave() %>% repeats, but convenient for filtering
#   corrr::stretch(na.rm = TRUE) %>%  
#   dplyr::filter(abs(r) > thres_used$thres_corr)

