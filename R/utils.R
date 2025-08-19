#' skewness
#' 
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
  out <- formals(fun) %>% as.list() %>% head(-1)  # freq_cut default in step_nzv is call 95/5
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


#' Correlation matrix in long format
#' 
#' Calculate correlation matrix of a numeric data set and stretch to long format for convenient filtering..
#'
#' @param x A tibble, data frame or matrix containing numeric columns to be correlated.
#' @param method,use Arguments that are passed to [stats::cor()]
#' @param shave logical. if TRUE, only the lower triangle of the correlation matrix is kept.
#'
#' @return
#' A tibble with the pair of variable names in the columns `x` and `y` and the 
#' corresponding correlation in the column `r`
#'
#' TODO deprecate

corrr_mini <- function(
    x,
    method = 'pearson',
    use    = 'pairwise.complete.obs',
    shave  = FALSE
){
  
  # keep numeric columns only
  x <- x %>% 
    tibble::as_tibble() %>% 
    dplyr::select(tidyselect::where(is.numeric))
  
  if(ncol(x) == 0) stop("Data does not contain any numeric columns.")
  
  if(ncol(x) >= 2){
    
    # compute correlations
    res_cor <- stats::cor(
      x = x, 
      method = method, 
      use    = use
    )
    
    # upper and lower triangle individually, 
    index_lower <- utils::combn(seq_along(colnames(res_cor)), 2) %>% t()
    index_upper <- index_lower[, 2:1]
    
    stretch_lower <- tibble::tibble(
      x = rownames(res_cor)[index_lower[ , 1]], 
      y = colnames(res_cor)[index_lower[ , 2]],  
      r = res_cor[index_lower]
    )
    stretch_upper <- NULL
    if(!shave){
      stretch_upper <- tibble::tibble(
        x = rownames(res_cor)[index_upper[ , 1]], 
        y = colnames(res_cor)[index_upper[ , 2]],  
        r = res_cor[index_upper]
      )
    }
    
    corr_tibble <- dplyr::bind_rows(stretch_lower, stretch_upper)
    
  }else{
    
    corr_tibble <- tibble::tibble(x = character(), y = character(), r = numeric())
    
  }
  
  corr_tibble
}

#' Stretch a correlation matrix to long format
#' 
#' Stretch a correlation matrix from [stats::cor()] to a long format tibble
#'
#' @param x a symmetric matrix containing the correlations
#' @param shave logical. if TRUE, only the lower triangle of the correlation 
#'   matrix is kept.
#'
#' @return
#' A tibble with the pair of variable names in the columns `x` and `y` and the 
#' corresponding correlation in the column `r`
#'

corr_stretch <- function(
    x,
    shave  = FALSE
){
  
  if (!rlang::inherits_any(x, "matrix")) 
    cli::cli_abort(c("!" = "{.code x} has to be a matrix."))
  
  if (!isSymmetric(x)) 
    cli::cli_abort(c("!" = "{.code x} has to be a symmetric matrix."))
  
  if(ncol(x) < 2) 
    cli::cli_abort(c("!" = "{.code x} has to have at least 2 columns."))
  
  # upper and lower triangle individually, 
  index_lower <- utils::combn(seq_along(colnames(x)), 2) %>% t()
  index_upper <- index_lower[, 2:1]
  
  stretch_lower <- data.frame(
    # start with a df to avoid column name `x` 
    # and object name `x` collusion
    x = rownames(x)[index_lower[ , 1]] ,
    y = colnames(x)[index_lower[ , 2]],
    r = as.data.frame(x)[index_lower]
  )
  
  stretch_upper <- NULL
  if(!shave){
    stretch_upper <- data.frame(
      x = rownames(x)[index_upper[ , 1]], 
      y = colnames(x)[index_upper[ , 2]],  
      r = as.data.frame(x)[index_upper]
    )
  }
  
  corr_tibble <- dplyr::bind_rows(stretch_lower, stretch_upper) %>% 
    tibble::as_tibble()
  
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

length0_to_null <- function(x) if (length(x) == 0) NULL else x
