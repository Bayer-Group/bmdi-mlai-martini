x <- martini_ml_regr


to_original_scale <- function(x){ # x  object of class martini_ml 

  # recipe order: ... %>% log() %>% impute() %>% (unlog() %>% ) ... %>% normalize() %>% ...
  # reverse normalization first, then log
  
  stopifnot(inherits(x, "martini_ml"))
  
  vars_back_trafo <- list() 
  
  d_prep <- x$data$prep # has entries train and test
  
  
  # TODO instead of separate steps check
  # tidy_recipe %>% dplyr::filter(stringr::str_detect(type, "log_skewness|normalize")) %>% list_rbind()
  #  
  tidy_recipe <- recipes::tidy(x$recipe$prep)
  has_log <- tidy_recipe$type %>% 
    stringr::str_detect("log_skewness") %>% 
    any()
  has_log <- 
  has_norm <- tidy_recipe$type %>% 
    stringr::str_detect("log_skewness") %>% 
    any()
  
  # log ####
  if (!has_log) {
    info_log <- list()
  } else{
    # get vars for back trafo as 
    # log trafo as setdiff() of vars from step_log_skewness() and 
    tidy_log_unlog <- tidy_recipe %>% 
      dplyr::filter(stringr::str_detect(type, "log_skewness"))
    
    numbers_log_unlog <- tidy_log_unlog %>% 
      dplyr::select(type, number) %>% 
      tibble::deframe()
    
    # nested list per log/unlog: 
    # names of transformed variables, value is log base
    info_log_unlog <-  purrr::map(
      numbers_log_unlog,
      ~ x$recipe$prep %>% 
        recipes::tidy(.x) %>% 
        # TODO check: terms & also for log_skewness_undo?
        dplyr::select(tidyselect::any_of(c("terms", "base"))) %>% 
        tibble::deframe() %>% 
        as.list()
    )
    
    vars_for_backtrafo <- setdiff(
      # transformed...
      names(info_log_unlog$log_skewness),
      # ... no back transformation ...
      names(info_log_unlog$log_skewness_undo)
    ) %>% 
      # ... still in prepped data set
      intersect(colnames(x$data$prep$train))  
    
    # list with names of variables to be back transformed from log, value is log base
    info_log <- info_log_unlog$log_skewness %>% 
      purrr::keep_at(vars_for_backtrafo)
      
  }
  
  # normalization ####
  if (!has_norm) {
    info_norm <- list()
  } else {
    
    number_norm <- tidy_recipe %>% 
      dplyr::filter(stringr::str_detect(type, "normalize")) %>% 
      dplyr::pull(number) 
    
    # list with names of variables to be back transformed from normalization, 
    # with values mean and sd
    info_norm <- recipes::tidy(x$recipe$prep, number_norm) %>% 
      dplyr::select(-tidyselect::any_of("id")) %>% 
      split(., .$terms) %>% 
      purrr::map(
        ~ dplyr::select(.x, -tidyselect::any_of("terms")) %>% 
          tibble::deframe()
      ) 
    
  }
  
  # combine trafo info ####
  info_back_trafo <- purrr::list_merge(
    info_norm,
    info_log
  )
  
  # apply back trafo and return ####
  purrr::map(
    d_split,
    purrr::imap(\(info_back_trafo, var){
      d_split %>% 
        {if (!is.null(info_back_trafo$sd)){
          dplyr::mutate(dplyr::across(var, function(x){(x*info_back_trafo$sd) + info_back_trafo$mean} ))
        } else{.}} %>% 
        {if (!is.null(info_back_trafo$base)){
          dplyr::mutate(dplyr::across(var, function(x){ info_back_trafo$base ** x} ))
        } else{.}} 
    })
  )
    
}