test_that("original_scale() works", {
  
  withr::with_seed(2116,{
    
    n <- 250
    d_feat <- tibble::tibble(
      .id = 1:n,
      sym1 = rnorm(n, mean = 10, sd =1),
      sym2 = rnorm(n, mean = 100, sd =5),
      skw1 = exp(rnorm(n, mean = 1, sd = 2)),
      skw2 = exp(rnorm(n, mean = 0, sd = 1)),
      skw_corr = skw2 *2,
      # add a constant (skewness = NaN)
      const = rep(1,n)
    )
    d_out <- tibble::tibble(
      .id  = 1:n,
      .out = rnorm(n)
    )
    
  })
  
  
  # log & norm
  x <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    train_prop = .75,
    prep_step_normalize = TRUE, 
    prep_step_log = !FALSE, # TRUE: only log, no undo
    check_feature = FALSE
  )
  
  get_trafo_params(x$recipe$prep)
  back_trafo_skw1 <- original_scale_fun(x$recipe$prep, 'skw1')
  get_data(x, type = 'prep', split_id = 'set')
  back_trafo_skw1(get_data(x, type = 'prep', split_id = 'set')$skw1)
  
  res_back_trafo <- original_scale(x)
  get_data(x, type = 'raw')
  
  # log only
  x <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    train_prop = .75,
    prep_step_normalize = FALSE, 
    prep_step_log = TRUE, # TRUE: only log, no undo
    check_feature = FALSE
  )
  
  res_back_trafo <- original_scale(x)
  
})

original_scale_fun <- function(x, term){ 

  stopifnot(inherits(x, "recipe"))
  stopifnot(
    purrr::map_lgl(x$steps, recipes::is_trained) %>% all()
  )
  
   info_back_trafo <- get_trafo_params(x)
   
   term_params <- info_back_trafo %>% dplyr::filter(terms == term)
   
   function(y){
     if (!is.na(term_params$sd)) {
       y <-  y * term_params$sd + term_params$mean
     }
     if (!is.na(term_params$base)) {
       y <- term_params$base ** y
     }
     y
   }
   
  
  }

original_scale <- function(x){ # x  object of class martini_ml 

  # recipe order: ... %>% log() %>% impute() %>% (unlog() %>% ) ... %>% normalize() %>% ...
  # reverse normalization first, then log
  
  stopifnot(inherits(x, "martini_ml"))
  
  info_back_trafo <- get_trafo_params(x$recipe$prep)
  
  # apply back trafo and return ####
  d_prep <- get_data(x, type = "prep", split_id = "set") 
  
  purrr::pwalk(
    info_back_trafo %>% 
      dplyr::filter(terms %in% colnames(d_prep)), 
    \(terms, base, mean, sd){
      if (!is.na(sd)) {
        d_prep[, terms] <<- d_prep[, terms] * sd + mean
      }
      if (!is.na(base)) {
        d_prep[, terms] <<- base ** d_prep[, terms]
      }
    })
  
  d_prep %>% 
    split(., .$set) %>% 
    purrr::map(~dplyr::select(.x, -tidyselect::any_of("set")))
  
}
   
get_trafo_params <- function(
    x # only uses $recipe$prep
    ){
  stopifnot(inherits(x, "recipe"))
  stopifnot(
    purrr::map_lgl(x$steps, recipes::is_trained) %>% all()
  )
  
  tidy_recipe <- recipes::tidy(x)
  
  relevant_steps <- tidy_recipe %>% 
    dplyr::filter(stringr::str_detect(type, "log_skewness|normali.e")) %>% 
    dplyr::select(type, number) %>% 
    tibble::deframe() %>% 
    purrr::map(~{recipes::tidy(x, .x)})
  
  info_back_trafo <- dplyr::full_join(
    relevant_steps$log_skewness %>% 
      purrr::discard_at(relevant_steps$log_skewness_undo$terms %||% character()) %>% 
      dplyr::select(terms, base),
    relevant_steps$normalize %>% 
      dplyr::select(terms, statistic, value) %>% 
      # pivoting empty data set results in tibble of size 0 x 1
      tidyr::pivot_wider(names_from = statistic),
    by = "terms"
  )
  
  info_back_trafo
  
}

to_original_scale(x)
  
#   to_original_scale2 <- function(x){ # x  object of class martini_ml 
#     
#     # recipe order: ... %>% log() %>% impute() %>% (unlog() %>% ) ... %>% normalize() %>% ...
#     # reverse normalization first, then log
#     
#     stopifnot(inherits(x, "martini_ml"))
#     
#     vars_back_trafo <- list() 
#     
#     #d_prep <- x$data$prep # has entries train and test
#     tidy_recipe <- recipes::tidy(x$recipe$prep)
#     has_log <- tidy_recipe$type %>% 
#       stringr::str_detect("log_skewness") %>% 
#       any()
#     has_norm <- tidy_recipe$type %>% 
#       stringr::str_detect("normali.e") %>% 
#       any()
#     
#   
#   # log ####
#   if (!has_log) {
#     info_log <- list()
#   } else{
#     # get vars for back trafo as 
#     # log trafo as setdiff() of vars from step_log_skewness() and 
#     tidy_log_unlog <- tidy_recipe %>% 
#       dplyr::filter(stringr::str_detect(type, "log_skewness"))
#     
#     numbers_log_unlog <- tidy_log_unlog %>% 
#       dplyr::select(type, number) %>% 
#       tibble::deframe()
#     
#     # nested list per log/unlog: 
#     # names of transformed variables, value is log base
#     info_log_unlog <-  purrr::map(
#       numbers_log_unlog,
#       ~ x$recipe$prep %>% 
#         recipes::tidy(.x) %>% 
#         # TODO check: terms & also for log_skewness_undo?
#         dplyr::select(tidyselect::any_of(c("terms", "base"))) %>% 
#         tibble::deframe() %>% 
#         as.list()
#     )
#     
#     vars_for_backtrafo <- setdiff(
#       # transformed...
#       names(info_log_unlog$log_skewness),
#       # ... no back transformation ...
#       names(info_log_unlog$log_skewness_undo)
#     ) %>% 
#       # ... still in prepped data set
#       intersect(colnames(x$data$prep$train))  
#     
#     # list with names of variables to be back transformed from log, value is log base
#     info_log <- info_log_unlog$log_skewness %>% 
#       purrr::keep_at(vars_for_backtrafo)
#       
#   }
#   
#   # normalization ####
#   if (!has_norm) {
#     info_norm <- list()
#   } else {
#     
#     number_norm <- tidy_recipe %>% 
#       dplyr::filter(stringr::str_detect(type, "normalize")) %>% 
#       dplyr::pull(number) 
#     
#     # list with names of variables to be back transformed from normalization, 
#     # with values mean and sd
#     info_norm <- recipes::tidy(x$recipe$prep, number_norm) %>% 
#       dplyr::select(-tidyselect::any_of("id")) %>% 
#       split(., .$terms) %>% 
#       purrr::map(
#         ~ dplyr::select(.x, -tidyselect::any_of("terms")) %>% 
#           tibble::deframe()
#       ) 
#     
#   }
#   
#   # combine trafo info ####
#   info_back_trafo <- purrr::list_merge(
#     info_norm,
#     info_log
#   )
#   
#   # apply back trafo and return ####
#   purrr::map(
#     d_split,
#     purrr::imap(\(info_back_trafo, var){
#       d_split %>% 
#         {if (!is.null(info_back_trafo$sd)){
#           dplyr::mutate(dplyr::across(var, function(x){(x*info_back_trafo$sd) + info_back_trafo$mean} ))
#         } else{.}} %>% 
#         {if (!is.null(info_back_trafo$base)){
#           dplyr::mutate(dplyr::across(var, function(x){ info_back_trafo$base ** x} ))
#         } else{.}} 
#     })
#   )
#     
# }