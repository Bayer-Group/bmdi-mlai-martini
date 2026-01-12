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
      const = rep(1,n) # (skewness NaN for constant)
    )
    d_out <- tibble::tibble(
      .id  = 1:n,
      .out = rnorm(n)
    )
    
  })
  
  
  # log & norm
  res_prep <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    train_prop = .75,
    prep_step_normalize = TRUE, 
    prep_step_log = !FALSE, # TRUE: only log, no undo
    check_feature = FALSE
  )
  
  x <- res_prep$recipe$prep
  
  get_trafo_params(x)
  back_trafo_skw1 <- original_scale_fun(x, 'skw1')
  get_data(x, type = 'prep', split_id = 'set')
  back_trafo_skw1(get_data(x, type = 'prep', split_id = 'set')$skw1)
  
  res_back_trafo <- original_scale(x)
  get_data(x, type = 'raw')
  
  # log only
  res_prep <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out, 
    train_prop = .75,
    prep_step_normalize = FALSE, 
    prep_step_log = TRUE, # TRUE: only log, no undo
    check_feature = FALSE
  )
  
  x <- res_prep$recipe$prep
  res_back_trafo <- original_scale(x)
  
})

#' function for back transformation 
#' 
#' Create a function for back transformation of a single term from a trained 
#' recipe. Steps considered are [step_log_skewness()] and 
#' [recipes::step_normalize()]
#'
#' @param x a [recipes::fully_trained()] recipe
#' @param term term to extract (trained) transformation parameters for
#'
#' @return
#' @export
#'
original_scale_fun <- function(x, term, info_back_trafo = NULL){ 
  
  # TODO polish error 
  stopifnot(inherits(x, "recipe"))
  stopifnot(recipes::fully_trained(x))
  
  if (is.null(info_back_trafo)) {
    info_back_trafo <- get_trafo_params(x)
  } else{# rough check
    stopifnot(inherits(info_back_trafo, "df"))
    stopifnot("terms" %in% colnames(info_back_trafo))
    stopifnot(
      ("base" %in% colnames(info_back_trafo)) &&
        all(c("mean", "sd") %in% colnames(info_back_trafo)) 
    )
  }
  
  stopifnot(term %in% info_back_trafo$terms)
  
  term_params <- info_back_trafo %>% dplyr::filter(terms == term)
  
  function(y){
    if (!is.na(term_params$sd)) {
      y <- y * term_params$sd + term_params$mean
    }
    if (!is.na(term_params$base)) {
      y <- term_params$base ** y
    }
    y
  }
  
}

test_that("original_scale_fun() works", {
  
  x <- martini_ml_class$recipe$prep
  #  get_trafo_params(x)
  original_scale_fun(x, info_back_trafo = NULL)
})

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
   
test_that("get_trafo_params() works", {
  # get_trafo_params() works ####
  
  # log AND unlog steps-> no back trafo required
  params_shared <- list(
    feature    = martini_feat,
    outcome    = martini_outc_class, 
    check_feature = FALSE,
    quiet = TRUE
  )
  x <- do.call(
    prepare_ml,
    list(
      params_shared, 
      prep_step_normalize = FALSE, 
      prep_step_log = FALSE)
  )$recipe$prep
  
   get_trafo_params(x)
   
   # no trafo steps
   x <- prepare_ml(
     feature    = martini_feat,
     outcome    = martini_outc_class, 
     prep_step_normalize = FALSE, 
     prep_step_log = FALSE, # TRUE: only log, no undo
     check_feature = FALSE,
     quiet = TRUE
   )$recipe$prep
   
   get_trafo_params(x)
  
  
  
})

#' Extract transformation parameters from recipe
#' 
#' Parameters required for back transformation are extracted only from 
#' [recipes::step_normalize()] and the custom martini step (combination)
#' [step_log_skewness()]/[step_log_skewness_undo()]
#' 
#'
#' @param x a fully trained recipe, e.g. the entry martini_ml$recipe$prep
#'
#' @return If at least one term is on a different scale in the $prep 
#' data slot, a tibble with column names terms, base, mean and sd.
#' 
#' NULL if no back transformation is required, either because the 
#' respective steps were not included in the recipe or if log transformation 
#' log transformation was done just for imputation and has already been 
#' reversed in a recipe step.
#' @export
#'
#'
get_trafo_params <- function(
    x 
    ){
  
  stopifnot(inherits(x, "recipe"))
  stopifnot(recipes::fully_trained(x))
  # would work if only log/undo or normalize are trained (if part of recipe)
  
  tidy_recipe <- recipes::tidy(x)
  
  no_back_trafo <- tidy_recipe$type %>% 
    purrr::none(~stringr::str_detect(., "log_skewness|normali.e"))
  
  # no_back_trafo_out <- tibble::tibble(
  #   terms = character(), 
  #   log_base = numeric(),
  #   sd = numeric(),
  #   mean = numeric()
  # )
  # exit due to no log_skewness or normalize step
  if(no_back_trafo) return(NULL) #return(no_back_trafo_out)
  
  relevant_steps <- tidy_recipe %>% 
    dplyr::filter(stringr::str_detect(type, "log_skewness|normali.e")) %>% 
    dplyr::select(type, number) %>% 
    tibble::deframe() %>% 
    purrr::map(~{recipes::tidy(x, .x) %>% dplyr::select(-id)})
  
  params_back <- list()
  
  # get terms that were actually transformed
  terms_log <- setdiff(
      relevant_steps$log_skewness$terms,
      relevant_steps$log_skewness_undo$terms
    )
  terms_norm <- unique(relevant_steps$normalize$terms)
  
  
  if (length(terms_log) > 0) {
    params_back$log <- relevant_steps$log_skewness %>% 
      dplyr::select(terms, base) %>% 
      dplyr::filter(terms %in% terms_log)
  }
  
   if (length(terms_norm) > 0) {
    params_back$norm <- relevant_steps$normalize %>% 
      dplyr::select(terms, statistic, value) %>% 
      tidyr::pivot_wider(names_from = statistic)
   }
  
  # exit due to no variable selected for resp trafo
  if (length(params) == 0) return(NULL)
  
  trafo_params <- params_back %>%
    purrr::reduce(dplyr::full_join) %>% 
    {if (length(terms_norm) == 0) {
      dplyr::mutate(., sd = NA_real_, mean = NA_real_)
    } else{.}} %>% 
    {if (length(terms_log) == 0) {
      dplyr::mutate(., base = NA_real_)
    } else{.}}
  
  trafo_params
  
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