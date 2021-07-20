test_that("strata_trt works", {
  
  require(tidyverse)
  # test stratification WITH and WITHOUT added treatment
  
  trt_groups <- c('PLA', 'trt1', 'trt2')
  n_total    <- 90
  
  d_feat <- tibble(
    .id  = 1:n_total,
    .trt = rep(trt_groups, length.out = n_total),
    cont = rnorm(n_total)
  )
  d_out <- tibble(
    .id  = 1:n_total,
    .out = rep(c(
      rep('no event', round(n_total/length(trt_groups))-9),
      rep('event',    9)), 
      length.out = n_total) 
  )
  d_raw <- inner_join(d_out, d_feat) %>% 
    unite(trt.out, .trt, .out, remove = FALSE)
  prop_tot_event_trt <- d_raw %>% 
    pull(trt.out) %>% 
    table %>% 
    {. / sum(.)}
  
  seed <- 1130
  train_prop <- .5
  res_out <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = FALSE,
    seed       = seed
  )$data_raw

    
  res_out_trt <- prepare_ml(
    feature    = d_feat,
    outcome    = d_out,
    train_prop = train_prop,
    strata_trt = TRUE,
    seed       = seed
  )$data_raw
  
  # distribution of trt
  prop_event <- list(
    total   = d_raw       %>%           pull(   .out) %>% {. == "event"} %>% mean() ,
    out     = res_out     %>% map_dbl(~ pull(., .out) %>% {. == "event"} %>% mean()) %>% round(2),
    out_trt = res_out_trt %>% map_dbl(~ pull(., .out) %>% {. == "event"} %>% mean()) %>% round(2) 
  )
  
  # distribution of combined stratum variable trt_out
  smmry_reshape <- function(x, set = NA_character_){
    pull(x, trt.out) %>% 
      table() %>% 
      {./sum(.)} %>%  
      round(2) %>% 
      as.data.frame.table() %>%  as_tibble() %>%  
      pivot_wider(names_from = '.', values_from = Freq) %>% 
      mutate(set = set, .before = 1)
  }
  
  prop_out_trt <- list(
    
    total   = d_raw %>% smmry_reshape(set = 'total') ,
    
    out_trt = res_out_trt %>% 
      map(~ unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape) %>% 
      bind_rows(.id = 'set') %>% 
      mutate(strata_trt = TRUE, .after = set),
    
    out     = res_out     %>% 
      map(~ unite(., trt.out, .trt, .out, remove = FALSE) %>% 
            smmry_reshape)  %>% 
      bind_rows(.id = 'set') %>% 
      mutate(strata_trt = FALSE, .after = set)
  ) %>% 
    reduce(bind_rows)
  
  # compute sum of absolute deviations WITH and WITHOUT strata_trt parameter set to TRUE
  comp_strata_trt <- prop_out_trt %>%  
    mutate_if(is.numeric, ~ abs(.x - .x[set == 'total'])) %>% 
    filter(set != 'total') %>% 
    nest(e=contains('event')) %>% 
    mutate(sum_e = map_dbl(e, sum)) %>% 
    group_by(strata_trt) %>% 
    mutate(sum_e = sum(sum_e)) %>% 
    ungroup() %>% 
    select(strata_trt, sum_e)  %>% 
    distinct() %>% 
    deframe()
  
  testthat::expect_gt(
    comp_strata_trt['FALSE'], 
    comp_strata_trt['TRUE']
  )
  
})
