

d_ml_split <- list(d_ml, d_ml) %>% 
  set_names(d_ml$data_raw$train$.trt %>% levels()) %>% 
  imap(~{
    
    # remove '.trt' from dictionary
    .x$dict <- .x$dict %>% filter(column != ".trt")
    
    .x$data_raw$train <- .x$data_raw$train %>% dplyr::filter(.trt == .y)
    .x$data_raw$test  <- .x$data_raw$test  %>% dplyr::filter(.trt == .y)
    
    .x$split$data <- .x$split$data %>% 
      filter(.trt ==.y) %>% 
      select(-any_of(c("extended_strata", ".trt")))
    
    .x$split$in_id  <- which( .x$split$data$.id %in% .x$data_raw$train$.id)
    
    .x$prep_recipe <- .x$prep_recipe %>% 
      step_rm(any_of(".trt"), trained = TRUE, removals = ".trt")
    
    .x$data_prep$train <- recipes::bake(.x$prep_recipe, new_data = .x$data_raw$train)
    .x$data_prep$test  <- recipes::bake(.x$prep_recipe, new_data = .x$data_raw$test )
    
    .x
    
  })