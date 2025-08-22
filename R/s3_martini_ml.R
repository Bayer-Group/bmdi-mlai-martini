#' @exportS3Method 

print.martini_ml <- function(x, ...){
  
  # class ####
  cli::cli_text(
    #"\n",
    cli::col_silver("# Object of class 'martini_ml' with {x$outcome$mode} outcome {x$outcome$name}"),
    "\n\n"
  ) %>% cat()
  
  # data set sizes ####
  tab_dim <- purrr::map(
    x$data %>% purrr::set_names(~{paste0("data$", .)}), 
    \(rec_state) purrr::map(rec_state, pillar::dim_desc) %>% tibble::as_tibble_row() # 
  ) %>%
    purrr::list_rbind(names_to = "recipe_state") %>% 
    # format to char vector by row
    dplyr::add_row(
      tibble::as_tibble_row(
        names(.) %>% 
          purrr::set_names() %>% 
          #paste0("$", .) %>% 
          gsub("recipe_state", " ", .) %>% 
          gsub("^t", "$t", .) 
          #paste0("$", .)
        ),
      .) %>% 
    dplyr::mutate(recipe_state = pillar::align(recipe_state)) %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("train", "test")),
        ~as.character(.x) %>% pillar::align(align = "right")
      )) %>%
    tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
    dplyr::pull() %>% 
    cli::cat_line() %>% 
    c(cli::cli_h3("Data set sizes (n {cli::symbol$times} p)"), .)
  
  # main recipe steps ####
  if (!is.null(x$input$args$custom_recipe)) {
    c("A custom recipe was provided.") %>% 
    cli::cat_line() %>% 
      c(cli::cli_h3("Recipe"), .)
    
  } else {
  steps_to_report <- c(
    "filter_missing", 
    "log_skewness", 
    "impute_knn", 
    "nzv", 
    "normalize",
    "corr_keep", 
    "other2", 
    "dummy"
    ) 
  
  # versions with cli_symbols: checkbox on off / 
   steps_to_report %in% 
    recipes::tidy(x$recipe$prep)$type %>% 
    purrr::set_names(steps_to_report) %>% 
    tibble::enframe() %>% 
    # for log report TRUE iff data was log transformed and NOT back transformed,
    # (log_skewness && ! log_skewness_undo)
    # i.e. iff prep_step_log
    dplyr::mutate(value = replace(value, name == "log_skewness", x$input$args$prep_step_log)) %>%
    # dplyr::mutate(symbol = dplyr::if_else(value, cli::symbol$checkbox_on, cli::symbol$checkbox_off)) %>% 
    # dplyr::mutate(symbol = dplyr::if_else(value, cli::symbol$square_small_filled, cli::symbol$square_small)) %>% 
    # dplyr::mutate(symbol = dplyr::if_else(value, cli::symbol$tick, cli::symbol$cross)) %>% 
    dplyr::filter(value) %>% 
    dplyr::mutate(symbol = dplyr::if_else(value, cli::symbol$arrow_right, cli::symbol$stop)) %>% 
    dplyr::select(symbol, name) %>%
    tidyr::unite(col, tidyselect::everything(), sep = " ") %>% 
    dplyr::pull() %>%  
    cli::cat_line() %>% 
    c(cli::cli_h3("(parametrized) Recipe Steps"), .)
  }
  
}
  
