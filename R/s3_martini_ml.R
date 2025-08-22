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
    \(rec_state) purrr::map(rec_state, pillar::dim_desc) %>% tibble::as_tibble_row() 
  ) %>%
    purrr::list_rbind(names_to = "recipe_state") %>% 
    # format to char vector by row
    dplyr::add_row(
      tibble::as_tibble_row(
        names(.) %>% 
          purrr::set_names() %>% 
          gsub("recipe_state", " ", .) %>% 
          gsub("^t", "$t", .) 
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
    c(cli::cli_h3("Data set sizes (n {pillar:::mult_sign()} p)"), .)
  
}
  
