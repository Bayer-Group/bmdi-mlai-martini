

print.martini_spec <- function(x){
  
  purrr::imap_dfr(x, ~{
    tibble::tibble(.y, .x$type, as.character(.x$size))
  }) %>% 
    purrr::map_dfr(~{
      stringr::str_pad(.x, width = max(nchar(.x)), side = "right")
    }) %>% 
    tidyr::unite(col = "x", everything(), sep = " ") %>% 
    pull(x) %>% 
    paste(collapse = "\n") %>% 
    cat()
  
}