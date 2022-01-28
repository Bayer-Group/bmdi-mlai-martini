#' @exportS3Method 

print.martini_spec <- function(x){
  
  txt_sum <- c("name", "type", "size") %>% 
    rlang::set_names() %>% 
    tibble::as_tibble_row() %>% 
    dplyr::bind_rows(
      purrr::imap_dfr(x, ~{
        tibble::tibble(
          name = .y, 
          type = .x$type,  
          size = ifelse(is.null(.x$size), NA_character_, as.character(.x$size))
        )
      })
    ) %>% 
    purrr::map_dfr(~{
      stringr::str_pad(.x, width = max(nchar(.x), na.rm = TRUE), side = "right")
    }) %>% 
    tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
    dplyr::pull(x) %>% 
    paste('  ', ., '\n')
  
  txt_sum[1] <- crayon::bold(txt_sum[1]) %>% crayon::blue()
  
  purrr::walk(txt_sum, cat)
  
}