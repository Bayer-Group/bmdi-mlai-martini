#' @exportS3Method 

print.martini_spec <- function(x){
  
  txt_sum <- c("name", "type", "size", "ncol") %>% 
    rlang::set_names() %>% 
    tibble::as_tibble_row() %>% 
    dplyr::bind_rows(
      purrr::imap_dfr(x, ~{
        
        # determine number of resulting COL number in wide data set by number of (selected) ROWS from dict
        ncol <- .x$dict %>%  
          {if('selected' %in% names(.)){
            dplyr::filter(., selected)
          }else{.} 
          } %>% 
          nrow()
        
        tibble::tibble(
          name = .y, 
          type = .x$type,  
          size = ifelse(
            is.null(.x$size), 
            NA_character_, 
            .x$size %>% fs::as_fs_bytes() %>% as.character()),
          ncol = as.character(ncol)
        )
      })
    ) %>% 
    dplyr::mutate_at(
      c('name', 'type'), 
      ~crayon::col_align(.x, align = 'left' , width = max(nchar(.x), na.rm = TRUE))
    ) %>% 
    dplyr::mutate_at(
      c('size', 'ncol'),
      ~crayon::col_align(.x, align = 'right', width = max(nchar(.x), na.rm = TRUE))
    ) %>% 
    
    tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
    dplyr::pull(x) %>% 
    paste('  ', ., '\n')
  
  txt_sum[1] <- crayon::bold(txt_sum[1]) %>% crayon::blue()
  
  purrr::walk(txt_sum, cat)
  
}