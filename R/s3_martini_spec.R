#' @exportS3Method 

print.martini_spec <- function(x){
  
  txt_print <- c(
    "\n",
    crayon::silver("  Content"),
    "\n"
  )
  
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
    paste0('  ', ., '\n')
  
  txt_sum[1] <- crayon::bold(txt_sum[1]) %>% crayon::blue()
  
  txt_print <- c(txt_print, txt_sum)
  
  # key columns ####
  
  types     <- purrr::map_chr(x, "type") 
  
  # ... bds ####
  
  names_bds <- names(types)[which(types == 'bds')] %>% rlang::set_names()
  
  if(any( purrr::map_chr(x, "type")== "bds")){

    bds_keys  <- c("param", "value", "unit", "time")
    
    txt_bds <- c("name", bds_keys) %>% 
      rlang::set_names() %>% 
      tibble::as_tibble_row() %>% 
      dplyr::bind_rows(
        purrr::imap_dfr(names_bds, ~{c(name = .y, x[[.x]][bds_keys])})
      ) %>% 
      dplyr::mutate_all(
        ~crayon::col_align(.x, align = 'left' , width = max(nchar(.x), na.rm = TRUE))
      ) %>% 
      tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
      dplyr::pull(x) %>% 
      paste0('  ', ., '\n')
    
    txt_bds[1] <- crayon::bold(txt_bds[1]) %>% crayon::blue()
    
    txt_bds <- c(
      "\n",
      crayon::silver("  Key columns used in bds-type data sets"),
      "\n",
      txt_bds
    )
    
    txt_print <- c(txt_print, txt_bds)
    
  }
  
  # ... occds ####
  
  names_occds <- names(types)[which(types == 'occds')] %>% rlang::set_names()
  
  if(any( purrr::map_chr(x, "type")== "occds")){
    
    occds_keys  <- c("label", "value", "valuen", "count", "time")
    
    txt_occds <- c("name", occds_keys) %>% 
      rlang::set_names() %>% 
      tibble::as_tibble_row() %>% 
      dplyr::bind_rows(
        purrr::imap_dfr(names_occds, ~{c(name = .y, x[[.x]][occds_keys] %>% unlist())})
      ) %>% 
      dplyr::mutate_all(
        ~crayon::col_align(.x, align = 'left' , width = max(nchar(.x), na.rm = TRUE))
      ) %>% 
      tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
      dplyr::pull(x) %>% 
      paste0('  ', ., '\n')
    
    txt_occds[1] <- crayon::bold(txt_occds[1]) %>% crayon::blue()
    
    txt_occds <- c(
      "\n",
      crayon::silver("  Key columns used in occds-type data sets"),
      "\n",
      txt_occds
    )
    
    txt_print <- c(txt_print, txt_occds)
    
  }
  
  purrr::walk(txt_print, cat)
  
}