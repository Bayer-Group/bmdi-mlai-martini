#' @exportS3Method 

print.martini_spec <- function(x, ...){
  
  txt_print <- c(
    "\n",
    cli::col_silver("  Content"),
    "\n"
  )
  
  txt_sum <- c("name", "type", "size", "nsubj", "ncol") %>% 
    rlang::set_names() %>% 
    tibble::as_tibble_row() %>% 
    dplyr::bind_rows(
      purrr::imap_dfr(x, ~{
        
        tibble::tibble(
          name  = .y, 
          type  = .x$type,  
          size  = ifelse(
            is.null(.x$size), 
            NA_character_, 
            .x$size %>% fs::as_fs_bytes() %>% as.character()),
          nsubj = as.character(.x$data_info$nsubj),
          ncol  = as.character(.x$data_info$ncol)
        )
      })
    ) %>% 
    dplyr::mutate_at(c('name', 'type'), pillar::align) %>% 
    dplyr::mutate_at(
      c('size', 'nsubj', 'ncol'),
      ~pillar::align(.x, align = "right")
    ) %>% 
    
    tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
    dplyr::pull(x) %>% 
    paste0('  ', ., '\n')
  
  txt_sum[1] <- cli::style_bold(txt_sum[1]) %>% cli::col_blue()
  
  txt_print <- c(txt_print, txt_sum)
  
  # key columns ####
  
  types     <- purrr::map_chr(x, "type") 
  
  # ... bds ####
  
  names_bds <- names(types)[which(types == 'bds')] %>% rlang::set_names()
  
  if (any( purrr::map_chr(x, "type") == "bds")) {

    bds_keys  <- c("param", "value", "unit", "time")
    
    txt_bds <- c("name", bds_keys) %>% 
      rlang::set_names() %>% 
      tibble::as_tibble_row() %>% 
      dplyr::bind_rows(
        purrr::imap_dfr(names_bds, ~{c(name = .y, x[[.x]][bds_keys])})
      ) %>% 
      dplyr::mutate_all(pillar::align) %>% 
      tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
      dplyr::pull(x) %>% 
      paste0('  ', ., '\n')
    
    txt_bds[1] <- cli::style_bold(txt_bds[1]) %>% cli::col_blue()
    
    txt_bds <- c(
      "\n",
      cli::col_silver("  Key columns used in bds-type data sets"),
      "\n",
      txt_bds
    )
    
    txt_print <- c(txt_print, txt_bds)
    
  }
  
  # ... occds ####
  
  names_occds <- names(types)[which(types == 'occds')] %>% rlang::set_names()
  
  if (any(purrr::map_chr(x, "type") == "occds")) {
    
    occds_keys  <- c("label", "value", "valuen", "count", "time")
    
    txt_occds <- c("name", occds_keys) %>% 
      rlang::set_names() %>% 
      tibble::as_tibble_row() %>% 
      dplyr::bind_rows(
        purrr::imap_dfr(names_occds, ~{c(name = .y, x[[.x]][occds_keys] %>% unlist())})
      ) %>% 
      dplyr::mutate_all(pillar::align) %>% 
      tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
      dplyr::pull(x) %>% 
      paste0('  ', ., '\n')
    
    txt_occds[1] <- cli::style_bold(txt_occds[1]) %>% cli::col_blue()
    
    txt_occds <- c(
      "\n",
      cli::col_silver("  Key columns used in occds-type data sets"),
      "\n",
      txt_occds
    )
    
    txt_print <- c(txt_print, txt_occds)
    
  }
  
  all_data_info_ok <- all(purrr::map_lgl(x, ~attr(.x, 'data_info_ok')))
  all_filter_ok    <- all(purrr::map_lgl(x, ~attr(.x, 'filter_ok')))
  
  if (!all_data_info_ok | !all_filter_ok) {
    not_ok <- ''
    if (!all_data_info_ok) not_ok <- c(not_ok, 'content')
    if (!all_filter_ok)    not_ok <- c(not_ok, 'filter')
    
    cat(usethis::ui_warn(cli::bg_magenta(cli::col_white(
      paste0(
        ifelse(!is.null(data_id), paste0(data_id, ': '), ''), 
        'The ', 
        paste(not_ok, collapse = ' and '),
        ' info shown might be outdated due to adjustments to the spec object.', 
        'Re-run adam_spec() with data attached to enable updating. \n\n'
      )
    ))))
  }
  purrr::walk(txt_print, cat)
  
  # combine original filter set used to build the spec with potentially 
  # added filters during spec adjustment (not recommended)
  all_filters <- c(
    # filter argument passed from adam_spec() call
    attr(x, 'filter', exact = TRUE), 
    # actual filters
    purrr::map( x, 'filter') %>% unlist() 
    ) %>% unique()
  res_info    <- info_filter(x, all_filters, quiet = TRUE)

  if (res_info %>% purrr::map_lgl(~!is.null(.x)) %>% any()) {
    cat(cli::col_silver("\n  Filter information \n"))
    
    info_filter(x, attr(x, 'filter', exact = TRUE))
  }  
  
}