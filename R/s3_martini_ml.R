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
    x$data, 
    \(rec_state) purrr::map(rec_state, pillar::dim_desc) %>% tibble::as_tibble_row() # 
  ) %>%
    purrr::list_rbind(names_to = "recipe_state") %>% 
    # format to char vector by row
    dplyr::add_row(
      tibble::as_tibble_row(
        names(.) %>% purrr::set_names() %>% gsub("_", " ", .)),
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
  
 # cat(tab_nrow)
  
  
}
  
#print_ml_nrow <- 
  
#   txt_sum <- c("name", "type", "size", "nsubj", "ncol") %>% 
#     rlang::set_names() %>% 
#     tibble::as_tibble_row() %>% 
#     dplyr::bind_rows(
#       purrr::imap_dfr(x, ~{
#         
#         tibble::tibble(
#           name  = .y, 
#           type  = .x$type,  
#           size  = ifelse(
#             is.null(.x$size), 
#             NA_character_, 
#             .x$size %>% fs::as_fs_bytes() %>% as.character()),
#           nsubj = as.character(.x$data_info$nsubj),
#           ncol  = as.character(.x$data_info$ncol)
#         )
#       })
#     ) %>% 
#     dplyr::mutate_at(
#       c('name', 'type'), 
#       ~crayon::col_align(.x, align = 'left' , width = max(nchar(.x), na.rm = TRUE))
#     ) %>% 
#     dplyr::mutate_at(
#       c('size', 'nsubj', 'ncol'),
#       ~crayon::col_align(.x, align = 'right', width = max(nchar(.x), na.rm = TRUE))
#     ) %>% 
#     
#     tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
#     dplyr::pull(x) %>% 
#     paste0('  ', ., '\n')
#   
#   txt_sum[1] <- crayon::bold(txt_sum[1]) %>% crayon::blue()
#   
#   txt_print <- c(txt_print, txt_sum)
#   
#   # key columns ####
#   
#   types     <- purrr::map_chr(x, "type") 
#   
#   # ... bds ####
#   
#   names_bds <- names(types)[which(types == 'bds')] %>% rlang::set_names()
#   
#   if (any( purrr::map_chr(x, "type") == "bds")) {
# 
#     bds_keys  <- c("param", "value", "unit", "time")
#     
#     txt_bds <- c("name", bds_keys) %>% 
#       rlang::set_names() %>% 
#       tibble::as_tibble_row() %>% 
#       dplyr::bind_rows(
#         purrr::imap_dfr(names_bds, ~{c(name = .y, x[[.x]][bds_keys])})
#       ) %>% 
#       dplyr::mutate_all(
#         ~crayon::col_align(.x, align = 'left' , width = max(nchar(.x), na.rm = TRUE))
#       ) %>% 
#       tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
#       dplyr::pull(x) %>% 
#       paste0('  ', ., '\n')
#     
#     txt_bds[1] <- crayon::bold(txt_bds[1]) %>% crayon::blue()
#     
#     txt_bds <- c(
#       "\n",
#       crayon::silver("  Key columns used in bds-type data sets"),
#       "\n",
#       txt_bds
#     )
#     
#     txt_print <- c(txt_print, txt_bds)
#     
#   }
#   
#   # ... occds ####
#   
#   names_occds <- names(types)[which(types == 'occds')] %>% rlang::set_names()
#   
#   if (any(purrr::map_chr(x, "type") == "occds")) {
#     
#     occds_keys  <- c("label", "value", "valuen", "count", "time")
#     
#     txt_occds <- c("name", occds_keys) %>% 
#       rlang::set_names() %>% 
#       tibble::as_tibble_row() %>% 
#       dplyr::bind_rows(
#         purrr::imap_dfr(names_occds, ~{c(name = .y, x[[.x]][occds_keys] %>% unlist())})
#       ) %>% 
#       dplyr::mutate_all(
#         ~crayon::col_align(.x, align = 'left', width = max(nchar(.x), na.rm = TRUE))
#       ) %>% 
#       tidyr::unite(col = "x", tidyselect::everything(), sep = " ") %>% 
#       dplyr::pull(x) %>% 
#       paste0('  ', ., '\n')
#     
#     txt_occds[1] <- crayon::bold(txt_occds[1]) %>% crayon::blue()
#     
#     txt_occds <- c(
#       "\n",
#       crayon::silver("  Key columns used in occds-type data sets"),
#       "\n",
#       txt_occds
#     )
#     
#     txt_print <- c(txt_print, txt_occds)
#     
#   }
#   
#   all_data_info_ok <- all(purrr::map_lgl(x, ~attr(.x, 'data_info_ok')))
#   all_filter_ok    <- all(purrr::map_lgl(x, ~attr(.x, 'filter_ok')))
#   
#   if (!all_data_info_ok | !all_filter_ok) {
#     not_ok <- ''
#     if (!all_data_info_ok) not_ok <- c(not_ok, 'content')
#     if (!all_filter_ok)    not_ok <- c(not_ok, 'filter')
#     
#     cat(usethis::ui_warn(crayon::bgMagenta(crayon::white(
#       paste0(
#         ifelse(!is.null(data_id), paste0(data_id, ': '), ''), 
#         'The ', 
#         paste(not_ok, collapse = ' and '),
#         ' info shown might be outdated due to adjustments to the spec object.', 
#         'Re-run adam_spec() with data attached to enable updating. \n\n'
#       )
#     ))))
#   }
#   purrr::walk(txt_print, cat)
#   
#   # combine original filter set used to build the spec with potentially 
#   # added filters during spec adjustment (not recommended)
#   all_filters <- c(
#     # filter argument passed from adam_spec() call
#     attr(x, 'filter', exact = TRUE), 
#     # actual filters
#     purrr::map( x, 'filter') %>% unlist() 
#     ) %>% unique()
#   res_info    <- info_filter(x, all_filters, quiet = TRUE)
# 
#   if (res_info %>% purrr::map_lgl(~!is.null(.x)) %>% any()) {
#     cat(crayon::silver("\n  Filter information \n"))
#     
#     info_filter(x, attr(x, 'filter', exact = TRUE))
#   }  
#   
# }