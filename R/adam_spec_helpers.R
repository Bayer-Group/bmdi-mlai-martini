#' Title
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#'
#'
#' TODO complete docu

create_dict <- function(spec_entry){
  
  # input checks
  if(is.null(spec_entry$data)){
    usethis::ui_stop("No data attached. Dictionary can't be created.")
  }
  # not exported; no extra checks on spec_entry's list structure and entries
  
  
  dict <- with(spec_entry, {
    
    if(type == "adsl"){
      
      labelled::var_label(data)  %>%
        # fix labels (no label = empty string (already done in spec creation, just to be safe)
        purrr::imap(~{
          if(is.null(.x)){.y}else{.x}}
        ) %>% 
        unlist() %>% 
        tibble::enframe(name = 'param', value = 'label') %>% 
        dplyr::mutate(source = spec_id) %>% 
        dplyr::mutate(type = type) %>% 
        dplyr::mutate(selected = param %in% select)
      
    }else if(type == "bds"){
      
      dict  <- data %>% 
        dplyr::select( tidyselect::any_of(
          c(param, label, unit) %>% na.omit() 
        )) %>% 
        {if (!is.na(unit)){
          dplyr::group_by(., dplyr::across(-unit)) %>% 
            tidyr::fill(unit, .direction = "downup") %>% 
            dplyr::ungroup()
        } else {
          .
        }} %>% 
        dplyr::distinct() %>%
        dplyr::mutate(source = spec_id) %>% 
        dplyr::mutate(type   = type) 
      
      param_sel <- data %>% 
        {if(length(filter) > 0){       
          dplyr::filter(., !!! rlang::parse_exprs(filter))
        }else{.}
        } %>% 
        dplyr::pull(param) %>% 
        unique()
      
      dict %>% dplyr::mutate(selected = param %in% param_sel)
      
    }else if(type == 'occds'){
      
      dict  <- data %>% 
        dplyr::select(label = !! rlang::sym(label)) %>% 
        dplyr::distinct() %>%
        dplyr::mutate(source = spec_id) %>% 
        dplyr::mutate(type   = type) 
      
      label_sel <- data %>% 
        {if(length(filter) > 0){       
          dplyr::filter(., !!! rlang::parse_exprs(filter))
        }else{.}
        } %>% 
        dplyr::pull(!! rlang::sym(label)) %>% 
        unique()
      
      dict %>% dplyr::mutate(selected = label %in% label_sel)
      
    }
    
  })
  
  # remove data set label automatically created by haven::read_sas()
  attr(dict, 'label') <- NULL
  
  dict
}



#' data info on spec id
#' 
#' info on size of extracted data in terms of numbers of subjects and columns (roughly) 
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' 
#' @examples
#' data_info(martini_spec$adsl)

# TODO complete docu

data_info <- function(spec_entry){
  
  # TODO rewrite for full spec and map over ids to return 
  #updated version of spec where data_info slots of ids are updated
  
  #spec_entry[["data_info"]] <-
  with(spec_entry, {
    
    out <- list(
      nsubj = NA_integer_,
      ncol  = NA_integer_
    )
    
    if(!is.null(data)){ 
      
      out$nsubj <- data %>% 
        {if(length(filter) > 0){ 
          dplyr::filter(., !!! rlang::parse_exprs(filter))
        }else{.}} %>% 
        dplyr::select(tidyselect::all_of(id)) %>% 
        dplyr::n_distinct()
      
    }
    
    if(!is.null(dict)){
      # ncol is based on dictionary, so it works with and w/o data attached
      out$ncol <- dict %>% 
        {if(type == "adsl"){
          dplyr::filter(., selected)
        }else{.}} %>% 
        nrow()
      
    }
    
    out
  })
  
}


with(spec_entry, {
  data %>% 
    dplyr::select( tidyselect::any_of(
      c(param, label, unit) %>% na.omit() 
    )) %>% 
    {if (!is.na(unit)){
      dplyr::group_by(., dplyr::across(-unit)) %>% 
        tidyr::fill(unit, .direction = "downup") %>% 
        dplyr::ungroup()
    } else {
      .
    }} %>% 
    dplyr::distinct() %>%
    dplyr::mutate(source = spec_id) %>% 
    dplyr::mutate(type   = type) %>%  
    print()
})
