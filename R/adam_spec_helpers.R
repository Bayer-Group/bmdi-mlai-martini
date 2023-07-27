#' Title
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' the dictionary
#'
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' TODO complete docu

create_dict <- function(spec_entry){
  
  # input checks
  
  if(is.null(spec_entry$data)){
    cli::cli_abort( c(
       'i' = 'Dictionary creation requires data to be attached to the spec object.',
       'x' = 'No data is attached.',
       '*' = 'Rerun {.fn adam_spec} with {.code attach_data = TRUE}.'
    ))
    
  }
  # not exported; no extra checks on spec_entry's list structure and entries
  
  
  dict <- with(spec_entry, {
    
    if(type == "adsl"){
      
      labelled::var_label(data)  %>%
        # fix labels (no label = empty string (already done in spec creation, just to be safe))
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
          # determine combinations of only param, label and unit to handle randomly missing unit entries
          # same unit should be filled across timepoints
          c("param" = param, "label" = label, unit) %>% na.omit() 
        )) %>% 
        {if (!is.null(unit)){
          dplyr::group_by(., dplyr::across(-tidyselect::any_of(c("param", "label")))) %>% 
            tidyr::fill(tidyselect::any_of(unit), .direction = "downup") %>% 
            dplyr::ungroup() %>% 
            dplyr::rename(tidyselect::any_of(c("unit" = unit)))
        } else {
          .
        }} %>% 
        dplyr::distinct() %>%
        dplyr::mutate(source = spec_id) %>% 
        dplyr::mutate(type   = type) 
      
      # for consistent dict structure: add NA columns for time and/or unit if missing
      if(is.null(unit)) dict <- dict %>% dplyr::mutate(unit = NA_character_)
      
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
  # TODO write test 'create_dict'
}



#' data info on spec id
#' 
#' info on size of extracted data in terms of numbers of subjects and columns (roughly) 
#'
#' @param spec_entry Top level entry of an object of class `martini_spec`.
#'
#' @return
#' list with entries `nsubj` and `ncol` giving
#' the number of distinct values in the .id column and the (approximate)
#' number of columns derived from the data set, respectively.
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)

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


cor_quiet <- purrr::quietly(stats::cor)

