#' @title one stop shop for building the data set
#' Allows to 
#' {\itemize 
#'   \item build from spec (with or without data already attached)
#'   \item build from ads path (spec is created and used to build)
#'   \item create spec only (also available from \code{adam_spec()})}
#' 
#' @param spec 
#' @param path the path to the ads files
#' @param spec_only if build from path, don't apply the just created spec to data set
#' @param filter a character vector of conditions to be passed to \code{dplyr::filter()}, e.g. regarding visits, treatment arms or parameters. Defaults to NULL.
#' @param keep character vector defining the subset of data sets in the given `path` to create the specification for (e.g. \code{c('adsl', 'advs'))}).
#'  If both \code{keep} and \code{drop} are specified, \code{keep} overrides \code{drop}. Defaults to NULL.
#' @param drop character vector defining a subset of data sets in the given `path` to be excluded from the list of specifications (e.g. \code{'adqseq5d')}). Defaults to NULL.
#' @param join either function to join data sets (e.g. \code{dplyr::full_join} or a character (vector) giving the names of the data sets containing the .ids to keep (e.g. \code{join= c('adxb', 'adlb')}). defaults to \code{dplyr::inner_join}
#' @param attach_data boolean. attach the imported raw data
#' 
#' @description  \code{adam_spec()} matches file names in the given path against an internal library to decide on which \code{adam_*_spec()} function to use for which data set.
#'  Only files in the library will be processed, the rest will be ignored. Names of unprocessed files will be printed to the console.
#'  For those, specifications may be created manually using the appropriate \code{adam_spec_*()} function and appended to the specification list created by \code{adam_*_spec()}. 
#'
#' Individual filters are only applied if the resulting data set has a positive number of rows (ignoring those causing errors or yielding a 0-row data set). 
#'
#' Please refer to the documentations of the \code{adam_spec_*()} functions for full details.
#'
#' @return  \code{adam_spec()} returns named list of specifications that can be passed to the \code{adam_prep()} function. 
#'         Each element contains the specification for a single data set and is named with the domain abbreviation (e.g. adsl, adqskccq).
#'         The list can be manually adjusted if required, e.g. adding further specifications or altering existing ones.
#' 
#' @seealso \code{\link{adam_spec_adsl}()}, \code{\link{adam_spec_bds}()}, \code{\link{adam_spec_occds}()}
#'

build <- function(
  spec        = NULL, 
  path        = NULL, 
  spec_only   = FALSE, 
  filter      = NULL,
  keep        = NULL,
  drop        = NULL ,
  join        = dplyr::inner_join,
  attach_data = FALSE
){
  
  
  # initial check    ####
  if ( is.null(spec) && is.null(path)) usethis::ui_oops("Either 'spec' or 'path' needs to be provided!")
  if (!is.null(spec) && spec_only) {
    spec_only <- FALSE
    cat('\n')
    usethis::ui_info("`spec_only = TRUE` is ignored since spec is already provided.\n")
  }
  
  from_spec <- is.null(path)
  from_path <- ! from_spec
  
  # create specs ####   
  if (from_path){
    
      file_info <- adam_domain_type(path, keep, drop)
       
      interim <- list()
      
      # ... adsl spec ####
      
      if ( any(file_info$type == "adsl") ){
        
        files_adsl <- file_info %>% 
          dplyr::filter(type == "adsl") %>% 
          dplyr::select(dom, file) %>% 
          tibble::deframe()
        
        interim <- interim %>% 
          append(
            purrr::map(files_adsl,
                ~ adam_spec_adsl(file = .x, filter = filter, attach_data = attach_data) %>% 
                  {if(! spec_only){
                    build_adsl(.)
                  } else {.} }
                )
          )
        
      }
      
      # ... bds spec ####
      
      if ( any(file_info$type == "bds") ){
        
        files_bds <- file_info %>% 
          dplyr::filter(type == "bds") %>% 
          dplyr::select(dom, file) %>% 
          tibble::deframe()
        
        interim <- interim %>% 
          append(
            purrr::map(files_bds, 
                ~adam_spec_bds(file = .x, filter = filter, attach_data = attach_data)%>% 
                  {if(! spec_only){
                    build_bds(.)
                  } else {.} }
                )
          )
        
      }
      
      # ... occds spec ####
      if ( any(file_info$type == "occds") ){
        
        files_occds <- file_info %>% 
          dplyr::filter(type == "occds") %>% 
          dplyr::select(dom, file) %>% 
          tibble::deframe()
        
        interim <- interim %>% 
          append(
            purrr::map(files_occds, 
                       ~adam_spec_occds(file = .x, filter = filter, attach_data = attach_data)%>% 
                         {if(! spec_only){
                           build_occds(.)
                         } else {.} }
            )
          )
        
      }
      

   # end if(from_path)   
  }else{  # from_spec; spec is provided 
    
    # no names at all (names(spec) is null)
    if( is.null(names(spec)) )  names(spec) <- rep('', length(spec))
    
    for (i in 1:length(spec)){
      if(is.null(spec[[i]]$"spec_id"))   spec[[i]]$"spec_id" <- names(spec)[i]
    }
    
    interim <- purrr::map(spec, 
            ~  { do.call( paste0('build_',   .x[['type']]), list(.x)) }
        )
  }
  
 
  
  # create output object ####
  
  if(spec_only){
    
    out <- interim
    
  }else{
    
    # ... dict  #### 
    prepped_dict <- purrr::map(interim, ~.[['dict']]) %>% 
      purrr::reduce(dplyr::bind_rows)
    
    # ... source  #### 
    prepped_source <- purrr::map(interim, ~{
      .x[["source"]] %>% 
        tibble::as_tibble_row()
      }) %>% 
      purrr::reduce(dplyr::bind_rows) 
    
     
    
    # ... data ####
    # identify subjects from selected data sets to filter prepped_join
    # (if join is not a fct)
    if(! is.function(join)){
      if(any(join %in% names(interim) )) {
        join_ids <- purrr::map(interim[join %>%  intersect(names(interim))], ~.[['data']]) %>% 
          purrr::reduce(dplyr::full_join, by = '.id') %>% 
          pull(.id)
        join_filter <- ' .id %in% join_ids'
      }else{
        join_filter <- 'TRUE'
        usethis::ui_info("The domain(s) specified for 'join' are not available in the given spec. dplyr::full_join was used without additional filters.\n")
      }  
    }
    
   
    # combine and filter
    prepped_join <- purrr::map(interim, ~.[['data']]) %>% 
      {if(is.function(join)){
        purrr::reduce(., join, by = '.id') 
      }else{
        purrr::reduce(., dplyr::full_join, by = '.id') %>% 
          dplyr::filter(!! rlang::parse_expr(join_filter))  
      }}
     
    # extract all occds columns for explicit factor na
    # missing values occuring from occurence data mean 'absence of event', whereas NAs in bds data are true missing values
    # -> replace missings by 0 for numerics, level 'none' for factors
    vars_fct_expl_na <- prepped_dict %>% 
      dplyr::filter(type == 'occds') %>% 
      dplyr::pull(column)
     
    prepped_join <- prepped_join %>%  
       dplyr::mutate_at(vars(tidyselect::any_of(vars_fct_expl_na)), ~{
         if(is.numeric(.x)){
           tidyr::replace_na(.x, replace = 0L )
         }else{
           forcats::fct_explicit_na(.x, na_level = 'none') %>% 
             forcats::fct_shift(n = -1)
         }  
       })
  
  
    out                 <- prepped_join
    attr(out, "dict")   <- prepped_dict
    attr(out, "source") <- prepped_source
  }
  
  out
  
}


if(FALSE){
  
  path = 'real_world_data/99999/'
  # path   = '//by-xa221/Statdb/Ginger/Studies/BAY106-7197_Neladenosone_99999_PANTHEON/Data/Original/ads/'
  filter = c("SEX == 'F'", "AVISIT == 'BASELINE'")
  
  spec = ads_spec 
  spec_only = FALSE
  filter = NULL
  keep   = NULL
  drop   = NULL 
  attach_data = FALSE
  
}




