#' One stop shop for building the machine learning data set
#' 
#' The build() function allows to build a machine learning data set from a specification object as provided
#' by \code{\link{adam_spec}()} (with or without data already attached). It can also be used to build the data directly from an ads
#' path. In this case the specification object is created internally by calling the respective `adam_spec_*()` functions 
#' and immediately used to build the data set.
#' 
#' @param spec a specification object as provided by \code{\link{adam_spec}()} (either \code{spec} or \code{path} has to be provided)
#' @param path the path to the ads files (either \code{spec} or \code{path} has to be provided)
#' @param spec_only if build from path, don't apply the just created spec to data set
#' @param join either function to join data sets (e.g. \code{dplyr::full_join} or a character (vector) giving the names
#' of the data sets containing the .ids to keep (e.g. \code{join = c('adxb', 'adlb')}). defaults to \code{dplyr::inner_join}
#' @param filter a character vector of conditions to be passed to \code{dplyr::filter()},
#' e.g. regarding visits, treatment arms or parameters. Defaults to NULL. Only applied, if \code{spec} is not provided.
#' @param keep character vector defining the subset of data sets in the given `path` to create
#' the specification for (e.g. \code{c('adsl', 'advs'))}). If both \code{keep} and \code{drop} are specified,
#' \code{keep} overrides \code{drop}. Defaults to NULL. Only applied, if \code{spec} is not provided.
#' @param drop character vector defining a subset of data sets in the given `path` to
#' be excluded from the list of specifications (e.g. \code{'adqseq5d')}). Defaults to NULL.
#' Only applied, if \code{spec} is not provided.
#' @param attach_data boolean. Attach the imported raw data. Only applied, if \code{spec_only = TRUE}.
#' 
#'
#' @return
#' 
#' \code{build()} returns a wide data set with one row per subject and standardized column names for the subject id (`.id`)
#' and the treatment variable (`.trt`), if it is provided in the \code{spec} object. Objects with additional information on
#' the data are provided in the attributes of the returned object.
#' 
#' \item{`dict`}{
#' \describe{
#'   \item{`param`}{original parameter name in the source data}
#'   \item{`column`}{column name of the variable in the returned data. `column` is derived from `param` by transforming
#'   it into a valid file name and possibly adding a time extension, if multiple time points are considered for a particular parameter.}
#'   \item{`label`}{parameter label}
#'   \item{`source`}{source id provided by the specification object. If created with \code{\link{adam_spec}()}, this is the name of the domain.}
#'   \item{`type`}{adam data type of the source data (adsl, bds or occds)}
#'   \item{`unit`}{parameter unit (if applicable)}
#'   \item{`time`}{measurement time point (if applicable)}
#' }}
#' \item{`source`}{file path and md5 checksums of the source data sets}
#' 
#' 
#' @seealso \code{\link{build_adsl}()}, \code{\link{build_bds}()}, \code{\link{build_occds}()}
#'
#' @section Authors
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#'
#' @export

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
  
  # if PATH is provieded ####
  # ... create specs ####   
  if (from_path){
    
      file_info <- adam_domain_type(path, keep, drop)
       
      interim <- list()
      
      # ... ... type adsl ####
      
      if ( any(file_info$type == "adsl") ){
        
        files_adsl <- file_info %>% 
          dplyr::filter(type == "adsl") %>% 
          dplyr::select(domain, file) %>% 
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
      
      # ... ... type bds ####
      
      if ( any(file_info$type == "bds") ){
        
        files_bds <- file_info %>% 
          dplyr::filter(type == "bds") %>% 
          dplyr::select(domain, file) %>% 
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
      
      # ... ... type occds  ####
      if ( any(file_info$type == "occds") ){
        
        files_occds <- file_info %>% 
          dplyr::filter(type == "occds") %>% 
          dplyr::select(domain, file) %>% 
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
      
  # if SPEC is provided ####
  }else{
    
    # add names to the spec if none are provided
    if( is.null(names(spec)) )  names(spec) <- rep('', length(spec))
    
    for (i in 1:length(spec)){
      if(is.null(spec[[i]]$"spec_id"))   spec[[i]]$"spec_id" <- names(spec)[i]
    }
    
    # call the appropriate build_*() function
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
    # missing values occurring from occurrence data mean 'absence of event', whereas NAs in bds data are true missing values
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

# test area ####
if(FALSE){
  
  path <- "data/99999/ads"
  filter <- c("SEX == 'F'", "AVISIT == 'BASELINE'")
  keep <- c("adsl", "adxb")  
  wide <- build(path = path, keep = keep)

}




