#' Create specification object for adam data sets of type 'adsl'.
#' 
#' Given a file containing an adsl data set, \code{\link{adam_spec_adsl}()} will create a specification 
#' object for use in \code{\link{build_adsl}()} to actually create a subset of 
#' the data to be used in machine learning. For adsl specifically, the main task is the 
#' identification of noise and redundancies in the data and the selection of a potentially meaningful
#' set of columns (returned in \code{select}) and redundancies in the data. 
#' 
#' @param file the path of the sas file to process
#' @param id name of id (e.g. SUBJIDN, SUBJID) column to keep.
#' Highly redundant variables will not be included in the suggested set of columns returned in \code{select} (see Details).
#' @param trt column to be used as the treatment variable. All other predefined treatment variables (see Details) are added
#' to the \code{drop_list}. If NULL, all treatment variables will be added to the \code{drop_list}.
#' @param keep,drop columns to be kept/dropped, independent of the technical selection process within this function
#' @param filter character vector of filters following \code{dplyr::filter()} syntax for use in \code{\link{build_adsl}()}. 
#' Filters will be checked against the data and will only be kept if the filter would not throw an error and if the resulting
#' data set has positive number of rows. Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data
#' 
#' @details 
#' 
#' \item{id}{Non-numeric columns are recoded as numeric, based on the order in which they appear in the data (sorted by \code{id}). 
#' All columns with a perfect spearman correlation to \code{id} are considered redundant and added to the \code{drop_list}.}
#' \item{trt}{The predefined list of treatment variables is TRT01A, ARMCD, ARM, ACTARM, ACTARMCD, TRT01P, TR01PG1, TR02PG1, TR01AG1, TR02AG1.
#' No more than one of these variables will be returned in \code{select}. Note that the chosen treatment 
#' representing variable will be renamed to the standard '.trt' in \code{\link{build_adsl}()}.}
#' \item{filter}{See \code{\link{check_filter}()}}
#' 
#' 
#' @return 
#' A list containing the following 
#' \item{file, md5}{the name and md5 checksum, resp., of the file the generated spec is based up-on }
#' \item{data}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{type}{character string \code{adsl}, generally giving the type of adam data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{filter}{subset of \code{filter} that yields non-empty result when applied individually}
#' \item{select}{the suggested list of columns to select from the data set} 
#' \item{factor_levels}{a list column pairs factor/factorN to determine factor level order} 
#' \item{dict}{} 
#' \item{drop_list}{a list containing column names suggested to be dropped with the entry
#'  name identifying the rationale for the discard (user input \code{drop}, date/times columns, 
#'  numeric code for another variable, combined columns, empty and constant columns,
#'   columns with redundant information to \code{id} and \code{trt} if provided)
#' \item{id, trt}{passing unchanged input}  
#' \item{spec_id}{character string \code{adsl}, generally the name of the domain}  
#' \item{dict}{a tibble of column names and labels (if present in the data set)}  
#'  
#' 
#' @section Authors:
#' 
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 
#' @export



adam_spec_adsl <- function(
  file, 
  id          = 'SUBJID',
  trt         = 'TRT01A',
  keep        = NULL, 
  drop        = NULL,
  filter      = NULL,
  attach_data = FALSE
){

  # read adsl ####
  
  adsl <- haven::read_sas(file) %>% 
    dplyr::mutate_if(is.character, ~ dplyr::na_if(., ""))
  
  md5 <- tools::md5sum(file) %>% as.character()
  
  # check input ####
  if (!id %in% colnames(adsl)) usethis::ui_stop(paste0("Provided 'id' column ", id, " not present in the data set.\n"))
  if (!is.null(trt)){
    if (!trt %in% colnames(adsl)) usethis::ui_stop(paste0("Provided 'trt' column ", trt, " not present in the data set.\n"))
  }
  
  # create column dict (name <-> label)  ####
  dict <- labelled::var_label(adsl) %>% 
    tibble::enframe(name = 'param', value = 'label') %>%  
    dplyr::mutate(label  = purrr::map_chr(label, ~ .x[[1]])) %>% 
    dplyr::mutate(source = 'adsl') %>% 
    dplyr::mutate(type   = 'adsl')
  
  clmns <- dict$param
  labs  <- dict$label
  
  
  # define black list (column names that are always excluded) ####
  black_list <- c(
    "RANDNO",
    "ADSNAME", "STUDYID",
    "SITEID" , "SITENAM", 
    "INVID"  , "INVNAM"
  )
  
  # identify columns ####
  
  # ... identify date and time columns ####
  
  # identify date by variable type...
  date_auto <- purrr::map_lgl(adsl, assertive.types::is_date) %>% which() %>% names()
  # ...and label
  date_lab  <- purrr::map_lgl(
    labelled::var_label(adsl), 
    ~ stringr::str_detect(stringr::str_to_lower(.x), 'year|month|day|date|time')) %>% 
    which()
  
  all_dates <- c(date_auto, names(date_lab))
  
  # identify time variables
  all_times <- purrr::map_lgl(
    adsl, ~{any(class(.) %in% c("difftime", "hms", "Period", "POSIXct", "POSIXt", "Date"))}
  ) %>% which() %>% names()
  
  all_date_times <- c(all_dates, all_times) %>% unique()
  
  # transform date and time to character
  adsl <- adsl %>% 
    mutate_at(unique(c(all_dates, all_times)), as.character)
  
  # ... identify pairs of categorical/numerical columns ####
  
  # ... ... flags ####
  # naming convention   xxxFL -> xxxFN
  all_FL <- c(
    clmns %>% stringr::str_subset('FL$'),
    clmns[stringr::str_to_upper(labs %>% stringr::str_detect('\\bFLAG\\b'))]
  ) %>% 
    unique()
  
  flags <- c(all_FL, stringr::str_replace(all_FL, 'FL$', 'FN')) %>% 
    unique()
  
  # NOTE automated detection may not catch all flags
  
  
  # ... ... categoricals with numeric code ####
  
  
  # identify columns to keep...
  # columns with additional numeric code
  clmn_mod     <- paste0(clmns, 'N')
  clmn_ind_lab <- clmn_mod %in% clmns
  clmn_cat     <- clmns[clmn_ind_lab]
  # corresponding columns with numeric code (to be used for level order, then dropped)
  clmn_num     <- clmn_mod[clmn_ind_lab]
  
  # if column name of categorical already has maximum length of 8 characters, the rule above does not apply
  # analogous search by labels:
  lab_mod <- paste(labs, '(N)')
  lab_ind <- lab_mod %in% labs
  lab_cat <- labs[lab_ind]
  lab_num <- lab_mod[lab_ind]
  
  # mapping of columns to keep (labels) and columns to use for level order
  all_lab_lev <- dplyr::bind_rows(
    tibble::tibble(
      lab = clmn_cat,
      lev = clmn_num
    ),
    tibble::tibble(
      lab = dict %>%
        slice(match(lab_cat, dict %>%  pull(label))) %>%  
        pull(param),
      lev = dict %>%
        slice(match(lab_num, dict %>%  pull(label))) %>% 
        pull(param)
    )
  ) %>% 
    dplyr::distinct() 
  
  # keep list of all num codes for later use (setdiff with all numeric columns)
  all_num_codes <- c(
    all_lab_lev$lev,
    dict %>% filter(str_detect(label, "\\(N\\)$")) %>% pull(param)
  ) %>% unique()
  
  # reduce to pairs for which level order needs to be extracted
  lab_lev <- all_lab_lev  %>% 
    dplyr::filter(!lab %in% flags) %>% 
    dplyr::filter(lab != id) %>% 
    dplyr::filter(lev != id)

  # create list of factor levels
  lev_list <- list()
  for(r in 1:nrow(lab_lev)){
    lev  <- rlang::sym(lab_lev[r,] %>% dplyr::pull(lev))
    lab  <- rlang::sym(lab_lev[r,] %>% dplyr::pull(lab))
    
    levs <- adsl %>% 
      dplyr::select(lab_lev[r,] %>% as.character()) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(!! lev) %>% 
      dplyr::pull(!! lab)
    
    lev_list[[lab]] <- levs
  }
  
  # ... identify combined columns (e.g. age/sex/race) ####
  
  all_slash <- labs %>% stringr::str_subset('/' )
  ind       <- all_slash %>%  
    stringr::str_split('/') %>% 
    purrr::map_lgl( ~ { all(.x %in% labs)})
  
  all_comb <- all_slash[ind]
  all_comb_columns <- dict %>% filter(labs %in% all_comb) %>% pull(param)
  
  
  # ... identify redundants for id and trt ####
  
  # ... ... ids ####
  
  # transform all variables into numerics to enable correlation computation
  # sorting by id and using fct_inorder to assess monotonous association of id column with remaining data set
  adsl_num <- adsl %>%
    dplyr::arrange(tidyselect::all_of(id)) %>% 
    dplyr::mutate_if(~!is.numeric(.), ~{
      factor(.x) %>% 
        forcats::fct_inorder() %>% 
        as.numeric()
    }) %>% 
    janitor::remove_constant(na.rm = TRUE)
  
  # correlations with 'id'
  cors_id <- stats::cor(adsl_num, adsl_num[, id], method = "spearman", use = 'pairwise.complete.obs') %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("name") %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(value = tidyselect::all_of(id))
  
  redundant_id <- cors_id %>% 
    dplyr::filter(near(value, 1)) %>% 
    dplyr::pull(name) %>% 
    setdiff(id)

  # ... ... treatment ####
  
  # match standard treatment column names against actual data
  trt_adam <- intersect(
    c("TRT01A", "ARMCD", "ARM", "ACTARM", "ACTARMCD", "TRT01P", "TR01PG1", "TR02PG1", "TR01AG1", "TR02AG1"),
    colnames(adsl)
  )

  redundant_trt <- setdiff(trt_adam, trt)
  
  all_redundants <- c(redundant_id, redundant_trt) %>% unique()
  
  # ... all numerics ####
  # candidates for select, will be intersected with numeric codes
  all_numerics <- adsl %>% 
    dplyr::select_if(is.numeric) %>% 
    colnames() 
  
  # ... empty columns ####
  empties <- setdiff( 
    adsl %>%  colnames(),
    adsl %>%  janitor::remove_empty(which = 'cols') %>%  colnames()
  )
  
  # ... constant columns ####
  constants <- setdiff( 
    adsl %>%  colnames(),
    adsl %>%  janitor::remove_constant(na.rm = TRUE) %>% colnames()
  ) %>% 
    setdiff(empties)
  
  # collect output ####
  
  # ... drop list ####
  drop_list <- list(
    "drop"         = drop,
    "datetimes"    = all_date_times,
    "numcodes"     = all_num_codes,
    "combinations" = all_comb_columns,
    "redundancies" = all_redundants,
    "empty"        = empties,
    "constant"     = constants
  ) %>% 
    purrr::map(~setdiff(., keep))
  
  # ... selected columns ####
  select_list <- c(
    id,
    trt,
    names(lev_list),
    all_numerics,
    keep
  ) %>% 
    setdiff(drop_list %>% unlist() %>% unique())
  
  # ... check filter ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(adsl, filter)
  actual_filter <- filter[keep_filter]
  
  
  # ... dictionary ####

  dict <- dict %>%  
    dplyr::mutate(selected = ifelse(
      param %in% select_list, TRUE, FALSE
    ) )
  
  # ... output object ####  

  out <- list(
    file    = file,
    md5     = md5,
    data    = NULL,
    type    = "adsl",
    
    filter        = actual_filter,
    select        = select_list,
    factor_levels = lev_list[intersect(select_list, names(lev_list))],
    
    dict       = dict,
    #drop_notes = NULL,
    drop_list  = drop_list,
    id         = id,
    trt        = trt,
    spec_id    = 'adsl'
  )

  if(attach_data){
    out$data <- adsl
  }
  
  out
  
}

# test area####
if(FALSE){
  
  require(tidyverse)
  require(haven)
  require(labelled)
  
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  study <- c(99999)[1]#  , 99999, 99999)[3]
  # file  <- paste0('real_world_data/adsl/', study, '/adsl.sas7bdat')
  file <-  paste0('data/', study, '/ads/adsl.sas7bdat')
  
  id = 'SUBJID'
  trt = NULL
  keep = NULL
  drop = NULL
  filter = c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
  attach_data = TRUE
  
  spec <- adam_spec_adsl(file = file, id = id, filter = filter, attach_data = attach_data)
  
}

