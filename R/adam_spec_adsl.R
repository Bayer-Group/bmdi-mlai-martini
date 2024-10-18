#' Create specification object for ADaM data sets of type 'adsl'
#' 
#' Given a file containing an adsl data set, \code{\link{adam_spec_adsl}()} will create a specification 
#' object for use in \code{\link{build_adsl}()} to actually create a subset of 
#' the data to be used in machine learning. For adsl specifically, the main task is the 
#' identification of noise and redundancies in the data and the selection of a potentially meaningful
#' set of columns (returned in \code{select}) and redundancies in the data. 
#' 
#' @param file the path of the sas(7bdat) or rds file to process
#' @param id name of id (e.g. SUBJIDN, SUBJID) column to keep.
#' Highly redundant variables will not be included in the suggested set of columns returned in \code{select} (see Details).
#' @param trt column to be used as the treatment variable. All other predefined treatment variables (see Details) are added
#' to the \code{drop_list}. If NULL, all treatment variables will be added to the \code{drop_list}.
#' @param keep,drop columns to be kept/dropped, independent of the technical selection process within this function
#' @param filter character vector of filters following \code{dplyr::filter()} syntax for use in \code{\link{build_adsl}()} (see Details).
#' Defaults to NULL. 
#' @param attach_data boolean. attach the imported raw data.
#' @param catalog_file path to the catalog file to be passed to 
#' [haven::read_sas()]. Defaults to NULL. 
#' Ignored if `file` is not a sas7bdat file.
#' 
#' @details
#' 
#' \describe{
#'   \item{*Subject id*}{Non-numeric columns are recoded as numeric, based on the order in which they appear in the data
#'   (sorted by \code{id}). All columns with a perfect Spearman correlation to \code{id} are considered redundant and added to
#'   the \code{drop_list}. In addition, all numeric columns with a perfect Spearman correlation to RANDDT (if available in the data)
#'   are also added to the \code{drop_list}, as well as RANDNO (if present in data).}
#'   \item{*Treatment variable*}{The predefined list of treatment variables is TRT01A, ARMCD, ARM, ACTARM, ACTARMCD, TRT01P, TR01PG1, TR02PG1, TR01AG1, TR02AG1.
#'   No more than one of these variables will be returned in \code{select}. Note that the chosen treatment representing
#'   variable will be renamed to the standard '.trt' in \code{\link{build_adsl}()}.}
#'   \item{*Filter check*}{Filters will be checked against the data and will only be kept if the filter would not throw an error and if the resulting
#'   data set has positive number of rows. See \code{\link{check_filter}()} for further details.}
#' }
#' 
#' @return 
#' A list containing the following 
#' \item{`file`, `md5`}{the name and md5 checksum, resp., of the file the generated spec is based upon}
#' \item{`data`}{the raw data set if \code{attach_data}, NULL otherwise}
#' \item{`data_info`}{a list containing the number of subjects `nsubj` and columns `ncol` in the data after applying `filter`}
#' \item{`type`}{character string \code{adsl}, generally giving the type of ADaM data set processed (\code{adsl}/\code{bds}/\code{occds})}
#' \item{`filter`}{subset of \code{filter} that yields non-empty result when applied individually (using \code{\link{check_filter}()}}
#' \item{`select`}{the suggested list of columns to select from the data set} 
#' \item{`factor_levels`}{a list column pairs factor/factorN to determine factor level order} 
#' \item{`flag_table`}{a tibble with columns id and any columns identified as flag (character and matching numeric) based on matching column names or labels}
#' \item{`id`, `trt`}{passing unchanged input}  
#' \item{`drop_list`}{a list containing column names suggested to be dropped with the entry
#' name identifying the rationale for the discard
#'   \describe{ 
#'     \item{`drop`}{passing the user input `drop`} 
#'     \item{`datetime`}{date/times columns} 
#'     \item{`numcode`}{numeric code for another variable (incl numeric flags)} 
#'     \item{`flag`}{flags (both numeric and character columns), see also `flag_table`}
#'     \item{`combination`, `empty`, `constant`}{combined, empty and constant columns, resp.}
#'     \item{`redundancy`}{columns with redundant information to `id` and `trt` if provided)}
#'  }}
#' \item{`spec_id`}{character string \code{adsl}, generally the name of the domain}  
#' \item{`dict`}{a tibble of column names and labels (if present in the data set)}  
#' 
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
#' 



adam_spec_adsl <- function(
    file         = NULL,
    data         = NULL,
    id           = 'USUBJID',
    trt          = NULL,
    keep         = NULL, 
    drop         = NULL,
    filter       = NULL,
    attach_data  = FALSE, 
    catalog_file = NULL
){

  # TODO adam_spec_adsl() - refactor everything!!!
   
  # initial check(s)  ####
  
  if (all(c(is.null(data), is.null(file)))) {
    usethis::ui_stop(
      paste0(
        'At least one of ', usethis::ui_code('data'), ' or ',
        usethis::ui_code('file'), ' need to be provided.\n'))
  }
  
  # import ####
  if (is.null(data)) {
    imported <- import_info(file, catalog_file = catalog_file)
    data   <- imported$data
    md5    <- imported$md5
    size   <- imported$size
    domain <- basename(file) %>% tools::file_path_sans_ext()
  }else{
    md5    <- NULL
    size   <- NULL
    domain <- domain %||% "custom"
  }
  
  # check input ####
  # collect column name parameters ####
  col_spec <- list(
    "id"  = list(column = id,  required = TRUE),
    "trt" = list(column = trt, required = FALSE)
  )
  
  col_select_raw <- purrr::imap(col_spec, ~{
    check_role(
      data = data, 
      role = .y, 
      column_spec = .x$column, 
      required = .x$required,
      type = "adsl", 
      call = rlang::caller_env(n = 4)
    )
  })
  
  col_select <- purrr::map(col_select_raw, "column")
  
  use_for_build <- purrr::map_lgl(col_select_raw, "check_passed") %>% all()

  # dict creation (name <-> label)  ####
  dict <- adsl_dict(data)
  
  # identify columns to drop ####
  # and create level list
  # NOTE automated detection may yield false positives and false negatives
  
  identify_res <- adsl_identify(
    data,
    dict = dict,
    id  = col_select$id,
    trt = col_select$trt
  )
  
  # transform date and time to character to ensure exclusion
  # from numeric selection
  # (caution: mutate() deletes column labels)
  labs_adsl <- labelled::var_label(data)
  data <- data %>% 
    dplyr::mutate_at(identify_res$to_remove$dttm, as.character) %>% 
    labelled::set_variable_labels(.labels = labs_adsl, .strict = TRUE)
  
  
  # identify all numerics ####
  # candidates for select, 
  # will be intersected with numeric codes
  all_numerics <- data %>% 
    dplyr::select_if(is.numeric) %>% 
    colnames() 
  
  
  # collect output ####
  
  # ... drop list ####
  drop_list <- list(
    "drop"         = drop,
    "datetime"     = identify_res$to_remove$dttm,
    "numcode"      = identify_res$to_remove$factor,
    "combination"  = identify_res$to_remove$combined,
    "redundancy"   = identify_res$to_remove$redundant,
    "flag"         = identify_res$to_remove$flag,  
    "empty"        = identify_res$to_remove$empty,
    "constant"     = identify_res$to_remove$constant,
    "blacklist"    = identify_res$to_remove$black_list
  ) %>% 
    purrr::map(~setdiff(., keep)) %>% 
    purrr::map(~setdiff(., c(id, trt)))  
    

  # ... selected columns ####

  lev_list <- identify_res$lev_list
  
  select_list <- c(
    id,
    trt,
    names(lev_list),
    all_numerics,
    keep
  ) %>% 
    setdiff(drop_list %>% unlist() %>% unique())
  
  # extract relevant factor levels
  factor_levels <- lev_list[intersect(select_list, names(lev_list))]
  
  # ... complete drop list
  drop_list$other <- colnames(data) %>% 
    setdiff(select_list) %>% 
    setdiff(drop_list %>% unlist())
    
  # ... check filter ####
  # only filter that individually yield non-empty tibbles are kept
  keep_filter   <- check_filter(data, filter, data_id = 'adsl')$individual %>% 
    purrr::map_lgl("keep") %>% 
    as.logical()
  actual_filter <- filter[keep_filter]
  
  # ... dictionary ####

  dict <- dict %>%  
    dplyr::mutate(selected = param %in% select_list)
  
  
  # ... flag table ####
  
  flag_table <- data %>% 
    dplyr::select(tidyselect::any_of(
      c(id, identify_res$to_remove$flag)
    ))
  
  # ... output object ####  
  
  create_spec_out(
    file, data, md5, size, actual_filter, domain, col_select, dict,
    select_list, factor_levels, drop_list, flag_table,
    type = "adsl", 
    attach_data = attach_data
  )
  
}

 
 

# helper ####
 
#' create adsl dictionary from column labels/names
#'
#' @param data adsl-like data set
#' @param param,label column name for resulting tibble where column name 
#' (param) and label are stored
#'
#' @return a `tibble`
#'
#' 
 adsl_dict <- function(
    data,
    param = 'param',
    label = 'label'
  ){
  
  purrr::imap(labelled::var_label(data), ~{.x %||% .y}) %>% 
    unlist() %>% 
    tibble::enframe(name = param, value = label) %>% 
    dplyr::mutate(source = 'SL') %>% 
    dplyr::mutate(type   = 'adsl')
 }
 
 
 # adsl_identify(data) 
 
 #' identify/categorize columns from adsl
 #'
 #' family of helper functions to identify columns to drop from adsl data set 
 #' @param data adsl-like data set in which to identify particular columns of interest
 #' @param dict,dict_param,dict_label dict is `tibble` as created by `adsl_dict()`
 #' where `dict_param` and `dict_label` indicate the columns in `dict`
 #' containing for parameter names (column names of `data`) and labels, resp.
 #' @param type character vector determining the categories of column types to identify. 
 #' defaults all possible categories: 
 #' `dttm`, `constant`, `combined`, `flag`, `factor`, `redundant`
 #' @param id,trt user-selected column names in `data` for ID and treatment column,
 #' defaulting to `SUBJID` and `TRT01A`, resp.
 #' @param clmn_flag (factor and redundants only) character vector of names identified as flags
 #' @param black_list character vector of columns that should be dropped for 
 #' most analyses, see details.
 #' 
 #' @return list with two top level entries, where `to_remove` is a list of 
 #' column names from `data` that were identified as candidates for a given category
 #' and `lev_list` a `list` required to set factor level orders.
 #' 
 #' @details
 #' 
 #' Columns meeting the following criteria are returned
 #' 
 #' `adsl_identify_dttm()`:  `methods::is(.x, "Date")` is TRUE, 
 #' the label contains strings 'year', 'month', 'day', 'date' or 'time'
 #'  (not case sensitive), class is one of 'difftime', 
 #'  'hms', 'Period', 'POSIXct', 'POSIXt', 'Date'
 #' 
 #'  `adsl_identify_constant()`: identification via 
 #'  `janitor::remove_empty(which = 'cols')`, `janitor::remove_constant(na.rm = TRUE)`
 #' 
 #' `adsl_identify_redundant()`: redundant columns to selected trt and id columns
 #' 
 #' `adsl_identify_combined()`: if labels (from dict) contain '/' 
 #' and all parts are column names themselves
 #' 
 #' By default, `black_list` contains `RANDNO`, 
 #' `SITEID`, `SITENAM`, `INVID`, `INVNAM`.
 #' 
 #' @section Authors: 
 #' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)
 #' 
 #' @name adsl_identify
 NULL
 
 #' @rdname adsl_identify 
 
 adsl_identify <- function(
    data,
    dict = NULL,
    type = c(
      # adsl only
      'dttm', 'constant', 
      # using dict
      'combined', 'flag',
      # using flag results
      'factor', 'redundant'
    ),
    
    dict_label = "label", 
    dict_param = "param", 
    id         = 'SUBJID',
    trt        = 'TRT01A',
    
    black_list = c(
      "RANDNO",
      #"ADSNAME", "STUDYID",
      "SITEID" , "SITENAM", 
      "INVID"  , "INVNAM"
    )
    
  ){

  # input checks
  type <- rlang::arg_match(type, multiple = TRUE)
  stopifnot(c(id, trt) %in% names(data))
  if(!is.null(dict)) stopifnot(c(dict_label, dict_param) %in% names(dict))
  
  type <- rlang::arg_match(type, multiple = TRUE) %>% 
    purrr::set_names()
  type_orig <- type # to remove `flag` from output
  
  # factor detection makes use of identified flag columns
  # print(type)
  if(any(c('factor', 'redundant') %in% type)){
    type %>% {c(., 'flag')} %>% purrr::set_names() %>% unique()
  }
  
  if(is.null(dict)) {
    dict <- adsl_dict(
      data,
      label = dict_label,
      param = dict_param
    )
  }
  
  all_args <- as.list(environment())

  # TODO rewrite if flag %in% type -> compute  first, add clmn_flag = out$flag 
  # to all_args and map over everything but flag
  to_remove <- purrr::map(type[! names(type) %in% c('factor', 'redundant')] , ~{
    fct_name <- paste0('adsl_identify_', .x)
    fct_args <- formals(fct_name) %>% names()
    
    use_args <- all_args  %>% 
      magrittr::extract(fct_args) %>% 
      purrr::compact()
    
    do.call(
      fct_name,
      use_args
    )
  })

  if('factor' %in% type){
    res_fct <- do.call(
      adsl_identify_factor,
      tibble::lst(data, id, dict, dict_label, dict_param, clmn_flag = to_remove$flag)
    )
    # NOTE factors should not be in the 'to_remove' entry
    to_remove$factor <- res_fct[['all_num_codes']]
   
  }else{
    res_fct <- NULL
  }
  
  if('redundant' %in% type){
    to_remove$redundant <- do.call(
      adsl_identify_redundant,
      tibble::lst(data, id, trt, clmn_flag = to_remove$flag)
    )
  }
  
  if(!'flag' %in% type_orig) to_remove$flag <- NULL
  
  to_remove$black_list <- intersect(black_list, colnames(data))
  
  tibble::lst(to_remove, lev_list = res_fct$lev_list)
}
  

 
 
#' @rdname adsl_identify 

adsl_identify_dttm <- function(
    data
  ){
  
  # identify date by variable type...
  date_auto <- purrr::map_lgl(data, ~methods::is(.x, "Date")) %>% which() %>% names()
  
  # ...and label
  no_labels <- labelled::var_label(data) %>% purrr::compact() %>% purrr::is_empty()
  if(no_labels){
    date_lab <- character()
  }else{
    date_lab  <- purrr::map_lgl(labelled::var_label(data), ~{
      if(!is.null(.x)){
        stringr::str_detect(stringr::str_to_lower(.x), 'year|month|day|date|time')
      }else{
        FALSE
      }
    }) %>% which()
  }
  
  all_dates <- c(date_auto, names(date_lab))
  
  # identify time variables
  all_times <- purrr::map_lgl(
    data, ~{any(class(.) %in% c("difftime", "hms", "Period", "POSIXct", "POSIXt", "Date"))}
  ) %>% which() %>% names()
  
  c(all_dates, all_times) %>% unique()
  
}
 
#' @rdname adsl_identify 

adsl_identify_constant <- function(
    data
  ){
  
  empty <- setdiff( 
    data %>% colnames(),
    data %>% janitor::remove_empty(which = 'cols') %>% colnames()
  )
    
  constant <- setdiff( 
    data %>% colnames(),
    data %>% janitor::remove_constant(na.rm = TRUE) %>% colnames()
  ) %>% setdiff(empty)
  
  tibble::lst(constant, empty)
  
}


#' @rdname adsl_identify

adsl_identify_combined <- function(
  data, 
  dict       = NULL,
  dict_label = "label",
  dict_param = "param"
){
  
  # Only works based on column label (column name if label is missing, which would not contain '/')
  # TODO LATER rewriteto actually using column values to identify combinations
  
  if(!is.null(dict)) stopifnot(c(dict_label, dict_param) %in% names(dict))
  
  all_slash <- dict[[dict_label]] %>% stringr::str_subset('/')
  ind       <- all_slash %>%  
    stringr::str_split('/') %>% 
    purrr::map_lgl(~{all(.x %in% dict[[dict_label]])})
  
  all_comb <- all_slash[ind]
  
  dict %>% 
    dplyr::filter(!!rlang::sym(dict_label) %in% all_comb) %>% 
    dplyr::pull(!!rlang::sym(dict_param))
  
}

#' @rdname adsl_identify

adsl_identify_redundant <- function(
  data, 
  id, 
  trt, 
  clmn_flag
){
  
  # ... ... ids ####
  
  # transform all variables into numerics to enable correlation computation
  # sorting by id and using fct_inorder to assess monotonous association of id column with remaining data set
  # (excluding previously identified flags, as they might introduce zero variance issues)
  # -> identifies all character id columns (also those with a different order as 'id')
  # and all numeric id columns with the same order as 'id'
  adsl_cor_id <- data %>%
    dplyr::select(-tidyselect::all_of(clmn_flag)) %>% 
    dplyr::arrange(!!rlang::sym(id)) %>% 
    dplyr::mutate_if(~!is.numeric(.), ~{
      factor(.x) %>% 
        forcats::fct_inorder() %>% 
        as.numeric()
    }) %>% 
    janitor::remove_constant(na.rm = TRUE)
  
  # correlations with 'id'
  cors_id <- stats::cor(
      adsl_cor_id, adsl_cor_id[, id], 
      method = "spearman", 
      use = 'pairwise.complete.obs'
    ) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("name") %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(value = tidyselect::all_of(id))
  
  # potential remaining numeric id columns are identified by monotonous relation with randomization date
  cors_randdt <- NULL
  
  if("RANDDT" %in% colnames(data)){
    # also check it's not constant (e.g. all NA in IA)
    if(data %>% dplyr::pull(RANDDT) %>% dplyr::n_distinct() %>% {. > 1}){
      
      adsl_cor_randdt <- data %>%
        dplyr::select(-tidyselect::all_of(clmn_flag)) %>% 
        dplyr::mutate(RANDDT = as.Date(RANDDT) %>% as.numeric()) %>% 
        dplyr::select_if(is.numeric) %>% 
        janitor::remove_constant(na.rm = TRUE)
      
      # NOTE stats::cor causes 'zero-sd' warning. use quietly
      cors_randdt <- cor_quiet( #stats::cor(
        adsl_cor_randdt,
        adsl_cor_randdt[, "RANDDT"],
        method = "spearman",
        use    = 'pairwise.complete.obs'
      ) %>% magrittr::extract('result') %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("name") %>% 
        tibble::as_tibble() %>% 
        dplyr::rename(value = tidyselect::all_of("RANDDT"))
    }}
  
  redundant_id <- cors_id %>% 
    dplyr::bind_rows(cors_randdt) %>% 
    dplyr::filter(dplyr::near(value, 1)) %>% 
    dplyr::pull(name) %>% 
    setdiff(id)
  
  # randomization number (standard name = RANDNO) can not be identified by algorithm
  if ("RANDNO" %in% colnames(data)) redundant_id <- c(redundant_id, "RANDNO")
  
  # ... ... treatment ####
  
  # match standard treatment column names against actual data
  trt_adam <- intersect(
    c("TRT01A", "ARMCD", "ARM", "ACTARM", "ACTARMCD", "TRT01P", "TR01PG1", "TR02PG1", "TR01AG1", "TR02AG1"),
    colnames(data)
  )
  
  redundant_trt <- setdiff(trt_adam, trt)
  
  c(redundant_id, redundant_trt) %>% unique()
  
}

#' @rdname adsl_identify

adsl_identify_flag <- function(
  data,     
  dict, 
  dict_param = "param", 
  dict_label = "label"
){
  
  labs  <- dict[[dict_label]]
  clmns <- dict[[dict_param]]
  
  # flag naming convention: xxxFN (numeric) + xxxFL (character)
  
  # TODO check if suffix 'FN' + numeric is exclusively allowed for Flags, otherwise, condition above may catch too much
  all_fn <- data %>% dplyr::select_if(is.numeric) %>% names() %>% stringr::str_subset('FN$')
  # NOTE from experience, if either is missing, it is xxxFL, not xxxFN
  #all_fl <- data %>% dplyr::select_if(is.character) %>% names() %>% stringr::str_subset('FL$')
  all_lab_flag <- clmns[stringr::str_to_upper(labs) %>% stringr::str_detect('\\bFLAG\\b')]

  c(
    all_fn, stringr::str_replace(all_fn, 'FN$', 'FL'),
    #all_fl, stringr::str_replace(all_fl, 'FL$', 'FN'),
    all_lab_flag
  ) %>% 
    unique() %>% 
    sort() %>% 
    intersect(clmns)
  
}

#' @rdname adsl_identify

adsl_identify_factor <- function(
    data,
    id,
    clmn_flag = NULL,
    dict, 
    dict_param = "param", 
    dict_label = "label"
){
  
  labs  <- dict[[dict_label]]
  clmns <- dict[[dict_param]]
  
  clmns_num <- data %>% dplyr::select_if(is.numeric) %>% names()
  
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
        dplyr::slice(match(lab_cat, dict %>%  dplyr::pull(!!rlang::sym(dict_label)))) %>%  
        dplyr::pull(!!rlang::sym(dict_param)),
      lev = dict %>%
        dplyr::slice(match(lab_num, dict %>%  dplyr::pull(!!rlang::sym(dict_label)))) %>% 
        dplyr::pull(!!rlang::sym(dict_param))
    )
  ) %>% 
    dplyr::distinct() %>% 
    # only keep guessed pairs if the column guessed as numeric code is actually numeric
    dplyr::filter(lev %in% clmns_num)
  
  # reduce to pairs for which level order needs to be extracted
  lab_lev <- all_lab_lev  %>% 
    {if(!is.null(clmn_flag)){
      dplyr::filter(., !lab %in% clmn_flag) 
    }else{.}} %>% 
    dplyr::filter(lab != id) %>% 
    dplyr::filter(lev != id)
  
  # account for 'numeric coding only without matching categorical'
  num_only <- dict %>% 
    # start with potential num codes judging by label suffix
    dplyr::filter(stringr::str_detect(label, "\\(N\\)$")) %>% 
    dplyr::pull(param) %>% 
    # remove those for which pairs were identified (in lab_lev)
    setdiff(all_lab_lev$lev) %>% 
    # check column is actually numeric
    intersect(clmns_num) 
  
  if(length(num_only) > 0){
    
    # non-integer candidates 
    no_integer <- data %>% 
      dplyr::select(tidyselect::any_of(num_only)) %>% 
      dplyr::select_if( ~{readr::guess_parser(.x, guess_integer = TRUE) != 'integer'} ) %>% 
      names() 
    
    # candidates with too many levels (potentially numeric values)
    # TODO threshold as parameter?
    thres_fct <- min(nrow(data)/2, 50)
    
    too_many_levels <- num_only %>% 
      purrr::map_lgl(~{
        data[[.x]] %>% dplyr::n_distinct() %>% {. > thres_fct}
      }) %>% 
      which() %>% 
      num_only[.]
    
    num_only <- setdiff(
      num_only, 
      c(too_many_levels, no_integer)
    )
    
    if(length(num_only) > 0){
      lab_lev <- dplyr::bind_rows(
        lab_lev,
        tibble::tibble(
          lab = num_only,
          lev = num_only
        )
      )
    }
  }
  
  # create list of factor levels
  lev_list <- list()
  for(r in 1:nrow(lab_lev)){
    lev  <- rlang::sym(lab_lev[r,] %>% dplyr::pull(lev))
    lab  <- rlang::sym(lab_lev[r,] %>% dplyr::pull(lab))
    
    levs <- data %>% 
      dplyr::select(lab_lev[r,] %>% as.character()) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(!! lev) %>% 
      dplyr::pull(!! lab) %>% 
      na.exclude()
    
    levs_label          <- attr(levs, "label") 
    attributes(levs)    <- NULL
    attr(levs, "label") <- levs_label
    
    lev_list[[lab]] <- levs
  }
  
  tibble::lst(
    all_num_codes = all_lab_lev$lev,
    lev_list
  )
  
}


