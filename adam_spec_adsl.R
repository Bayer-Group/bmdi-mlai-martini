
#' spec adsl
#' 
#' @param file the file 
#' @param id name of id column to keep
#' @param trt name of trt column to keep
#' @param keep columns to be kept (overrides blacklist)
#' @param drop superseded. columns to be dropped (overrides whitelist)
#' @param filter character vector of filter criteria to be evaluated
#' @param attach_data boolean. attach the imported raw data

# test area####
if(FALSE){
  
  require(tidyverse)
  require(haven)
  require(labelled)
  
  # 'real_world_data/adsl/99999/adsl.sas7bdat'
  study <- c(99999, 99999, 99999)[3]
  file  <- paste0('real_world_data/adsl/', study, '/adsl.sas7bdat')
  
  id = 'SUBJID'
  trt = NULL
  keep = NULL
  drop = NULL
  filter = c("FASFL == 'Y'", "AGE < 80", "GENDER == 'female'")
  attach_data = TRUE
  
  spec <- adam_spec_adsl(file = file, id = id, filter = filter, attach_data = attach_data)
  
}



# adam_spec_adsl() ####

adam_spec_adsl <- function(
  file, 
  id = 'SUBJID',
  trt = NULL,
  keep = NULL, 
  drop = NULL,
  filter = NULL,
  attach_data = FALSE,
  ...
  
){
  
  # packages  ####
  #if (!require("pacman")) install.packages("pacman")  
  #pacman::p_load(labelled,   # setting and extracting column labels
  #               tidymodels  # modeling framework
  #)    
  
  #  read adsl ####
  #'adsl.sas7bdat' %in% list.files(path)
  adsl <- haven::read_sas(file)
  
  # create column dict (name <-> label)  
  dict <- labelled::var_label(adsl) %>% 
    tibble::enframe(name ='param', value = 'label') %>%  
    dplyr::mutate(label = map_chr(label, ~ .x[[1]])) %>% 
    dplyr::mutate(source = 'adsl')
  
  clmns <- dict$param
  labs  <- dict$label
  
  
  # define black and whitelist ####
  ## black and white list 
  black_list <- c(
    'RANDNO',
    'ADSNAME', 'STUDYID', # covered in constant
    "SITEID" ,  "SITENAM" , 
    "INVID"  ,  "INVNAM" ,
    # "TRT01P" ,  "TRT01PN"
    'AGEGREU'     # e.g. 99999: two agegroups, agegreu only for agegr01 -> discard
  )
  
  #white_list <- c()
  
  #discard$black_list <- black_list
  
  # identify date columns ####
  
  date_auto <- purrr::map_lgl(adsl, assertive.types::is_date) %>%  which() %>%  names
  date_lab  <- purrr::map_lgl(
    labelled::var_label(adsl), 
    ~ stringr::str_detect(str_to_lower(.x), 'year|month|day|date|time')) %>% 
    which()
  all_dates <- c(date_auto, names(date_lab))
  
  
  # identify pairs of categorical/numerical columns ####
  
  # for flags ####
  # naming convention   xxxFL -> xxxFN
  all_FL <-  c( #intersect( 
    clmns %>% stringr::str_subset('FL$' ),  
    clmns[labs %>% stringr::str_detect(' Flag$')] ) %>% 
    unique
  
  # flags without FL suffix in column name (e.g. SUBNY02=Subset 02 Analysis Flag in 99999)
  
  # flags_N  <- all_FL %>% 
  #   str_replace( 'FL$', 'FN' ) %>% 
  #  { . %in% clmns} %>% 
  #   all_FL[.] %>% 
  #   str_replace( 'FL$', 'FN' ) 
  
  
  flags <- c( all_FL , 
              stringr::str_replace( all_FL,'FL$', 'FN' )  ) %>%  unique()
  
  
  
  
  ### categoricals with numeric code
  
  
  # identify columns to keep... ####
  # columns with additional numeric code
  clmn_mod <- paste0(clmns, 'N')
  # clmn indices to order ()
  clmn_ind_lab  <- clmn_mod %in% clmns
  # columns to be kept
  clmn_cat  <- clmns[clmn_ind_lab]
  # columns to be used for level order, then dropped
  clmn_num  <- clmn_mod[clmn_ind_lab]
  
  lab_mod <- paste(labs, '(N)')
  lab_ind <- lab_mod %in% labs
  lab_cat <- labs[lab_mod %in% labs]
  lab_num <- lab_mod[lab_mod %in% labs]
  
  # mapping of columns to keep (labels) and columns to use for level order
  all_lab_lev <- dplyr::bind_rows(
    tibble::tibble(lab = clmn_cat,  
                   lev = clmn_num),
    tibble::tibble(lab = dict %>%  filter(label %in% lab_cat) %>%  pull(param) ,  
                   lev = dict %>%  filter(label %in% lab_num) %>%  pull(param)  )
  ) %>% 
    dplyr::distinct() 
  
  # keep list of all num codes to setdiff with all numeric columns
  all_num_codes <- all_lab_lev$lev
  
  # reduce to pairs for which level order needs to be extracted
  lab_lev <- all_lab_lev  %>% 
    dplyr::filter(!lab %in% flags) %>% 
    dplyr::filter(lab != id) %>% 
    dplyr::filter(lev != id)
  
  
  
  # num_codes <- c(clmn_cat, 
  #                dict %>%  filter(label %in% lab_cat) %>%  pull(column)) %>% 
  #   unique()
  # 
  # discard$num_codes <- lab_lev %>%  pull(lev)
  
  # identify combined columns ####
  
  # create list of factor levels ####
  lev_list <- list()
  for(r in 1:nrow(lab_lev)){
    lev <-  rlang::sym(lab_lev [r,] %>% dplyr::pull(lev))
    lab <-  rlang::sym(lab_lev [r,] %>% dplyr::pull(lab))
    
    levs <-  adsl %>% 
      dplyr::select(lab_lev [r,] %>%  as.character()) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(!!lev) %>% 
      dplyr::pull(!!lab)
    lev_list[[lab]] <- levs
    #adsl <-  adsl %>% 
    #    mutate(!! lab := factor(!!lab , levels = levs))
  }
  
  # identify combined columns ####
  
  all_slash <- labs %>% stringr::str_subset('/' )
  ind  <- all_slash %>%  
    stringr::str_split('/') %>% 
    purrr::map_lgl( ~ { all(.x %in% labs)})
  all_comb_columns <- all_slash[ind]
  
  
  # identify redundants for id and trt ####
  
  ## ...ids ####
  
  suppressWarnings({
    cors <- adsl %>%
      dplyr::mutate_all( ~{factor(.) %>% forcats::fct_inorder(.) %>% as.numeric(.)}) %>% 
      janitor::remove_constant(na.rm = TRUE) %>% 
      stats::cor(method = "spearman", use = 'pairwise.complete.obs')
  })
  
  redundant_id <-  cors[, id] %>% 
    tibble::enframe() %>% 
    dplyr::filter(near(value, 1)) %>% 
    dplyr::pull(name) %>% 
    setdiff(id)
  
  
  
  ## ...trt ####
  
  trt_adam <- intersect(
    c("TRT01A", "ARMCD", "ARM", "ACTARM", "ACTARMCD", "TRT01P", "TR01PG1", "TR02PG1", "TR01AG1", "TR02AG1"),
    colnames(adsl)
  )
  
  if (is.null(trt) && !is.null(trt_adam)){
    trt <- trt_adam[1]
  }
  redundant_trt <- setdiff(trt_adam, trt)
  
  # numerics without categorical pendant ####
  all_numerics <- adsl %>% 
    dplyr::select_if(is.numeric) %>% 
    colnames() 
  
  # empty columns 
  empties <- setdiff( 
    adsl %>%  colnames(),
    adsl %>%  janitor::remove_empty('cols') %>%  colnames()
  )
  
  # drop list ####
  drop_list <- c(
    drop,
    all_dates,
    all_num_codes,
    all_comb_columns,
    redundant_id,
    redundant_trt,
    empties
  ) %>% 
    setdiff(keep) %>% 
    unique()
  
  # selected columns ####
  select_list <- c(
    id,
    trt,
    names(lev_list),
    all_numerics,
    keep
  ) %>% 
    setdiff(drop_list)
  
  # check filter ####

  keep_filter <- map_lgl(filter, function(x){
    try_it <- try(
      {adsl %>% dplyr::filter(!! rlang::parse_expr(x))},
      silent = TRUE
    )
    is_error <- "try-error" %in% class(try_it)
    is_norow <- FALSE
    if (!is_error) is_norow <- nrow(try_it) == 0
    !(is_error || is_norow)
    
  })
  
  actual_filter <- filter[keep_filter]
  

  dict <- dict %>%  
    dplyr::mutate(selected = ifelse(
      param %in% select_list, TRUE, FALSE
    ) )
  
  
  # output ####
  out <- list(
    file = file,
    data = NULL,
    type = "adsl",
    filter = actual_filter,
    select = select_list,
    factor_levels = lev_list[intersect(select_list, names(lev_list))],
    dict = dict,
    drop_notes = NULL,
    id = id
  )

  if(attach_data){
    out$data <- adsl
  }
  
  out
  
}