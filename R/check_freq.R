#' Identify factors with low frequency classes
#'
#' @param x output of `\link{prepare_ml}()` or a tibble 
#' @param thres integer. Factors with at least one class of size smaller than `thres` will be identified. Defaults to 10.
#' @param quiet logical to suppress printing messages to the console

#' @return
#' In case factors are present in the data set that meet the condition of interest, 
#' relevant names are printed in the console and a list of frequency tables is returned.
#' 
#' @details 
#' If `x` is the output of `\link{prepare_ml}()`, the tibble `x$data$prep$train` is checked for
#'  factors with at least one low frequency class. 
#'  Please refer to the package vignette for further details.
#' 
#' @export
#' @section Authors:
#' Maike Ahrens (ahrensmaike), Sebastian Voss (svoss09)


check_freq <- function(
  x,
  thres = 10,
  quiet = FALSE
){
  
  data_check <- if(inherits(x, "martini_ml")) {
    x$data$prep$train
  }else if (is.null(x)) {
      cli::cli_abort("Please check your input. Currently NULL.")
    }else if (!is.data.frame(x)) {
      cli::cli_abort("Please check your input. Should be a data frame.")
    }else{
    x
  }
  
  # determine all class distributions
  fct_counts <- data_check %>% 
    dplyr::select(dplyr::where(is.factor), dplyr::where(is.character)) %>% 
    purrr::map(~{
      tibble::tibble(fct = .x) %>% 
        dplyr::count(fct, sort = TRUE)
    }) 
  
  # determine size of smallest class per factor
  min_counts <- fct_counts %>% 
    purrr::map_int(
      ~.x %>% dplyr::slice_min(n) %>%  
        dplyr::pull(n) %>% 
        unique()
    )
  
  # get names of 'risky' factors
  risky <- purrr::keep(min_counts, ~ {.x  < thres}) %>% 
    names()
  
  # determine overall minimum of class sizes (named)
  overall_min <- if( length(fct_counts) == 0) {
    NA_integer_
  } else{
    min_counts %>% sort() %>% magrittr::extract(1)
  }
  
  
  out <- list(
    vars = risky,
    counts = fct_counts[risky], 
    overall_min = overall_min
  )
  
  if (!quiet) {
    if (length(risky) > 0 ) {
      cli::cli_inform(c(
        'i' = '{cli::qty(risky)}The following factor{?s} {?has/have} low frequencies (<{thres}) in at least one class: \n{risky}'
        )
      )
    } else{
      cli::cli_text(
        c("No factors with low frequency class (<{thres}) detected in data set.")
      )
      
    }
  } 
  
  invisible(out)
}
