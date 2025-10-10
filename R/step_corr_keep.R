#' High correlation filter
#'
#' `step_corr_keep()` creates a *specification* of a recipe step that will
#' potentially remove variables that have large absolute correlations with other
#' variables, keeping just one representative from highly correlated variable 
#' pairs. The choice of representatives can be controlled by providing a set of
#' variables that should be prioritized.
#'
#' @inheritParams step_log_skewness 
#' @param threshold A value for the threshold of absolute correlation values.
#'   The step will try to remove the minimum number of columns so that all the
#'   resulting absolute correlations are less than this value.
#' @param keep A character vector, containing variables that should be kept. 
#'   These will be prioritized when selecting a representative from a 
#'   variable pair with an absolute correlation greater than `threshold` 
#'   (see details). If `NULL`, this step is equivalent to 
#'   [recipes::step_corr()].
#' @param use A character string for the `use` argument to the [stats::cor()]
#'   function.
#' @param method A character string for the `method` argument to the
#'   [stats::cor()] function.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
#' @param high_corr A tibble containing all correlations above `threshold`. 
#'   These values are not determined until [prep()] is called.
#' @template recipe-step-return
#' 
#' @author  Modified from [recipes::step_corr()].
#' @seealso [recipes::step_corr()]
#' 
#' @export
#'
#' @details
#'
#' This step can potentially remove columns from the data set. This may
#' cause issues for subsequent steps in your recipe if the missing columns are
#' specifically referenced by name. To avoid this, see the advice in the
#' _Tips for saving recipes and filtering columns_ section of 
#' [recipes::selections].
#'
#' This step attempts to remove variables to keep the largest absolute
#' correlation between the variables less than `threshold`.
#'
#' The filter tries to prioritize predictors for removal based on the global
#' affect on the overall correlation structure. If you have two
#' predictors with an absolute correlation above `threshold`, the variable with 
#' the larger average correlation with all other predictors will be removed, 
#' unless it is specified in `keep` as a variable the user wants to prioritize. 
#' If the absolute correlation of two variables in `keep` exceeds the 
#' `threshold`, the variable with the larger average correlation to the other 
#' predictors will be removed and and the user is informed by a message in the 
#' console.
#'
#' When a column has a single unique value, that column will be excluded from
#' the correlation analysis. Also, if the data set has sporadic missing values
#' (and an inappropriate value of `use` is chosen), some columns will also be
#' excluded from the filter.
#'
#' The arguments `use` and `method` don't take effect if case weights are used
#' in the recipe.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected to be removed}
#'   \item{id}{character, id of this step}
#' }
#' 
#' # Tuning Parameters
#' 
#' The `threshold` parameter can be tuned.
#' 
#' @template recipe-case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("MASS")
#' # create a data set
#' set.seed(1717)
#' p <- 5
#' corrm <- matrix(numeric(p^2), ncol = p, nrow = p)
#' # variable 2 has a higher average correlation 
#' # than all other variables
#' corrm[,2] <- corrm[2,] <- .2
#' # variable 1 and 2 have high correlation
#' corrm[1,2] <- corrm[2,1] <- .9
#' diag(corrm) <- 1
#' X <- MASS::mvrnorm(n = 100, mu = rep(0, p), Sigma = corrm) %>% 
#'   tibble::as_tibble(.name_repair = ~paste0("V", 1:p))
#' 
#' # apply correlation filter without specifying `keep`
#' rec_prep <- recipes::recipe(~., data = X) %>% 
#'   step_corr_keep(
#'     recipes::all_numeric_predictors(),
#'     threshold = .8
#'   ) %>% 
#'   recipes::prep()
#' 
#' recipes::bake(rec_prep, new_data = NULL)
#' 
#' # make sure that "V2" is kept
#' rec_keep_prep <- recipes::recipe(~., data = X) %>% 
#'   step_corr_keep(
#'     recipes::all_numeric_predictors(),
#'     threshold = .8,
#'     keep = "V2"
#'   ) %>% 
#'   recipes::prep()
#' 
#' recipes::bake(rec_keep_prep, new_data = NULL)
#' 
#' # inspect high correlations
#' rec_keep_prep$steps[[1]]$high_corr

step_corr_keep <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  use = "pairwise.complete.obs",
  method = "pearson",
  keep = NULL,
  removals = NULL,
  high_corr = NULL,
  skip = FALSE,
  id = recipes::rand_id("corr_keep")
) {
  
  recipes::add_step(
    recipe,
    step_corr_keep_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      threshold = threshold,
      use = use,
      method = method,
      keep = keep,
      removals = removals,
      high_corr = high_corr,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_corr_keep_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    use,
    method,
    keep,
    removals,
    high_corr,
    skip,
    id,
    case_weights
  ) {
    recipes::step(
      subclass = "corr_keep",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      use = use,
      method = method,
      keep = keep,
      removals = removals,
      high_corr = high_corr,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

corr_keep_filter <-
  function(
    x,
    wts = NULL,
    cutoff = .90,
    use = "pairwise.complete.obs",
    method = "pearson",
    keep = NULL
  ) {
    x <- recipes::correlations(x, wts = wts, use = use, method = method)
    
    # create correlation tibble in long format for output 
    high_corr <- corr_stretch(x) %>% dplyr::filter(abs(r) > cutoff)
    
    if (any(!vctrs::vec_detect_complete(x))) {
      all_na <- apply(x, 2, function(x) all(is.na(x)))
      if (sum(all_na) >= nrow(x) - 1) {
        cli::cli_warn(
          "Too many correlations are `NA`; skipping correlation filter."
        )
        return(numeric(0))
      } else {
        na_cols <- which(all_na)
        if (length(na_cols) > 0) {
          x[na_cols, ] <- 0
          x[, na_cols] <- 0
          cli::cli_warn(
            "The correlation matrix has missing values. \\
            {length(na_cols)} column{?s} {?was/were} excluded from the filter."
          )
        }
      }
      if (anyNA(x)) {
        cli::cli_warn(
          "The correlation matrix has sporadic missing values. \\
          Some columns were excluded from the filter."
        )
        x[is.na(x)] <- 0
      }
      diag(x) <- 1
    }
    averageCorr <- colMeans(abs(x))
    # adjusting recipes::corr_filter() to account for predetermined representatives
    # vars_keep_corr
    avgCorrVarsRank <- as.numeric(as.factor(averageCorr))
    if (length(keep) > 0) {
      var_in_keep <- names(averageCorr) %in% keep
      avgCorrVarsRank[var_in_keep] <- avgCorrVarsRank[var_in_keep] - max(avgCorrVarsRank)
    }
    
    x[lower.tri(x, diag = TRUE)] <- NA
    combsAboveCutoff <- which(abs(x) > cutoff)
    
    colsToCheck <- ceiling(combsAboveCutoff / nrow(x))
    rowsToCheck <- combsAboveCutoff %% nrow(x)
    
    # Discard column variable in the correlation pair with the higher average
    # correlation across all pairwise correlations
    colsToDiscard <- avgCorrVarsRank[colsToCheck] > avgCorrVarsRank[rowsToCheck]
    rowsToDiscard <- !colsToDiscard
    
    deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
    deletecol <- unique(deletecol)
    if (length(deletecol) > 0) {
      deletecol <- colnames(x)[deletecol]
    }
    
    if (length(intersect(deletecol, keep))>0) {
      cli::cli_inform(c(
        "Representatives to be kept for correlated variables (above {cutoff}) were {keep}.",
        '!' = "Not all representatives were kept as the correlation amongst them was above the cutoff.",
        'i' = "{intersect(deletecol, keep)} {?was/were} removed from the feature set."
      ))
    }
    
    list(
      removals = deletecol,
      high_corr = high_corr
    )
  }

#' @exportS3Method 
prep.step_corr_keep <- function(x, recipe, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  #recipes::check_type(training[, col_names], types = c("double", "integer"))
  do.call(
    utils::getFromNamespace("check_type", "recipes"),
    list(dat = training[, col_names], types = c("double", "integer"))
  )
  #recipes:::check_number_decimal(x$threshold, min = 0, max = 1, arg = "threshold")
  do.call(
    utils::getFromNamespace("check_number_decimal", "rlang"),
    list(x = x$threshold, min = 0, max = 1, arg = "threshold")
  )
  use <- x$use
  rlang::arg_match(
    use,
    c(
      "all.obs",
      "complete.obs",
      "pairwise.complete.obs",
      "everything",
      "na.or.complete"
    )
  )
  method <- x$method
  rlang::arg_match(method, c("pearson", "kendall", "spearman"))
 
  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  
  if (length(col_names) > 1) {
    res_corr_keep_filter <- corr_keep_filter(
      x = training[, col_names],
      wts = wts,
      cutoff = x$threshold,
      use = use,
      method = method,
      keep =  x$keep
    )
    filter <- res_corr_keep_filter$removals
    high_corr <- res_corr_keep_filter$high_corr
  } else {
    filter <- character(0)
    high_corr <- NULL
  }
 
  step_corr_keep_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    use = use,
    method = method,
    keep = x$keep,
    removals = filter,
    high_corr = high_corr,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @exportS3Method 
bake.step_corr_keep <- function(object, new_data, ...) {
  already_in_recipes <- exists('recipes_remove_cols', where = asNamespace('recipes'), mode = 'function')
  new_data <- if(already_in_recipes){
    recipes::recipes_remove_cols(new_data, object)
  }else{ # fallback: copy in martini
    martini_recipes_remove_cols(new_data, object)
  }
  new_data
}

#' @exportS3Method 
print.step_corr_keep <-
  function(x, width = max(20, options()$width - 36), ...) {
    title <- "Correlation filter on "
    recipes::print_step(
      x$removals,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }



tidy_filter <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble::tibble(terms = unname(x$removals))
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @exportS3Method 
tidy.step_corr_keep <- tidy_filter

#' @exportS3Method 
tunable.step_corr_keep <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_corr_keep",
    component_id = x$id
  )
}


#' Removes original columns if options apply
#'
#' This helper function should be used whenever the argument
#' `keep_original_cols` is used in a function.
#'
#' @param new_data A tibble.
#' @param object A step object.
#' @param col_names A character vector, denoting columns to remove.
#' @return new_data with `col_names` removed if `get_keep_original_cols(object)
#'   == TRUE` or `object$preserve == TRUE`.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @export
martini_remove_original_cols <- function(new_data, object, col_names) {
  keep_original_cols <- get_keep_original_cols(object)
  if (any(isFALSE(object$preserve), !keep_original_cols)) {
    new_data <- martini_remove_original_cols(new_data, object, col_names)
  }
  new_data
}

#' Removes columns if options apply
#'
#' This helper function removes columns based on character vectors.
#'
#' @param new_data A tibble.
#' @param object A step object.
#' @param col_names A character vector, denoting columns to remove. Will
#'   overwrite `object$removals` if set.
#'
#' @return `new_data` with column names removed if specified by `col_names` or
#'   `object$removals`.
#' @keywords internal
#'
#' @seealso [developer_functions]
#'
#' @export
martini_remove_original_cols <- function(new_data, object, col_names = character()) {
  if (length(col_names) > 0) {
    removals <- col_names
  } else if (length(object$removals) > 0) {
    removals <- object$removals
  } else {
    return(new_data)
  }
  
  if (length(removals) > 0) {
    # drop = FALSE in case someone uses this on a data.frame
    new_data <- new_data[, !(colnames(new_data) %in% removals), drop = FALSE]
  }
  new_data
}
