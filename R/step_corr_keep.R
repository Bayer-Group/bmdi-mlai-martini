#' High correlation filter with a twist
#'
#' `step_corr_keep()` creates a *specification* of a recipe step that will
#' potentially remove variables that have large absolute correlations with other
#' variables BUT with a twist.
#'
#' @inheritParams step_center 
#' 
#' TODO
#' @param threshold A value for the threshold of absolute correlation values.
#'   The step will try to remove the minimum number of columns so that all the
#'   resulting absolute correlations are less than this value.
#' @param use A character string for the `use` argument to the [stats::cor()]
#'   function.
#' @param method A character string for the `method` argument to the
#'   [stats::cor()] function.
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [prep()] is
#'   called.
# @template step-return
# @template filter-steps
#' @author Modified from recipes. Original R code for filtering algorithm by Dong Li, modified by Max
#'   Kuhn. Contributions by Reynald Lescarbeau (for original in `caret`
#'   package). Max Kuhn for the `step` function.
#' @export
#'
#' @details
#'
#' This step attempts to remove variables to keep the largest absolute
#' correlation between the variables less than `threshold`.
#'
#' The filter tries to prioritize predictors for removal based on the global
#' affect on the overall correlation structure. If you have two identical
#' predictors, the variable ordered first will be removed.
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
# When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
# columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected to be removed}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_corr"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
# @template case-weights-unsupervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(biomass, package = "modeldata")
#'
#' set.seed(3535)
#' biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))
#'
#' biomass_tr <- biomass[biomass$dataset == "Training", ]
#' biomass_te <- biomass[biomass$dataset == "Testing", ]
#'
#' rec <- recipe(
#'   HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + duplicate,
#'   data = biomass_tr
#' )
#'
#' corr_filter <- rec |>
#'   step_corr_keep(all_numeric_predictors(), threshold = .5)
#'
#' filter_obj <- prep(corr_filter, training = biomass_tr)
#'
#' filtered_te <- bake(filter_obj, biomass_te)
#' round(abs(cor(biomass_tr[, c(3:7, 9)])), 2)
#' round(abs(cor(filtered_te)), 2)
#'
#' tidy(corr_filter, number = 1)
#' tidy(filter_obj, number = 1)
step_corr_keep <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  use = "pairwise.complete.obs",
  method = "pearson",
  keep = NULL, #vars_keep_corr,
  removals = NULL,
  high_corr = NULL, # check if necessary
  skip = FALSE,
  id = recipes::rand_id("corr_keep")
) {
  
  # check for previous log step
  # note: we cannot check for previous backtrafo
  
  columns_logged <- character(0)
  if (any('log' %in% tidy(recipe)$type)) {
    
    number_log_step <- recipe %>% 
      tidy() %>% 
      pull(type) %>% 
      magrittr::equals('log') %>% 
      which() %>% 
      tail(1)
    columns_logged <- recipe$steps[[number_log_step]]$columns
      
  }
  
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
      columns_logged = columns_logged,
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
    columns_logged,
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
      columns_logged = columns_logged,
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
    # adjusting recipes::corr_filter() to account for predetermined representives
    # vars_keep_corr
    avgCorrVarsRank <- as.numeric(as.factor(averageCorr))
    if (length(keep)> 0) {
      avgCorrVarsRank[names(averageCorr) %in% keep] <- avgCorrVarsRank[names(averageCorr) %in% keep] - max(avgCorrVarsRank)
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
      high_corr = x
    )
  }

#' @exportS3Method 
prep.step_corr_keep <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))
  recipes:::check_number_decimal(x$threshold, min = 0, max = 1, arg = "threshold")
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
    columns_logged = x$columns_logged,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @exportS3Method 
bake.step_corr_keep <- function(object, new_data, ...) {
  new_data <- recipes::recipes_remove_cols(new_data, object)
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

#' @rdname tidy.recipe
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
