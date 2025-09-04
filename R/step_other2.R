#' Collapse infrequent categorical levels
#'
#' `step_other2()` creates a *specification* of a recipe step that will
#' potentially pool infrequently occurring values into an `"other_ml"` category.
#'
# @inheritParams recipes::step_other
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param threshold A numeric value between 0 and 1, or an integer greater or
#'   equal to one.  If less than one, then factor levels with a rate of
#'   occurrence in the training set below `threshold` will be pooled to `other`.
#'   If greater or equal to one, then this value is treated as a frequency and
#'   factor levels that occur less than `threshold` times will be pooled to
#'   `other`.
#' @param other A single character value for the other category, defaults to
#' `"other_ml"`. 
#' @param single_low_level character controlling handling of a single low 
#' rate/frequency class. Defaults to 'as-is', where data is unmodified if only 
#' a single level meets the criterion for pooling. This is different from 
#' `recipes::step_other()`'s behavior ('rename'), where the low rate/frequency 
#' class would not be pooled with other classes but renamed to `other`
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [bake()]? While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)). Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.

#' @param objects A list of objects that contain the information to pool
#'   infrequent levels that is determined by [prep()].
#' @template recipe-step-return
# @family dummy variable and encoding steps
# @seealso [dummy_names()]
#' @export
#' @author This step is based on 
#' [`recipes::step_other()`](https://github.com/tidymodels/recipes/blob/d269758cb171698f38f376fcd711de941840657f/R/other.R#L1)
#' with only minor modifications.
#'  
#' @details
#'
#' The overall proportion (or total counts) of the categories are computed. The
#' `other` category is used to pool any two or more categorical levels whose individual
#' proportion (or frequency) in the training set is less than `threshold`.
#'
#' If no pooling is done the data are unmodified (although character data may be
#' changed to factors based on the value of `strings_as_factors` in [prep()]).
#' Otherwise, a factor is always returned with different factor levels.
#'
#' If `threshold` is less than the largest category proportion, all levels
#' except for the most frequent are collapsed to the `other` level.
#'
#' If `other_ml` is in the list of discarded levels, no error occurs.
#'
#' If no pooling is done, novel factor levels are converted to missing. If
#' pooling is needed, they will be placed into the other_ml category.
#'
#' When data to be processed contains novel levels (i.e., not contained in the
#' training set), the other category is assigned.
#' 
#' # Differences to step_other()
#' - a single class subject to lumping is kept as-is as opposed to renamed 
#' to `other` (during prep)
#' - If the level defined in `other` is an original class level that was not 
#' subject to pooling, 
#' the user is informed, but no error is raised.
#' # TODO check message in test case
#' - novel factor levels are not pooled with an existing lumped category (during bake)
#'
#' # Tidying
#'
#' When you [`recipes::tidy()`][recipes::tidy.recipe()] this step, 
#' a tibble is returned with
#' columns `terms`, `retained` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{retained}{character, factor levels not pulled into `other`}
#'   \item{id}{character, id of this step}
#' }
#'
#' @inheritSection step_log_skewness Case weights

#' @template recipe-case-weights-unsupervised
#'

step_other2 <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    threshold = .05,
    other = "other_ml",
    single_low_level = c("as-is", "rename"),
    objects = NULL,
    skip = FALSE,
    id = recipes::rand_id("other2")
  ) {
    recipes::add_step(
      recipe,
      step_other2_new(
        terms = rlang::enquos(...),
        role = role,
        trained = trained,
        threshold = threshold,
        other = other,
        single_low_level = single_low_level,
        objects = objects,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_other2_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    other,
    single_low_level,
    objects,
    skip,
    id,
    case_weights
  ) {
    recipes::step(
      subclass = "other2",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      other = other,
      single_low_level = single_low_level,
      objects = objects,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @exportS3Method 
prep.step_other2 <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (!is.numeric(x$threshold)) {
    cli::cli_abort(
      "{.arg threshold} should be a single numeric value {.obj_type_friendly {x$threshold}}"
    )
  }

  # check according to user defined threshold as proportion or count
  if (x$threshold >= 1) {
    # check_number_whole(x$threshold, arg = "threshold", min = 1)
    do.call(
      utils::getFromNamespace("check_number_whole", "rlang"),
      list(x = x$threshold, arg = "threshold", min = 1)
    )
  } else {
    #check_number_decimal(x$threshold, arg = "threshold", min = 0)
    do.call(
      utils::getFromNamespace("check_number_decimal", "rlang"),
      list(x = x$threshold, arg = "threshold", min = 0)
    )
  }

  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  # objects <- lapply(
  #   training[, col_names],
  #   keep_levels,
  #   threshold = x$threshold,
  #   other = x$other,
  #   wts = wts
  # )
  objects <- purrr::imap(
    training[, col_names],
    ~ keep_levels(
      .x,
      threshold = x$threshold,
      other = x$other,
      wts = wts,
      name_x = .y
    )
  )

  step_other2_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    other = x$other,
    single_low_level = x$single_low_level,
    objects = objects,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_other2 <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  recipes::check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    if (!object$objects[[col_name]]$collapse) {
      next
    }
    tmp <- new_data[[col_name]]

    if (!is.character(tmp)) {
      tmp <- as.character(tmp)
    }

    tmp <- ifelse(
      !(tmp %in% object$objects[[col_name]]$keep) & !is.na(tmp),
      object$objects[[col_name]]$other,
      tmp
    )

    # assign other factor levels other here too.
    tmp <- factor(
      tmp,
      levels = c(
        object$objects[[col_name]]$keep,
        object$objects[[col_name]]$other
      ) %>% unique()
    )

    new_data[[col_name]] <- tmp
  }

  new_data
}

#' @exportS3Method 
print.step_other2 <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collapsing factor levels for "
    if (x$trained) {
      columns <- purrr::map_lgl(x$objects, \(.x) .x$collapse)
      columns <- names(columns)[columns]
    } else {
      columns <- names(x$objects)
    }
    recipes::print_step(
      columns,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

keep_levels <- function(
  x,
  threshold = .1,
  other = "other_ml",
  single_low_level = c("as-is", "rename"),
  wts = NULL,
  name_x = NULL,
  call = rlang::caller_env(2)
) {
  if (!is.factor(x)) {
    x <- factor(x)
  }

  single_low_level <- rlang::arg_match(single_low_level, c("as-is", "rename"))
  #xtab <- sort(weighted_table(x, wts = wts), decreasing = TRUE)
  xtab_raw <- do.call(
    utils::getFromNamespace("weighted_table", "recipes"),
    list(x, wts = wts)
  )
  xtab <- sort(xtab_raw, decreasing = TRUE)

  if (threshold < 1) {
    if (is.null(wts)) {
      xtab <- xtab / sum(!is.na(x))
    } else {
      xtab <- xtab / sum(as.double(wts)[!is.na(x)])
    }
  }

  dropped <- which(xtab < threshold)
  orig <- levels(x)
  min_count_drop <- ifelse(single_low_level == 'as-is', 1, 0)
  
  if (length(dropped) > min_count_drop) {
    keepers <- names(xtab[-dropped])
  } else {
    keepers <- orig
  }

  if (length(keepers) == 0) {
    keepers <- names(xtab)[which.max(xtab)]
  }

  if (other %in% keepers) {
    cli::cli_inform(paste(
      "The level {.code other} is already a factor level", 
      ifelse(!is.null(name_x), paste("in variable", name_x), ""),
      "that will be retained. \\
      Please adjust your data set accordingly using a different", 
      "value if you prefer to keep classes separate."),
      call = call
    )
  }

  list(
    keep = orig[orig %in% keepers],
    collapse = length(dropped) > min_count_drop,
    other = other
  )
}

#' @exportS3Method 
tidy.step_other2 <- function(x, ...) {
  if (recipes::is_trained(x)) {
    values <- purrr::map(x$objects, function(x) x$keep)
    n <- vapply(values, length, integer(1))
    values <- vctrs::list_unchop(
      values,
      ptype = character(),
      name_spec = rlang::zap()
    )
    res <- tibble::tibble(
      terms = rep(names(n), n),
      retained = values
    )
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      retained = rep(rlang::na_chr, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @exportS3Method 
tunable.step_other2 <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold", range = c(0, 0.1))
    ),
    source = "recipe",
    component = "step_other2",
    component_id = x$id
  )
}
