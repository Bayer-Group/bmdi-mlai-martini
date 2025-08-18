#' Logarithmic transformation based on skewness
#'
#' `step_log_skewness()` creates a *specification* of a recipe step that will log
#' transform numeric variables if the skewness exceeds a given threshold.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [selections()] for more details.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param columns A character vector of the variable names that are 
#'   log-transformed. This field is a placeholder and will be populated once 
#'   [prep()] is used.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [bake()]? While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)). Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @param skewness Numeric threshold for the skewness. If the skewness of a 
#' variable exceeds this threshold, it will be log-transformed. Otherwise, 
#' it will remain as-is. If `NULL`, all selected numeric variables will be 
#' transformed.
#' @param base A numeric value for the base.
#' @param offset An optional value to add to the data prior to logging (to avoid
#'   `log(0)`).
#'
#' @template recipe-step-return
#' 
#' @author  Modified from [recipes::step_log()].
#' @seealso [recipes::step_log()]
#' 
#' @export
#' 
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `base` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{base}{numeric, value for the base}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template recipe-case-weights-not-supported
#'
#' @examples
#' set.seed(313)
#' examples <- matrix(exp(rnorm(40)), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipes::recipe(~ V1 + V2, data = examples)
#'
#' log_trans <- rec |>
#'   step_log_skewness(recipes::all_numeric_predictors(), skewness = 1)
#'
#' log_obj <- recipes::prep(log_trans, training = examples)
#'
#' transformed_te <- recipes::bake(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' plot(examples$V2, transformed_te$V2)
step_log_skewness <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    skewness = NULL,
    base = exp(1),
    offset = 0,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("log_skewness")
  ) {
    recipes::add_step(
      recipe,
      step_log_skewness_new(
        terms = rlang::enquos(...),
        role = role,
        trained = trained,
        skewness = skewness,
        base = base,
        offset = offset,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_log_skewness_new <-
  function(
    terms, 
    role, 
    trained, 
    skewness, 
    base, 
    offset, 
    columns, 
    skip, 
    id
  ) {
    recipes::step(
      subclass = "log_skewness",
      terms = terms,
      role = role,
      trained = trained,
      skewness = skewness,
      base = base,
      offset = offset,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @exportS3Method 
prep.step_log_skewness <- function(x, training, info = NULL, ...) {
  
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  #recipes::check_type(training[, col_names], types = c("double", "integer"))
  do.call(
    utils::getFromNamespace("check_type", "recipes"),
    list(dat = training[, col_names], types = c("double", "integer"))
  )
  #recipes:::check_number_decimal(x$offset, arg = "offset")
  do.call(
    utils::getFromNamespace("check_number_decimal", "recipes"),
    list(x = x$offset, arg = "offset")
  )
  #recipes:::check_number_decimal(x$base, arg = "base", min = 0)
  do.call(
    utils::getFromNamespace("check_number_decimal", "recipes"),
    list(x = x$base, arg = "base", min = 0)
  )
  
  if (!is.null(x$skewness)){
    
    is_skewed <- logical(length(col_names)) %>% purrr::set_names(col_names)
    for (col_name in col_names) {
      is_skewed[col_name] <- isTRUE(skw(training[[col_name]], na.rm = TRUE) > x$skewness)
    }
    col_names <- col_names[unname(is_skewed)]
    
  }
  
  step_log_skewness_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    skewness = x$skewness,
    base = x$base,
    offset = x$offset,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @exportS3Method 
bake.step_log_skewness <- function(object, new_data, ...) {
  # For backward compatibility #1284
  col_names <- names(object$columns) %||% object$columns
  recipes::check_new_data(col_names, object, new_data)
  
  # for backward compatibility
  if (all(names(object) != "offset")) {
    object$offset <- 0
  }
  
  # TODO add check if log can be applied and exclude variables if not possible
  
  for (col_name in col_names) {
    tmp <- new_data[[col_name]]
    new_data[[col_name]] <- log(tmp + object$offset, base = object$base)
  }
  
  new_data
}

#' @exportS3Method 
print.step_log_skewness <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Log transformation on "
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @exportS3Method 
tidy.step_log_skewness <- function(x, ...) {
  out <- do.call(
    utils::getFromNamespace("simple_terms", "recipes"), 
    list(x = x, rlang::dots_list())
  )
  out$base <- x$base
  out$id <- x$id
  out
}

#' Undoing logarithmic transformations based on skewness
#'
#' `step_log_skewness_undo()` creates a *specification* of a recipe step that 
#' will reverse any log transformations that are done by [step_log_skewness()].
#'
#' @inheritParams step_log_skewness
#' @param id_undo id of the corresponding [step_log_skewness()] to reverse the 
#' log transformation on 
#' 
#' @inherit step_log_skewness return author seealso
#' @inheritSection step_log_skewness Case weights
#' @inheritSection step_log_skewness Tidying
#' 
#' @export

step_log_skewness_undo <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    base = NULL,
    offset = NULL,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("log_skewed_undo"),
    id_undo = NULL
) {
  
  recipe_tidy <- recipes::tidy(recipe)
  
  # check 'id_undo', if provided
  if (!is.null(id_undo)) {
    
    if (!id_undo %in% recipe_tidy$id) {
      cli::cli_abort(c(
        "!" = "{.code id_undo = {id_undo}} is not an id of a previous recipe step."))
    }
    
    type_id_undo <- recipe_tidy %>% 
      dplyr::filter(id == id_undo) %>% 
      dplyr::pull(type)
    
    if (type_id_undo != "log_skewness") {
      cli::cli_abort(c(
        "!" = "The recipe step with {.code id_undo = {id_undo}} is not of type 'log_skewness'."))
    }
    
  }
  
  columns_logged <- character(0)
  if (any("log_skewness" %in% recipe_tidy$type)) {
    
    # identify number of specified log step ...
    number_log_step <- if (!is.null(id_undo)) {
      recipe_tidy %>% 
        dplyr::filter(id == id_undo) %>%
        dplyr::pull(number)
    } else {
      # ... or take the last one
      recipe_tidy %>%
        dplyr::filter(type == "log_skewness") %>% 
        dplyr::pull(number) %>% 
        tail(1)
    }
    
    recipe_prepared_step <- recipes::prep(recipe) %>% 
      magrittr::extract2("steps") %>% 
      magrittr::extract2(number_log_step)
    
    columns_logged <- recipe_prepared_step$columns
    
    base   <- recipe_prepared_step$base
    offset <- recipe_prepared_step$offset
    
  }
  
  recipes::add_step(
    recipe,
    step_log_skewness_undo_new(
      terms = rlang::enquos(...),
      role = role,
      trained = trained,
      base = base,
      offset = offset,
      columns = columns_logged,
      skip = skip,
      id = id
    )
  )
}

step_log_skewness_undo_new <-
  function(
    terms, 
    role, 
    trained, 
    base,
    offset,
    columns, 
    skip, 
    id
  ) {
    recipes::step(
      subclass = "log_skewness_undo",
      terms = terms,
      role = role,
      trained = trained,
      base = base,
      offset = offset,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @exportS3Method 
prep.step_log_skewness_undo <- function(x, training, info = NULL, ...) {
  
  # if columns have been removed after log trafo, silently skip them for back transformation
  #col_names <- recipes::recipes_eval_select(unname(x$columns), training, info)
  
  # currently, missing vars are skipped silently
  # logged_but_gone <- unname(x$columns) %>% setdiff(names(training))
  # if (length(logged_but_gone) > 0) {
  #   cli::cli_inform(c(
  #     "{.fn step_log_skewness_undo} is supposed to reverse the log transformation of {.fn step_log_skewness}.",
  #     "i" = "The variable{?s} {logged_but_gone} {?was/were} removed in the mean time."
  #   ))
  # }
  col_names <- recipes::recipes_eval_select(
    unname(x$columns) %>% intersect(names(training)),
    training,
    info
  )
  
  recipes::check_type(training[, col_names], types = c("double", "integer"))
  do.call(
    utils::getFromNamespace("check_number_decimal", "recipes"),
    list(x = x$offset, arg = "offset")
  )
  do.call(
    utils::getFromNamespace("check_number_decimal", "recipes"),
    list(x = x$base, arg = "base", min = 0)
  )
  
  step_log_skewness_undo_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    base = x$base,
    offset = x$offset,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @exportS3Method 
bake.step_log_skewness_undo <- function(object, new_data, ...) {
  # For backward compatibility #1284
  col_names <- names(object$columns) %||% object$columns
  recipes::check_new_data(col_names, object, new_data)
  
  for (col_name in col_names) {
    tmp <- new_data[[col_name]]
    new_data[[col_name]] <- object$base**tmp - object$offset
  }
  
  new_data
}

#' @exportS3Method 
print.step_log_skewness_undo <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Log transformation reversed on "
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @exportS3Method 
tidy.step_log_skewness_undo <- function(x, ...) {
  out <- do.call(
    utils::getFromNamespace("simple_terms", "recipes"), 
    list(x = x, rlang::dots_list())
  )
  out$base <- x$base
  out$id <- x$id
  out
}

