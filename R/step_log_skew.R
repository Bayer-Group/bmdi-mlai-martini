#' Logarithmic transformation based on skewness
#'
#' `step_log()` creates a *specification* of a recipe step that will log
#' transform numericvariables if the skewness exceeds a given threshold.
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
#' @param signed A logical indicating whether to take the signed log. This is
#'   `sign(x) * log(abs(x))` when `abs(x) => 1` or `0 if abs(x) < 1`. If `TRUE`
#'   the `offset` argument will be ignored.
#'
#' @template recipe-step-return
#' 
#' @author  Modified from recipes.
#' 
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
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
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' log_trans <- rec |>
#'   step_log_skew(all_numeric_predictors(), skewness = 1)
#'
#' log_obj <- prep(log_trans, training = examples)
#'
#' transformed_te <- bake(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' plot(examples$V2, transformed_te$V2)
#'
#' @seealso [recipes::step_log()]
#' @export
step_log_skew <-
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
    signed = FALSE,
    id = recipes::rand_id("log_skew")
  ) {
    recipes::add_step(
      recipe,
      step_log_skew_new(
        terms = rlang::enquos(...),
        role = role,
        trained = trained,
        skewness = skewness,
        base = base,
        offset = offset,
        columns = columns,
        skip = skip,
        signed = signed,
        id = id
      )
    )
  }

step_log_skew_new <-
  function(
    terms, 
    role, 
    trained, 
    skewness, 
    base, 
    offset, 
    columns, 
    skip, 
    signed, 
    id
  ) {
    recipes::step(
      subclass = "log_skew",
      terms = terms,
      role = role,
      trained = trained,
      skewness = skewness,
      base = base,
      offset = offset,
      columns = columns,
      skip = skip,
      signed = signed,
      id = id
    )
  }

#' @exportS3Method 
prep.step_log_skew <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes:::check_type(training[, col_names], types = c("double", "integer"))
  recipes:::check_number_decimal(x$offset, arg = "offset")
  recipes:::check_bool(x$signed, arg = "signed")
  recipes:::check_number_decimal(x$base, arg = "base", min = 0)
  
  if (!is.null(x$skewness)){
    
    is_skewed <- logical(length(col_names)) %>% purrr::set_names(col_names)
    for (col_name in col_names) {
      is_skewed[col_name] <- skw(training[[col_name]], na.rm = TRUE) > x$skewness
    }
    col_names <- col_names[unname(is_skewed)]
    
  }
  
  step_log_skew_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    skewness = x$skewness,
    base = x$base,
    offset = x$offset,
    columns = col_names,
    skip = x$skip,
    signed = x$signed,
    id = x$id
  )
}

#' @exportS3Method 
bake.step_log_skew <- function(object, new_data, ...) {
  # For backward compatibility #1284
  col_names <- names(object$columns) %||% object$columns
  recipes:::check_new_data(col_names, object, new_data)
  
  # for backward compat
  if (all(names(object) != "offset")) {
    object$offset <- 0
  }
  
  if (object$signed && object$offset != 0) {
    cli::cli_warn("When {.arg signed} is TRUE, {.arg offset} will be ignored.")
  }
  
  for (col_name in col_names) {
    tmp <- new_data[[col_name]]
    
    if (object$signed) {
      tmp <- ifelse(
        abs(tmp) < 1,
        0,
        sign(tmp) * log(abs(tmp), base = object$base)
      )
    } else {
      tmp <- log(tmp + object$offset, base = object$base)
    }
    
    new_data[[col_name]] <- tmp
  }
  
  new_data
}

#' @exportS3Method 
print.step_log_skew <-
  function(x, width = max(20, options()$width - 31), ...) {
    msg <- ifelse(x$signed, "Signed log", "Log")
    title <- glue::glue("{msg} transformation on ")
    recipes:::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @exportS3Method 
tidy.step_log_skew <- function(x, ...) {
  out <- recipes:::simple_terms(x, ...)
  out$base <- x$base
  out$id <- x$id
  out
}