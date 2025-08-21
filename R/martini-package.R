#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom recipes bake
#' @importFrom recipes prep
#' @importFrom recipes tidy
#' @importFrom recipes tunable
#' @importFrom rlang %||%
#' @importFrom stats na.omit na.exclude quantile as.formula
#' @importFrom utils head tail
## usethis namespace: end
utils::globalVariables(c(
  "domain", "type", "param", "label", "value", "name", "spec_id",
  ".id", ".trt", ".out", ".time", ".status", ".",
  "column", "selected", ".key",
  "lab", "lev",
  "any_na", "aval", "min_aval", "n_dist", "paramcd", "skew",
  "RANDDT", 'RANDNO',
  ".strata", "number",
  "guess", "var",
  "new_name", "old_name", "n",
  "txt", "data_id", 
  "file_ext_fct",
  'r', 'role', 'variable', 'x', 'y',
  'label_x', 'label_y', 'label2', 'label3',
  "dupl_ctrl", "recipe_state"
))
