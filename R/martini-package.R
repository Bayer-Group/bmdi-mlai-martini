#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @importFrom stats na.omit na.exclude quantile as.formula
## usethis namespace: end
utils::globalVariables(c(
  "domain", "type", "param", "label", "value", "name",
  ".id", ".trt", ".out", ".time", ".status", ".",
  "column", "selected", ".key",
  "any_na", "aval", "min_aval", "n_dist", "paramcd", "skew",
  "RANDDT", 'RANDNO',
  ".strata", "number",
  "new_name", "old_name", "n"
))
