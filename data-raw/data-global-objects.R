
spec_cols_required <- list(
  adsl = c("id"),
  bds  = c("id", "value", "param"),
  occds = c("id", "label")
)

usethis::use_data(spec_cols_required, overwrite = TRUE)
