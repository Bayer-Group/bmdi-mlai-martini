

adam_guess <- function(file, key){
  
  file_info <- adam_domain_type(file)
  
  if (file_info$type == "none"){
    usethis::ui_stop(
      paste0("No guessing options available for '", file_info$dom,
             "'. Parameter '", key, "' needs to be provided.\n")
    )
  }
  
  # add guessing options
  
}