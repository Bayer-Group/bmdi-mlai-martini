

adam_guess <- function(file){
  
  file_info <- adam_domain_type(file)
  
  if (file_info$type == "none"){
    usethis::ui_stop(
      paste0("No guessing options available for domain '", file_info$dom, "'\n")
    )
  }
  
  # add guessing options
  
}