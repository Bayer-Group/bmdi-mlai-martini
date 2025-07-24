
library(tidyverse)

files_sas <- fs::dir_info("inst/martini_example_study/ads/") %>% pull(path)

path_out <- "inst/martini_example_study_rds/ads"

fs::dir_create(path_out)



walk(files_sas, ~{
  
  name <- tools::file_path_sans_ext(.x) %>% basename()
  
  data <- haven::read_sas(.x)
  
  if (name == "advs") {
    
    set.seed(2107)
    
    visit_times <- seq(
      as.POSIXct("2023/03/03 00:00:00"), 
      as.POSIXct("2023/04/05 00:00:00"), 
      by = "min"
    )
    
    data <- left_join(
      data,
      data %>% 
        distinct(SUBJID, AVISITN) %>% 
        arrange(SUBJID, AVISITN) %>% 
        group_split(SUBJID) %>% 
        map(~{
          .x %>% 
            mutate(
              VSDT = sample(visit_times, size = 1) + 
                as.difftime(seq(0, n()-1, 1)*30+runif(n = n()), units = "days")
            )
        }) %>% 
        list_rbind(),
      by = c("SUBJID", "AVISITN")
    )
    
    data <- list(
      data %>% 
        mutate(ANL01FL = "Y"),
      data %>% 
        filter(PARAMCD == "HR", AVISIT == "Baseline") %>% 
        mutate(
          AVAL = round(AVAL + rnorm(n(), sd = 0.15*sd(AVAL, na.rm = TRUE))),
          VSDT = VSDT - as.difftime(runif(n = n(), min = 1, max = 3), units = "hours")
        ),
      data %>% 
        filter(PARAMCD == "HR", AVISIT == "Baseline") %>% 
        mutate(
          AVAL = round(AVAL + rnorm(n(), sd = 0.15*sd(AVAL, na.rm = TRUE))),
          VSDT = VSDT - as.difftime(runif(n = n(), min = 1, max = 3), units = "hours")
        )
    ) %>% 
      list_rbind() %>% 
      mutate(AVISIT = if_else(AVISIT == "Baseline", "Visit 1", AVISIT)) %>% 
      arrange(USUBJID, PARAMCD, AVISITN, VSDT)
    
    
  }
  
  write_rds(data, file = file.path(path_out, paste0(name, ".rds")))
  
})

