
library(tidyverse)
library(labelled)

set.seed(1746)

n <- 20


# ADSL ####

age_breaks <- c(-Inf, 60, 75, Inf)
age_levs   <- c('< 60' = '[-Inf,60)', '60 - <75' = '[60,75)', '>=75' = '[75, Inf)'  )

race_levs  <- c("WHITE", "BLACK", "ASIAN")

adsl <- tibble(
  # constants
  ADSNAME  = "ADSL",
  STUDYID  = 1909,
  
  # study ID plus redundant
  SUBJID   = as.character(10000 + 1:n),
  USUBJID  = paste0(STUDYID, SUBJID),
  
  # Flag
  ITTFL    = sample(c("Y", NA), size = n, replace = TRUE, prob = c(0.9, 0.1)),
  
  # TRT to choose
  TRT01P   = sample(c("PLC", "TRT"), size = n, replace = TRUE),
  TRT01A   = TRT01P,
  
  # date
  RANDDT   = as.Date("1909-12-19") + sample(1:1000, size = n, replace = TRUE),
  
  # empties
  EMPTYC   = NA_character_,
  EMPTYN   = NA_real_,
  
  # numeric
  AGE      = sample(45:92, size = n, replace = TRUE)
) %>% 
  
# factors with numeric coding
mutate(
  # ... SEX
  SEX      = sample(c("M", "F"), size = n, replace = TRUE),
  SEXN     = factor(SEX) %>%  fct_relevel('M') %>%  as.integer(),
  
  # ...RACE
  RACE     = sample(c(race_levs, NA_character_), 
                    size = n, replace = TRUE, prob = c(0.7, 0.1, 0.15, 0.05)),
  RACEN    = factor(RACE, levels = race_levs) %>%   as.integer()
) %>% 
  
# ... AGE  
mutate(
  AGEGR01  = cut(AGE, breaks = age_breaks, right = FALSE),
  AGEGR01N = as.numeric(AGEGR01) %>%  as.integer(),
  .after   = AGE
) %>%  
mutate_at('AGEGR01',  ~ fct_recode(.x, !!! age_levs ) %>%  as.character() ) %>%  

# combined column
unite(UASR, USUBJID, AGE, RACE, SEX, sep='/', remove = FALSE)



# define labels ####
adsl_labels <- list(
  ADSNAME  = "Dataset Name",
  STUDYID  = "Study Identifier",
  SUBJID   = "Subject Identifier for the Study",
  USUBJID  = "Unique Subject Identifier",
  ITTFL    = "Intent-To-Treat Population Flag",
  TRT01A   = "Actual Treatment for Period 01",
  TRT01P   = "Planned Treatment for Period 01",
  RANDDT   = "Date of Randomization",
  EMPTYC   = NULL,
  EMPTYN   = NULL,
  AGE      = "Age",
  AGEGR01  = "Age Group 01",
  AGEGR01N = "Age Group 01 (N)",
  SEX      = "Sex",
  SEXN     = "Sex (N)",
  RACE     = "Race",
  RACEN    = "Race (N)",
  UASR     = "Unique Subject Identifier/Age/Sex/Race"
)


adsl <- adsl %>% 
  set_variable_labels( .labels = adsl_labels)

# view(adsl)
