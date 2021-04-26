
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
    
    # ... RACE
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
  unite(UASR, USUBJID, AGE, RACE, SEX, sep = '/', remove = FALSE)
  
  
  
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
  EMPTYC   = "",
  EMPTYN   = "",
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

adsl_labels <- adsl_labels %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")



write_csv(adsl,        "data/adsl.csv", na = "")
write_csv(adsl_labels, "data/adsl_labels.csv")



# view(adsl)

# BDS ####

# ... main version ####

n_param <- 3
n_tp    <- 3

n_id <- 5

# create grid
subjid  <- as.character(10000 + 1:n_id)
paramcd <- paste0("LAB", 1:n_param)
avisit  <- paste0("Visit ", 1:n_tp)

bds_str <- expand_grid(subjid, paramcd, avisit)
colnames(bds_str) <- colnames(bds_str) %>% str_to_upper()
bds_str <- bds_str %>% bind_rows(
  bds_str %>% 
    filter(
      PARAMCD == "LAB2",
      AVISIT  == "Visit 1"
    )
) %>% 
  arrange(SUBJID, PARAMCD, AVISIT)

adlb <- bds_str %>% 
  mutate(ADSNAME = "ADLB", .before = 1) %>% 
  mutate(STUDYID = 1909,   .before = 2) %>% 
  mutate(AVAL = sample(1:5, size = n(), replace = TRUE)) %>% 
  group_by(SUBJID, PARAMCD) %>% 
  mutate(BASE = first(AVAL)) %>% 
  mutate(ABLFL = if_else(row_number() == 1, "Y", "")) %>% 
  ungroup() %>% 
  mutate(
    CHG   = AVAL - BASE,
    PARAM = str_replace(PARAMCD, "LAB", "Laboratory "),
    AVALU = str_replace(PARAMCD, "LAB", "unit")
  ) %>% 
  relocate(PARAM, .after = PARAMCD) %>% 
  set_variable_labels(., .labels = colnames(.))

adlb_labels <- var_label(adlb) %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")



write_csv(adlb,        "data/adlb.csv", na = "")
write_csv(adlb_labels, "data/adlb_labels.csv")

# ... alternative version 1 (missing values) ####

adlb_miss <- adlb %>% 
  group_by(SUBJID) %>% 
  slice(-sample(1:n(), size = 1)) %>% 
  ungroup()

write_csv(adlb_miss,   "data/adlb_miss.csv", na = "")
write_csv(adlb_labels, "data/adlb_miss_labels.csv")

# ... alternative version 2 (non-standard column names) ####

adlb_rename <- adlb %>% 
  mutate(AVAL = as.character(AVAL)) %>% 
  rename(MARTINI = PARAMCD, LBSTRESC = AVAL) %>% 
  set_variable_labels(., .labels = colnames(.))

adlb_rename_labels <- var_label(adlb_rename) %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")

write_csv(adlb_rename,        "data/adlb_rename.csv", na = "")
write_csv(adlb_rename_labels, "data/adlb_rename_labels.csv")






