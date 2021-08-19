
library(tidyverse)
library(labelled)

set.seed(1909)

n <- 320

# ADSL -----


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
  TRT01PN  = factor(TRT01P) %>%  fct_relevel("PLC") %>% as.integer(),
  TRT01A   = TRT01P,
  TRT01AN  = factor(TRT01A) %>%  fct_relevel("PLC") %>% as.integer(),
  
  # date
  RANDDT   = as.Date("1909-12-19") + sample(1:1000, size = n, replace = TRUE),

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
  TRT01P   = "Planned Treatment for Period 01",
  TRT01PN  = "Planned Treatment for Period 01 (N)",
  TRT01A   = "Actual Treatment for Period 01",
  TRT01AN  = "Actual Treatment for Period 01 (N)",
  RANDDT   = "Date of Randomization",
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

write_csv(adsl,        "dev/data/prep_pkg_study/adsl.csv", na = "")
write_csv(adsl_labels, "dev/data/prep_pkg_study/adsl_labels.csv")

# ADVS ----

r <- tribble(
  ~BMI,  ~BPDIA, ~HR,    ~BPSYS, ~WEIGHT,
  1.000, 0.196,  0.0875, 0.207,  0.860,
  0.196, 1.000,  0.194,  0.581,  0.256,
  0.0875,0.194,  1.000,  0.0546, 0.0723,
  0.207, 0.581,  0.0546, 1.000,  0.148,
  0.860, 0.256,  0.0723, 0.148,  1.000
) %>% 
  as.matrix()

rownames(r) <- colnames(r)

m <-  c(27.96347,  71.46838,  69.79157, 118.44496,  82.30187)
names(m)  <- colnames(r)

s <-  c(4.917071, 10.567947, 11.734989, 14.950894, 17.543476)
names(s)  <- colnames(r)

si <- c(NA, 6.768732, 6.795291, 9.427696, NA)
names(si) <- colnames(r)



advs_pre <- MASS::mvrnorm(n = n, mu = m, Sigma = r * (s %x% t(s))) %>% 
  as_tibble() %>% 
  bind_cols(adsl %>% select(SUBJID)) %>% 
  pivot_longer(-SUBJID, names_to = "PARAMCD", values_to = "Baseline") %>% 
  arrange(SUBJID, PARAMCD)


advs_pre_sub <- advs_pre %>% 
  filter(PARAMCD %in% names(na.exclude(si))) %>% 
  left_join(
    si %>% 
      na.exclude() %>% 
      enframe("PARAMCD", "si"),
    by = "PARAMCD"
  ) %>% 
  mutate(`Visit 2` = Baseline  + rnorm(1, sd = si)) %>% 
  mutate(`Visit 3` = `Visit 2` + rnorm(1, sd = si)) %>% 
  select(-si, -Baseline)

advs_info <- tribble(
  ~PARAMCD, ~PARAM, ~AVALU,
  "BMI",    "Body Mass Index",         "kg/m2",
  "BPDIA",  "Diastolic Blood Pressure", "mmHg",
  "BPSYS",  "Systolic Blood Pressure",  "mmHg",
  "HR",     "Heart Rate",               "beats/min",
  "WEIGHT", "Weight",                   "kg"
)

advs <- advs_pre %>% 
  left_join(advs_pre_sub, by = c("SUBJID", "PARAMCD")) %>% 
  pivot_longer(
    cols           = -c("SUBJID", "PARAMCD"),
    names_to       = "AVISIT",
    values_to      = "AVAL",
    values_drop_na = TRUE
  ) %>% 
  left_join(advs_info, by = "PARAMCD") %>% 
  left_join(adsl %>% select(STUDYID, SUBJID, USUBJID, ITTFL), by = "SUBJID") %>% 
  mutate(AVAL = case_when(
    PARAMCD == "BMI"    ~ round(AVAL, 1),
    PARAMCD == "WEIGHT" ~ round(AVAL, 1),
    TRUE                ~ round(AVAL, 0)
  )) %>% 
  mutate(AVAL = case_when(
    AVISIT != "Baseline" & ITTFL != "Y" ~ NA_real_,
    TRUE                                ~ AVAL
  )) %>% 
  na.exclude() %>% 
  select(-ITTFL) %>% 
  slice_sample(prop = .99) %>% 
  relocate(STUDYID, USUBJID, SUBJID, PARAMCD, PARAM, AVAL, AVALU, AVISIT, .before = 1) %>% 
  mutate(AVISITN = factor(AVISIT) %>% as.integer()) %>% 
  arrange(SUBJID, PARAMCD, AVISITN) %>% 
  group_by(SUBJID, PARAMCD) %>% 
  nest() %>% 
  mutate(BASE = map(data, ~{
    .x %>% filter(AVISITN == 1) %>% pull(AVAL)
  })) %>% 
  unnest(BASE) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(CHG    = AVAL - BASE) %>% 
  mutate(R2BASE = AVAL / BASE) %>% 
  mutate(ADSNAME = "ADVS", .before = 1)


# define labels ####
advs_labels <- list(
  ADSNAME  = "Dataset Name",
  STUDYID  = "Study Identifier",
  SUBJID   = "Subject Identifier for the Study",
  USUBJID  = "Unique Subject Identifier",
  PARAMCD  = "Parameter Code",
  PARAM    = "Parameter",
  AVAL     = "Analysis Value",
  AVALU    = "Analysis Unit",
  AVISIT   = "Analysis Visit",
  AVISITN  = "Analysis Visit (N)",
  BASE     = "Baseline Value",
  CHG      = "Change from Baseline",
  R2BASE   = "Ratio to Baseline"
)

advs <- advs %>% 
  set_variable_labels( .labels = advs_labels)

advs_labels <- advs_labels %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")

write_csv(advs,        "dev/data/prep_pkg_study/advs.csv", na = "")
write_csv(advs_labels, "dev/data/prep_pkg_study/advs_labels.csv")