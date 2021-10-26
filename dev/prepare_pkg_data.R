
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
  STUDYID  = 17501,
  
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


# ADLB ----

r <- structure(c(1, -0.158, -0.043, 0.264, 0.206, 0.15, 0.137, -0.002, 
            0.205, -0.04, 0.036, -0.158, 1, 0.066, -0.224, -0.177, -0.314, 
            -0.14, 0.268, 0, -0.101, 0.307, -0.043, 0.066, 1, -0.044, 0.001, 
            -0.053, -0.004, -0.112, -0.025, -0.173, 0.224, 0.264, -0.224, 
            -0.044, 1, 0.939, 0.056, 0.163, 0.018, 0.038, 0.066, -0.033, 
            0.206, -0.177, 0.001, 0.939, 1, 0.003, 0.134, 0.047, 0.056, 0.081, 
            -0.014, 0.15, -0.314, -0.053, 0.056, 0.003, 1, 0.22, 0.018, 0.048, 
            0.078, -0.192, 0.137, -0.14, -0.004, 0.163, 0.134, 0.22, 1, 0.14, 
            0.055, 0.01, -0.036, -0.002, 0.268, -0.112, 0.018, 0.047, 0.018, 
            0.14, 1, 0.032, 0.035, -0.018, 0.205, 0, -0.025, 0.038, 0.056, 
            0.048, 0.055, 0.032, 1, -0.184, -0.095, -0.04, -0.101, -0.173, 
            0.066, 0.081, 0.078, 0.01, 0.035, -0.184, 1, -0.095, 0.036, 0.307, 
            0.224, -0.033, -0.014, -0.192, -0.036, -0.018, -0.095, -0.095, 
            1), .Dim = c(11L, 11L), .Dimnames = list(c("CALCIUM", "CREAT", 
                                                       "GGT", "HB", "HCT", "HDL", "LDL", "MAGNES", "POTASS", "SODIUM", 
                                                       "URICAC"), c("CALCIUM", "CREAT", "GGT", "HB", "HCT", "HDL", "LDL", 
                                                                    "MAGNES", "POTASS", "SODIUM", "URICAC")))


m <-  c(2.25, 0.184, 3.685, 2.608, 3.719, 3.811, 4.375, 0.769, 1.476, 4.931, 2.002)
names(m)  <- colnames(r)

s <- c(0.047, 0.302, 0.874, 0.124, 0.12, 0.323, 0.389, 0.126, 0.105, 0.021, 0.298)
names(s)  <- colnames(r)

si <- c(0.028, 0.101, 0.185, 0.039, 0.045, 0.1, 0.145, 0.064, 0.069, 0.013, 0.111)
names(si) <- colnames(r)


adlb_pre <- MASS::mvrnorm(n = n, mu = m, Sigma = r * (s %x% t(s))) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, exp) %>% 
  bind_cols(adsl %>% select(SUBJID)) %>% 
  pivot_longer(-SUBJID, names_to = "PARAMCD", values_to = "Baseline") %>% 
  arrange(SUBJID, PARAMCD)


adlb_pre_sub <- adlb_pre %>% 
  filter(PARAMCD %in% names(na.exclude(si))) %>% 
  left_join(
    si %>% 
      na.exclude() %>% 
      enframe("PARAMCD", "si"),
    by = "PARAMCD"
  ) %>% 
  mutate(`Visit 2` = Baseline  + exp(rnorm(1, sd = si))) %>% 
  mutate(`Visit 3` = `Visit 2` + exp(rnorm(1, sd = si))) %>% 
  select(-si, -Baseline)

adlb_info <- structure(list(PARAMCD = structure(c("CALCIUM", "CREAT", "GGT", 
                                                  "HB", "HCT", "HDL", "LDL", "MAGNES", "POTASS", "SODIUM", "URICAC"
), label = "Parameter Code"), PARAM = structure(c("Calcium (mg/dL) in Serum", 
                                                  "Creatinine (mg/dL) in Serum", "Gamma Glutamyl Transferase (U/L) in Serum", 
                                                  "Hemoglobin (g/dL) in Blood", "Hematocrit (%) in Blood - Calculated", 
                                                  "HDL Cholesterol (mg/dL) in Serum", "LDL Cholesterol (mg/dL) in Serum", 
                                                  "Magnesium (mg/dL) in Serum", "Potassium (mmol/L) in Serum", 
                                                  "Sodium (mmol/L) in Serum", "Urate (mg/dL) in Serum"), label = "Parameter"), 
AVALU = structure(c("mg/dL", "mg/dL", "U/L", "g/dL", "%", 
                    "mg/dL", "mg/dL", "mg/dL", "mmol/L", "mmol/L", "mg/dL"), label = "Standard Units")), row.names = c(NA, 
                                                                                                                       -11L), class = c("tbl_df", "tbl", "data.frame"))

adlb <- adlb_pre %>% 
  left_join(adlb_pre_sub, by = c("SUBJID", "PARAMCD")) %>% 
  pivot_longer(
    cols           = -c("SUBJID", "PARAMCD"),
    names_to       = "AVISIT",
    values_to      = "AVAL",
    values_drop_na = TRUE
  ) %>% 
  left_join(adlb_info, by = "PARAMCD") %>% 
  left_join(adsl %>% select(STUDYID, SUBJID, USUBJID, ITTFL), by = "SUBJID") %>% 
  mutate(AVAL = round(AVAL, 3)) %>% 
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
  mutate(ADSNAME = "adlb", .before = 1)


# define labels ####
adlb_labels <- list(
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

adlb <- adlb %>% 
  set_variable_labels( .labels = adlb_labels)

adlb_labels <- adlb_labels %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")

write_csv(adlb,        "dev/data/prep_pkg_study/adlb.csv", na = "")
write_csv(adlb_labels, "dev/data/prep_pkg_study/adlb_labels.csv")



# admh -----

lev <- list(
  "Coronary artery disorders" = tribble(
    ~MHDECOD,                    ~p,
    "Myocardial infarction",     .3,
    "Coronary artery disease",   .25,
    "Angina pectoris",           .05
  ),
  "Cardiac arrhythmias" = tribble(
    ~MHDECOD,                    ~p,
    "Atrial fibrillation",       .75,      
    "Ventricular tachycardia",   .05
  )
) %>% 
  enframe("MHHLGT") %>% 
  unnest("value")

stdy_start  <- -20:120
stdy_weight <- 1/(1:length(stdy_start))

admh <- crossing(SUBJID = adsl$SUBJID, lev) %>% 
  rowwise() %>% 
  mutate(MHOCCUR = sample(c("Y", "N"), size = 1, prob = c(p, 1-p))) %>% 
  ungroup() %>% 
  select(-p) %>% 
  filter(!(MHHLGT == "Coronary artery disorders" & MHOCCUR == "N")) %>% 
  mutate(MHOCCUR = case_when(
    MHHLGT == "Coronary artery disorders" ~ NA_character_,
    TRUE                                   ~ MHOCCUR
  )) %>%
  mutate(MHOCCURN = factor(MHOCCUR) %>% as.integer() %>% `-`(1)) %>% 
  mutate(
    MHSTDY = sample(stdy_start, size = n(), replace = TRUE, prob = stdy_weight) +
      sample(c(0, NA_real_), size = n(), replace = TRUE, prob = c(.7, .3))
  ) %>% 
  left_join(adsl %>% select(STUDYID, SUBJID, USUBJID), ., by = "SUBJID") %>% 
  mutate(ADSNAME = "ADMH", .before = 1)

# define labels ####
admh_labels <- list(
  ADSNAME  = "Dataset Name",
  STUDYID  = "Study Identifier",
  SUBJID   = "Subject Identifier for the Study",
  USUBJID  = "Unique Subject Identifier",
  MHHLGT   = "High Level Group Term",
  MHDECOD  = "Dictionary-Derived Term",
  MHOCCUR  = "Medical History Occurrence",
  MHOCCURN = "Medical History Occurrence (N)",
  MHSTDY   = "Study Day of Start of Observation"
)

admh <- admh %>% 
  set_variable_labels( .labels = admh_labels)

admh_labels <- admh_labels %>% 
  unlist() %>% 
  enframe(name = "column", value = "label")

write_csv(admh,        "dev/data/prep_pkg_study/admh.csv", na = "")
write_csv(admh_labels, "dev/data/prep_pkg_study/admh_labels.csv")
