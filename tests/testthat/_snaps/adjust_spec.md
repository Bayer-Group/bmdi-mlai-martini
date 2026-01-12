# adjust_adsl_select() works

    Code
      adjust_adsl_select(spec = martini_spec, select = c("SUBJID", "TRT01A", "SEX"))
    Condition
      Warning:
      The identifier column contained in the subject level data set must be selected.
      ! According to the specified modifications, the identifier column USUBJID is not part of the selected columns.
      i The column USUBJID will be added to the selection.
      * If the use of an alternative `id` column was intended, please re-run `adam_spec()` with the `id` argument.
    Output
      
        Content
        name type   size nsubj ncol
        adsl adsl   128K   289    4
        adlb bds   1.31M   289   11
        advs bds    448K   289    5
        admh occds  192K   282    5
      
        Key columns used in bds-type data sets
        name param   value unit  time  
        adlb PARAMCD AVAL  AVALU AVISIT
        advs PARAMCD AVAL  AVALU AVISIT
      
        Key columns used in occds-type data sets
        name label   value valuen count
        admh MHDECOD NA    NA     FALSE
      
        Filter information 
    Message
      v Each filter may be applied at least once.
      v 4 filters could be applied:
        - adsl: ITTFL == 'Y'
        - adlb: AVISIT == 'Baseline'
        - advs: AVISIT == 'Baseline'
        - admh: MHOCCUR == 'Y' | is.na(MHOCCUR)

# adjust_filter() works

    Code
      adjust_filter(spec = martini_spec, filter = "SUBJID %% 2 == 0", append = TRUE)
    Output
      
        Content
        name type   size nsubj ncol
        adsl adsl   128K   141    5
        adlb bds   1.31M   141   11
        advs bds    448K   141    5
        admh occds  192K   144    5
      
        Key columns used in bds-type data sets
        name param   value unit  time  
        adlb PARAMCD AVAL  AVALU AVISIT
        advs PARAMCD AVAL  AVALU AVISIT
      
        Key columns used in occds-type data sets
        name label   value valuen count
        admh MHDECOD NA    NA     FALSE
      
        Filter information 
    Message
      v Each filter may be applied at least once.
      v 4 filters could be applied:
        - adsl: ITTFL == 'Y', SUBJID %% 2 == 0
        - adlb: AVISIT == 'Baseline', SUBJID %% 2 == 0
        - advs: AVISIT == 'Baseline', SUBJID %% 2 == 0
        - admh: MHOCCUR == 'Y' | is.na(MHOCCUR), SUBJID %% 2 == 0

---

    Code
      adjust_filter(spec = martini_spec, filter = "SUBJID %% 2 == 0", append = FALSE)
    Output
      
        Content
        name type   size nsubj ncol
        adsl adsl   128K   160    5
        adlb bds   1.31M   141   11
        advs bds    448K   141    5
        admh occds  192K   160    5
      
        Key columns used in bds-type data sets
        name param   value unit  time  
        adlb PARAMCD AVAL  AVALU AVISIT
        advs PARAMCD AVAL  AVALU AVISIT
      
        Key columns used in occds-type data sets
        name label   value valuen count
        admh MHDECOD NA    NA     FALSE
      
        Filter information 
    Message
      v Each filter may be applied at least once.
      v 4 filters could be applied:
        - adsl: SUBJID %% 2 == 0
        - adlb: SUBJID %% 2 == 0
        - advs: SUBJID %% 2 == 0
        - admh: SUBJID %% 2 == 0

