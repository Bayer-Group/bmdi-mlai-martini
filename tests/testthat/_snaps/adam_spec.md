# adam_spec snapshots

    Code
      adam_spec(ads_path)
    Output
      
    Message <rlang_message>
      i The following domains were not processed as they are currently not in the library: 
          adlb_miss, adlb_rename
        You may consider using the `add_bds` argument in `adam_spec()` to add bds-type data.
    Output
      
      
        Content
        name type  size nsubj ncol
        adsl adsl  128K   320    7
        adlb bds   128K     5    3
        advs bds   448K   289    5
        admh occds 192K   320    2
      
        Key columns used in bds-type data sets
        name param   value unit  time  
        adlb PARAMCD AVAL  AVALU AVISIT
        advs PARAMCD AVAL  AVALU AVISIT
      
        Key columns used in occds-type data sets
        name label  value valuen count time
        admh MHHLGT NA    NA     TRUE  NA  

