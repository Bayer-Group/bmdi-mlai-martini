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
        name type size nsubj ncol
        adsl adsl 128K   320    7
        adlb bds  128K     5    3
        advs bds  448K   289    5
      
        Key columns used in bds-type data sets
        name param   value unit  time  
        adlb PARAMCD AVAL  AVALU AVISIT
        advs PARAMCD AVAL  AVALU AVISIT
      
        Filter information 
    Message <rlang_message>
      ! 1 filter could not be applied to any of the data sets:
        - TRUE
      * Please double check and adjust or remove from `filter` argument as applicable
        and rerun.

