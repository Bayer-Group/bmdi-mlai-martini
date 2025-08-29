# info_filter() works

    Code
      info_filter(martini_spec)
    Message
      v 4 filters could be applied:
        - adsl: ITTFL == 'Y'
        - adlb: AVISIT == 'Baseline'
        - advs: AVISIT == 'Baseline'
        - admh: MHOCCUR == 'Y' | is.na(MHOCCUR)

# adam_spec snapshots

    Code
      ads_spec
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
        admh MHHLGT NA    NA     TRUE  MHSTDY

---

    Code
      ads_spec_mod
    Output
      $adsl
      $adsl$file
      <REDACTED>
      
      $adsl$md5
      [1] "e1299ff8b6023ea1492c4e152c40234d"
      
      $adsl$size
      128K
      $adsl$type
      [1] "adsl"
      
      $adsl$filter
      NULL
      
      $adsl$spec_id
      [1] "adsl"
      
      $adsl$id
      [1] "SUBJID"
      
      $adsl$trt
      [1] "TRT01A"
      
      $adsl$select
      [1] "SUBJID"  "TRT01A"  "AGEGR01" "SEX"     "RACE"    "AGE"     "BMI"    
      
      $adsl$factor_levels
      $adsl$factor_levels$TRT01A
        PLC   TRT 
      "PLC" "TRT" 
      attr(,"label")
      [1] "Actual Treatment for Period 01"
      
      $adsl$factor_levels$AGEGR01
            < 60   60 - <75       >=75 
          "< 60" "60 - <75"     ">=75" 
      attr(,"label")
      [1] "Age Group 01"
      
      $adsl$factor_levels$SEX
        M   F 
      "M" "F" 
      attr(,"label")
      [1] "Sex"
      
      $adsl$factor_levels$RACE
        WHITE   BLACK   ASIAN 
      "WHITE" "BLACK" "ASIAN" 
      
      
      $adsl$drop_list
      $adsl$drop_list$drop
      NULL
      
      $adsl$drop_list$datetime
      [1] "RANDDT"
      
      $adsl$drop_list$numcode
      [1] "TRT01PN"  "TRT01AN"  "AGEGR01N" "SEXN"     "RACEN"   
      
      $adsl$drop_list$combination
      [1] "UASR"
      
      $adsl$drop_list$redundancy
      [1] "USUBJID" "UASR"    "RANDDT"  "TRT01P" 
      
      $adsl$drop_list$flag
      [1] "ITTFL"
      
      $adsl$drop_list$empty
      NULL
      
      $adsl$drop_list$constant
      $adsl$drop_list$constant$constant
      [1] "ADSNAME" "STUDYID" "ITTFL"  
      
      $adsl$drop_list$constant$empty
      character(0)
      
      
      $adsl$drop_list$blacklist
      character(0)
      
      $adsl$drop_list$other
      character(0)
      
      
      $adsl$flag_table
      # A tibble: 320 x 2
         SUBJID ITTFL
          <dbl> <chr>
       1  10001 Y    
       2  10002 Y    
       3  10003 Y    
       4  10004 Y    
       5  10005 Y    
       6  10006 Y    
       7  10007 Y    
       8  10008 Y    
       9  10009 Y    
      10  10010 Y    
      # i 310 more rows
      
      $adsl$dict
      # A tibble: 19 x 5
         param    label                                  source type  selected
         <chr>    <chr>                                  <chr>  <chr> <lgl>   
       1 ADSNAME  Dataset Name                           adsl   adsl  FALSE   
       2 STUDYID  Study Identifier                       adsl   adsl  FALSE   
       3 SUBJID   Subject Identifier for the Study       adsl   adsl  TRUE    
       4 USUBJID  Unique Subject Identifier              adsl   adsl  FALSE   
       5 UASR     Unique Subject Identifier/Age/Sex/Race adsl   adsl  FALSE   
       6 ITTFL    Intent-To-Treat Population Flag        adsl   adsl  FALSE   
       7 TRT01P   Planned Treatment for Period 01        adsl   adsl  FALSE   
       8 TRT01PN  Planned Treatment for Period 01 (N)    adsl   adsl  FALSE   
       9 TRT01A   Actual Treatment for Period 01         adsl   adsl  TRUE    
      10 TRT01AN  Actual Treatment for Period 01 (N)     adsl   adsl  FALSE   
      11 RANDDT   Date of Randomization                  adsl   adsl  FALSE   
      12 AGE      Age                                    adsl   adsl  TRUE    
      13 AGEGR01  Age Group 01                           adsl   adsl  TRUE    
      14 AGEGR01N Age Group 01 (N)                       adsl   adsl  FALSE   
      15 SEX      Sex                                    adsl   adsl  TRUE    
      16 SEXN     Sex (N)                                adsl   adsl  FALSE   
      17 RACE     Race                                   adsl   adsl  TRUE    
      18 RACEN    Race (N)                               adsl   adsl  FALSE   
      19 BMI      Body Mass Index (kg/m2) at baseline    adsl   adsl  TRUE    
      
      $adsl$data_info
      $adsl$data_info$nsubj
      [1] 320
      
      $adsl$data_info$ncol
      [1] 7
      
      
      $adsl$use_for_build
      [1] TRUE
      
      attr(,"filter_ok")
      [1] TRUE
      attr(,"data_info_ok")
      [1] TRUE
      
      $adlb
      $adlb$file
      <REDACTED>
      
      $adlb$md5
      [1] "a93bacf954d8bd2ce31732ece8f46e00"
      
      $adlb$size
      128K
      $adlb$type
      [1] "bds"
      
      $adlb$filter
      NULL
      
      $adlb$spec_id
      [1] "adlb"
      
      $adlb$id
      [1] "SUBJID"
      
      $adlb$value
      [1] "AVAL"
      
      $adlb$param
      [1] "PARAMCD"
      
      $adlb$time
      [1] "AVISIT"
      
      $adlb$unit
      [1] "AVALU"
      
      $adlb$label
      [1] "PARAM"
      
      $adlb$dupl_ctrl
      $adlb$dupl_ctrl$values_fn
      NULL
      
      $adlb$dupl_ctrl$arrange
      NULL
      
      
      $adlb$dict
      # A tibble: 3 x 6
        param label        unit  source type  selected
        <chr> <chr>        <chr> <chr>  <chr> <lgl>   
      1 LAB1  Laboratory 1 unit1 adlb   bds   TRUE    
      2 LAB2  Laboratory 2 unit2 adlb   bds   TRUE    
      3 LAB3  Laboratory 3 unit3 adlb   bds   TRUE    
      
      $adlb$data_info
      $adlb$data_info$nsubj
      [1] 5
      
      $adlb$data_info$ncol
      [1] 3
      
      
      $adlb$use_for_build
      [1] TRUE
      
      attr(,"filter_ok")
      [1] TRUE
      attr(,"data_info_ok")
      [1] TRUE
      
      $advs
      $advs$file
      <REDACTED>
      
      $advs$md5
      [1] "268a148a18b06e794a168349b06d8d60"
      
      $advs$size
      448K
      $advs$type
      [1] "bds"
      
      $advs$filter
      NULL
      
      $advs$spec_id
      [1] "advs"
      
      $advs$id
      [1] "SUBJID"
      
      $advs$value
      [1] "AVAL"
      
      $advs$param
      [1] "PARAMCD"
      
      $advs$time
      [1] "AVISIT"
      
      $advs$unit
      [1] "AVALU"
      
      $advs$label
      [1] "PARAM"
      
      $advs$dupl_ctrl
      $advs$dupl_ctrl$values_fn
      NULL
      
      $advs$dupl_ctrl$arrange
      NULL
      
      
      $advs$dict
      # A tibble: 5 x 6
        param  label                    unit      source type  selected
        <chr>  <chr>                    <chr>     <chr>  <chr> <lgl>   
      1 BMI    Body Mass Index          kg/m2     advs   bds   TRUE    
      2 BPDIA  Diastolic Blood Pressure mmHg      advs   bds   TRUE    
      3 BPSYS  Systolic Blood Pressure  mmHg      advs   bds   TRUE    
      4 HR     Heart Rate               beats/min advs   bds   TRUE    
      5 WEIGHT Weight                   kg        advs   bds   TRUE    
      
      $advs$data_info
      $advs$data_info$nsubj
      [1] 289
      
      $advs$data_info$ncol
      [1] 5
      
      
      $advs$use_for_build
      [1] TRUE
      
      attr(,"filter_ok")
      [1] TRUE
      attr(,"data_info_ok")
      [1] TRUE
      
      $admh
      $admh$file
      <REDACTED>
      
      $admh$md5
      [1] "0b5961115aad87b07cadc224eccc8742"
      
      $admh$size
      192K
      $admh$type
      [1] "occds"
      
      $admh$filter
      NULL
      
      $admh$spec_id
      [1] "admh"
      
      $admh$id
      [1] "SUBJID"
      
      $admh$label
      [1] "MHHLGT"
      
      $admh$value
      NULL
      
      $admh$valuen
      NULL
      
      $admh$time
      [1] "MHSTDY"
      
      $admh$count
      [1] TRUE
      
      $admh$dict
      # A tibble: 2 x 4
        label                     source type  selected
        <chr>                     <chr>  <chr> <lgl>   
      1 Cardiac arrhythmias       admh   occds TRUE    
      2 Coronary artery disorders admh   occds TRUE    
      
      $admh$data_info
      $admh$data_info$nsubj
      [1] 320
      
      $admh$data_info$ncol
      [1] 2
      
      
      $admh$use_for_build
      [1] TRUE
      
      attr(,"filter_ok")
      [1] TRUE
      attr(,"data_info_ok")
      [1] TRUE
      

