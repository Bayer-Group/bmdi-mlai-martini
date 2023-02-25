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
        admh MHHLGT NA    NA     TRUE  NA  

---

    Code
      ads_spec_mod
    Output
      $adsl
      $adsl$md5
      [1] "e1299ff8b6023ea1492c4e152c40234d"
      
      $adsl$size
      128K
      $adsl$data
      NULL
      
      $adsl$data_info
      $adsl$data_info$nsubj
      [1] 320
      
      $adsl$data_info$ncol
      [1] 7
      
      
      $adsl$type
      [1] "adsl"
      
      $adsl$filter
      NULL
      
      $adsl$select
      [1] "SUBJID"  "TRT01A"  "AGEGR01" "SEX"     "RACE"    "AGE"     "BMI"    
      
      $adsl$factor_levels
      $adsl$factor_levels$TRT01A
      [1] "PLC" "TRT"
      attr(,"label")
      [1] "Actual Treatment for Period 01"
      
      $adsl$factor_levels$AGEGR01
      [1] "< 60"     "60 - <75" ">=75"    
      attr(,"label")
      [1] "Age Group 01"
      
      $adsl$factor_levels$SEX
      [1] "M" "F"
      attr(,"label")
      [1] "Sex"
      
      $adsl$factor_levels$RACE
      [1] "WHITE" "BLACK" "ASIAN"
      
      
      $adsl$dict
      # A tibble: 19 x 5
         param    label                                  source type  selected
         <chr>    <chr>                                  <chr>  <chr> <lgl>   
       1 ADSNAME  Dataset Name                           SL     adsl  FALSE   
       2 STUDYID  Study Identifier                       SL     adsl  FALSE   
       3 SUBJID   Subject Identifier for the Study       SL     adsl  TRUE    
       4 USUBJID  Unique Subject Identifier              SL     adsl  FALSE   
       5 UASR     Unique Subject Identifier/Age/Sex/Race SL     adsl  FALSE   
       6 ITTFL    Intent-To-Treat Population Flag        SL     adsl  FALSE   
       7 TRT01P   Planned Treatment for Period 01        SL     adsl  FALSE   
       8 TRT01PN  Planned Treatment for Period 01 (N)    SL     adsl  FALSE   
       9 TRT01A   Actual Treatment for Period 01         SL     adsl  TRUE    
      10 TRT01AN  Actual Treatment for Period 01 (N)     SL     adsl  FALSE   
      11 RANDDT   Date of Randomization                  SL     adsl  FALSE   
      12 AGE      Age                                    SL     adsl  TRUE    
      13 AGEGR01  Age Group 01                           SL     adsl  TRUE    
      14 AGEGR01N Age Group 01 (N)                       SL     adsl  FALSE   
      15 SEX      Sex                                    SL     adsl  TRUE    
      16 SEXN     Sex (N)                                SL     adsl  FALSE   
      17 RACE     Race                                   SL     adsl  TRUE    
      18 RACEN    Race (N)                               SL     adsl  FALSE   
      19 BMI      Body Mass Index (kg/m2) at baseline    SL     adsl  TRUE    
      
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
      # ... with 310 more rows
      
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
      character(0)
      
      $adsl$drop_list$constant
      [1] "ADSNAME" "STUDYID" "ITTFL"  
      
      $adsl$drop_list$blacklist
      [1] "RANDNO"  "ADSNAME" "STUDYID" "SITEID"  "SITENAM" "INVID"   "INVNAM" 
      
      $adsl$drop_list$other
       [1] "ADSNAME"  "STUDYID"  "USUBJID"  "UASR"     "ITTFL"    "TRT01P"  
       [7] "TRT01PN"  "TRT01AN"  "RANDDT"   "AGEGR01N" "SEXN"     "RACEN"   
      
      
      $adsl$id
      [1] "SUBJID"
      
      $adsl$trt
      [1] "TRT01A"
      
      $adsl$spec_id
      [1] "SL"
      
      
      $adlb
      $adlb$md5
      [1] "a93bacf954d8bd2ce31732ece8f46e00"
      
      $adlb$size
      128K
      $adlb$type
      [1] "bds"
      
      $adlb$filter
      NULL
      
      $adlb$spec_id
      [1] "LB"
      
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
      1 LAB1  Laboratory 1 unit1 LB     bds   TRUE    
      2 LAB2  Laboratory 2 unit2 LB     bds   TRUE    
      3 LAB3  Laboratory 3 unit3 LB     bds   TRUE    
      
      $adlb$data_info
      $adlb$data_info$nsubj
      [1] 5
      
      $adlb$data_info$ncol
      [1] 3
      
      
      
      $advs
      $advs$md5
      [1] "268a148a18b06e794a168349b06d8d60"
      
      $advs$size
      448K
      $advs$type
      [1] "bds"
      
      $advs$filter
      NULL
      
      $advs$spec_id
      [1] "VS"
      
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
      1 BMI    Body Mass Index          kg/m2     VS     bds   TRUE    
      2 BPDIA  Diastolic Blood Pressure mmHg      VS     bds   TRUE    
      3 BPSYS  Systolic Blood Pressure  mmHg      VS     bds   TRUE    
      4 HR     Heart Rate               beats/min VS     bds   TRUE    
      5 WEIGHT Weight                   kg        VS     bds   TRUE    
      
      $advs$data_info
      $advs$data_info$nsubj
      [1] 289
      
      $advs$data_info$ncol
      [1] 5
      
      
      
      $admh
      $admh$md5
      [1] "0b5961115aad87b07cadc224eccc8742"
      
      $admh$size
      192K
      $admh$type
      [1] "occds"
      
      $admh$id
      [1] "SUBJID"
      
      $admh$filter
      NULL
      
      $admh$count
      [1] TRUE
      
      $admh$spec_id
      [1] "MH"
      
      $admh$label
      [1] "MHHLGT"
      
      $admh$dict
      # A tibble: 2 x 4
        label                     source type  selected
        <chr>                     <chr>  <chr> <lgl>   
      1 Cardiac arrhythmias       MH     occds TRUE    
      2 Coronary artery disorders MH     occds TRUE    
      
      $admh$data_info
      $admh$data_info$nsubj
      [1] 320
      
      $admh$data_info$ncol
      [1] 2
      
      
      
      attr(,"filter_ok")
      [1] TRUE
      attr(,"data_info_ok")
      [1] TRUE

