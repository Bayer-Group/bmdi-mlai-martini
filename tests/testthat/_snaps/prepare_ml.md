# prepare_ml snapshots

    Code
      ads_ml_class
    Output
      $data_raw
      $data_raw$train
      # A tibble: 215 x 18
           .id .out  .trt  AGEGR01        SEX   RACE    AGE BMI_adsl  LAB1  LAB2  LAB3
         <dbl> <fct> <fct> <fct>          <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>
       1 10002 event PLC   at_least_75    F     WHITE    90     30       4     4     4
       2 10006 event PLC   under_60       M     WHITE    47     39.1    NA    NA    NA
       3 10015 event PLC   at_least_75    M     WHITE    81     31.9    NA    NA    NA
       4 10022 event PLC   60_to_under_75 M     WHITE    64     24      NA    NA    NA
       5 10026 event PLC   at_least_75    F     WHITE    82     28.9    NA    NA    NA
       6 10030 event PLC   60_to_under_75 F     WHITE    72     26.6    NA    NA    NA
       7 10031 event PLC   under_60       F     <NA>     48     28      NA    NA    NA
       8 10032 event PLC   at_least_75    M     WHITE    77     31.9    NA    NA    NA
       9 10037 event PLC   60_to_under_75 M     WHITE    65     29.5    NA    NA    NA
      10 10038 event PLC   under_60       M     WHITE    59     31.4    NA    NA    NA
      # i 205 more rows
      # i 7 more variables: BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,
      #   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>
      
      $data_raw$test
      # A tibble: 74 x 18
           .id .out     .trt  AGEGR01     SEX   RACE    AGE BMI_adsl  LAB1  LAB2  LAB3
         <dbl> <fct>    <fct> <fct>       <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>
       1 10001 event    TRT   at_least_75 M     WHITE    85     30.1     5     2     1
       2 10005 event    PLC   at_least_75 F     WHITE    76     29       3     4     4
       3 10017 no event PLC   under_60    F     WHITE    52     26      NA    NA    NA
       4 10018 no event PLC   at_least_75 M     WHITE    80     29.5    NA    NA    NA
       5 10024 event    PLC   at_least_75 M     WHITE    83     24.4    NA    NA    NA
       6 10033 no event TRT   at_least_75 F     <NA>     77     20.7    NA    NA    NA
       7 10041 no event PLC   60_to_unde~ F     WHITE    63     25.7    NA    NA    NA
       8 10042 event    PLC   under_60    F     WHITE    55     34.8    NA    NA    NA
       9 10049 no event TRT   at_least_75 F     WHITE    84     27.1    NA    NA    NA
      10 10052 no event PLC   under_60    F     WHITE    47     31.9    NA    NA    NA
      # i 64 more rows
      # i 7 more variables: BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,
      #   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>
      
      
      $data_prep
      $data_prep$train
      # A tibble: 215 x 13
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10002 PLC   at_least_75   F     WHITE    90     30      58   119  84     90.2
       2 10006 PLC   under_60      M     WHITE    47     39.1    78   116  69    117. 
       3 10015 PLC   at_least_75   M     WHITE    81     31.9    81   122  67     96.1
       4 10022 PLC   60_to_under_~ M     WHITE    64     24      54   101  80     73.4
       5 10026 PLC   at_least_75   F     WHITE    82     28.9    68   102  68.6   95.6
       6 10030 PLC   60_to_under_~ F     WHITE    72     26.6    78    95  71     71.9
       7 10031 PLC   under_60      F     WHITE    48     28      63   116  78     84.3
       8 10032 PLC   at_least_75   M     WHITE    77     31.9    74    92  56     99.5
       9 10037 PLC   60_to_under_~ M     WHITE    65     29.5    76   136  50     88.9
      10 10038 PLC   under_60      M     WHITE    59     31.4    82   125  76     91  
      # i 205 more rows
      # i 2 more variables: coronary_artery_disorders <int>, .out <fct>
      
      $data_prep$test
      # A tibble: 74 x 13
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10001 TRT   at_least_75   M     WHITE    85     30.1    74   116    79   92.9
       2 10005 PLC   at_least_75   F     WHITE    76     29      67   111    63   89.9
       3 10017 PLC   under_60      F     WHITE    52     26      58   121    70   82  
       4 10018 PLC   at_least_75   M     WHITE    80     29.5    86   128    64   95.1
       5 10024 PLC   at_least_75   M     WHITE    83     24.4    65   133    60   64.2
       6 10033 TRT   at_least_75   F     WHITE    77     20.7    82   114    58   66.6
       7 10041 PLC   60_to_under_~ F     WHITE    63     25.7    74   139    82   90.4
       8 10042 PLC   under_60      F     WHITE    55     34.8    88   132    63   97.1
       9 10049 TRT   at_least_75   F     WHITE    84     27.1    89   140    71   86.2
      10 10052 PLC   under_60      F     WHITE    47     31.9    85   143    68   87.5
      # i 64 more rows
      # i 2 more variables: coronary_artery_disorders <int>, .out <fct>
      
      
      $split
      <Training/Testing/Total>
      <215/74/289>
      
      $outcome
      $outcome$name
      [1] ".out"
      
      $outcome$mode
      [1] "classification"
      
      
      $dict
      # A tibble: 17 x 8
         param                     column       source label type  spec_id unit  logtr
         <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>
       1 .out                      .out         user_~ .out  <NA>  <NA>    <NA>  <NA> 
       2 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> 
       3 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> 
       4 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> 
       5 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> 
       6 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> 
       7 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> 
       8 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> 
       9 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> 
      10 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> 
      11 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> 
      12 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> 
      13 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> 
      14 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> 
      15 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> 
      16 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> 
      17 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> 
      
      $prep_recipe
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 16
      ID:         1
      
      -- Training information 
      Training data contained 215 data points and 213 incomplete rows.
      
      -- Operations 
      * Variables removed: LAB1, LAB2, LAB3 | Trained
      * Removing rows with NA values in: .out | Trained
      * K-nearest neighbor imputation for: RACE, BMI_adsl, BMI_advs, ... | Trained
      * Removing rows with NA values in: .trt, AGEGR01, SEX, RACE, AGE, ... | Trained
      * Sparse, unbalanced variable filter removed: cardiac_arrhythmias | Trained
      * Correlation filter on: BMI_adsl | Trained
      * Collapsing factor levels for: <none> | Trained
    Output
      
      $prep_params
      $prep_params$thres_log
      $prep_params$thres_log$value
      [1] 2
      
      $prep_params$thres_log$text
      [1] "Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details)."
      
      
      $prep_params$thres_count
      $prep_params$thres_count$value
      [1] NA
      
      $prep_params$thres_count$text
      [1] "Not applicable."
      
      
      $prep_params$thres_corr
      $prep_params$thres_corr$value
      [1] 0.9
      
      $prep_params$thres_corr$text
      [1] "The applied cutoff for removal of variables due to high correlations was 0.9."
      
      
      $prep_params$vars_keep_corr
      $prep_params$vars_keep_corr$value
      [1] NA
      
      $prep_params$vars_keep_corr$text
      [1] "No variables were excluded specifically due to high correlation with the variables in \"vars_keep_corr\""
      
      
      $prep_params$thres_lump
      $prep_params$thres_lump$value
      [1] 0.05
      
      $prep_params$thres_lump$text
      [1] "Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). "
      
      
      $prep_params$imp_ignore
      $prep_params$imp_ignore$value
      [1] 0.8
      
      $prep_params$imp_ignore$text
      [1] "Variables were dropped if the proportion of available data was less than 80%."
      
      
      $prep_params$nzv
      $prep_params$nzv$value
      $prep_params$nzv$value$freq_cut
      [1] 19
      
      $prep_params$nzv$value$unique_cut
      [1] 10
      
      
      $prep_params$nzv$text
      [1] "Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10)."
      
      
      
      $removed
      $removed$rows
      $removed$rows$outlier_outcome
      NULL
      
      $removed$rows$na_outcome
      NULL
      
      $removed$rows$na_feature
      NULL
      
      
      $removed$cols
      $removed$cols$rm
        LAB1   LAB2   LAB3 
      "LAB1" "LAB2" "LAB3" 
      
      $removed$cols$nzv
      [1] "cardiac_arrhythmias"
      
      $removed$cols$corr
      [1] "BMI_adsl"
      
      $removed$cols$imp_ignore
      [1] "LAB1" "LAB2" "LAB3"
      
      
      

---

    Code
      ads_ml_regr
    Output
      $data_raw
      $data_raw$train
      # A tibble: 216 x 18
           .id  .out .trt  AGEGR01        SEX   RACE    AGE BMI_adsl  LAB1  LAB2  LAB3
         <dbl> <dbl> <fct> <fct>          <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>
       1 10002 -0.39 PLC   at_least_75    F     WHITE    90     30       4     4     4
       2 10005  0    PLC   at_least_75    F     WHITE    76     29       3     4     4
       3 10009 -0.03 PLC   at_least_75    F     WHITE    77     36.4    NA    NA    NA
       4 10010 -0.01 PLC   at_least_75    M     BLACK    88     29.9    NA    NA    NA
       5 10012 -0.06 PLC   under_60       F     WHITE    49     29.9    NA    NA    NA
       6 10015  1.12 PLC   at_least_75    M     WHITE    81     31.9    NA    NA    NA
       7 10017  1.13 PLC   under_60       F     WHITE    52     26      NA    NA    NA
       8 10018 -0.55 PLC   at_least_75    M     WHITE    80     29.5    NA    NA    NA
       9 10022  0.47 PLC   60_to_under_75 M     WHITE    64     24      NA    NA    NA
      10 10024  1.9  PLC   at_least_75    M     WHITE    83     24.4    NA    NA    NA
      # i 206 more rows
      # i 7 more variables: BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,
      #   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>
      
      $data_raw$test
      # A tibble: 73 x 18
           .id  .out .trt  AGEGR01        SEX   RACE    AGE BMI_adsl  LAB1  LAB2  LAB3
         <dbl> <dbl> <fct> <fct>          <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>
       1 10003 -0.79 TRT   60_to_under_75 F     <NA>     62     17.7     5     4     2
       2 10004 -1.24 TRT   at_least_75    F     ASIAN    86     33.6     4     4     1
       3 10006  0.78 PLC   under_60       M     WHITE    47     39.1    NA    NA    NA
       4 10007 -2.2  TRT   under_60       M     WHITE    53     33.3    NA    NA    NA
       5 10008 -1.73 TRT   at_least_75    M     WHITE    75     39.8    NA    NA    NA
       6 10014  1.22 PLC   60_to_under_75 M     WHITE    64     23.5    NA    NA    NA
       7 10019 -0.57 TRT   60_to_under_75 M     <NA>     62     22.1    NA    NA    NA
       8 10021 -2.48 TRT   under_60       M     BLACK    52     31.3    NA    NA    NA
       9 10027  1.07 TRT   under_60       M     WHITE    53     24.1    NA    NA    NA
      10 10029  1.07 TRT   under_60       M     WHITE    56     25.3    NA    NA    NA
      # i 63 more rows
      # i 7 more variables: BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,
      #   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>
      
      
      $data_prep
      $data_prep$train
      # A tibble: 216 x 13
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10002 PLC   at_least_75   F     WHITE    90     30      58   119    84   90.2
       2 10005 PLC   at_least_75   F     WHITE    76     29      67   111    63   89.9
       3 10009 PLC   at_least_75   F     WHITE    77     36.4    73   111    82  114. 
       4 10010 PLC   at_least_75   M     BLACK    88     29.9    71   121    60   81.9
       5 10012 PLC   under_60      F     WHITE    49     29.9    77   109    83   98.1
       6 10015 PLC   at_least_75   M     WHITE    81     31.9    81   122    67   96.1
       7 10017 PLC   under_60      F     WHITE    52     26      58   121    70   82  
       8 10018 PLC   at_least_75   M     WHITE    80     29.5    86   128    64   95.1
       9 10022 PLC   60_to_under_~ M     WHITE    64     24      54   101    80   73.4
      10 10024 PLC   at_least_75   M     WHITE    83     24.4    65   133    60   64.2
      # i 206 more rows
      # i 2 more variables: coronary_artery_disorders <int>, .out <dbl>
      
      $data_prep$test
      # A tibble: 73 x 13
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10003 TRT   60_to_under_~ F     WHITE    62     17.7    59   105    86   51.9
       2 10004 TRT   at_least_75   F     ASIAN    86     33.6    84   134    64  106  
       3 10006 PLC   under_60      M     WHITE    47     39.1    78   116    69  117. 
       4 10007 TRT   under_60      M     WHITE    53     33.3    68   127    59   79.4
       5 10008 TRT   at_least_75   M     WHITE    75     39.8    87   141    75  113. 
       6 10014 PLC   60_to_under_~ M     WHITE    64     23.5    83   126    84   75.4
       7 10019 TRT   60_to_under_~ M     WHITE    62     22.1    56   101    56   57.8
       8 10021 TRT   under_60      M     BLACK    52     31.3    84   124    80   97.6
       9 10027 TRT   under_60      M     WHITE    53     24.1    80   152    69   50.2
      10 10029 TRT   under_60      M     WHITE    56     25.3    67   130    70   73  
      # i 63 more rows
      # i 2 more variables: coronary_artery_disorders <int>, .out <dbl>
      
      
      $split
      <Training/Testing/Total>
      <216/73/289>
      
      $outcome
      $outcome$name
      [1] ".out"
      
      $outcome$mode
      [1] "regression"
      
      
      $dict
      # A tibble: 17 x 8
         param                     column       source label type  spec_id unit  logtr
         <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>
       1 .out                      .out         user_~ .out  <NA>  <NA>    <NA>  <NA> 
       2 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> 
       3 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> 
       4 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> 
       5 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> 
       6 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> 
       7 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> 
       8 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> 
       9 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> 
      10 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> 
      11 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> 
      12 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> 
      13 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> 
      14 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> 
      15 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> 
      16 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> 
      17 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> 
      
      $prep_recipe
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 16
      ID:         1
      
      -- Training information 
      Training data contained 216 data points and 213 incomplete rows.
      
      -- Operations 
      * Variables removed: LAB1, LAB2, LAB3 | Trained
      * Removing rows with NA values in: .out | Trained
      * K-nearest neighbor imputation for: RACE, BMI_adsl, BMI_advs, ... | Trained
      * Removing rows with NA values in: .trt, AGEGR01, SEX, RACE, AGE, ... | Trained
      * Sparse, unbalanced variable filter removed: cardiac_arrhythmias | Trained
      * Correlation filter on: BMI_adsl | Trained
      * Collapsing factor levels for: <none> | Trained
    Output
      
      $prep_params
      $prep_params$thres_log
      $prep_params$thres_log$value
      [1] 2
      
      $prep_params$thres_log$text
      [1] "Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details)."
      
      
      $prep_params$thres_count
      $prep_params$thres_count$value
      [1] NA
      
      $prep_params$thres_count$text
      [1] "Not applicable."
      
      
      $prep_params$thres_corr
      $prep_params$thres_corr$value
      [1] 0.9
      
      $prep_params$thres_corr$text
      [1] "The applied cutoff for removal of variables due to high correlations was 0.9."
      
      
      $prep_params$vars_keep_corr
      $prep_params$vars_keep_corr$value
      [1] NA
      
      $prep_params$vars_keep_corr$text
      [1] "No variables were excluded specifically due to high correlation with the variables in \"vars_keep_corr\""
      
      
      $prep_params$thres_lump
      $prep_params$thres_lump$value
      [1] 0.05
      
      $prep_params$thres_lump$text
      [1] "Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). "
      
      
      $prep_params$imp_ignore
      $prep_params$imp_ignore$value
      [1] 0.8
      
      $prep_params$imp_ignore$text
      [1] "Variables were dropped if the proportion of available data was less than 80%."
      
      
      $prep_params$nzv
      $prep_params$nzv$value
      $prep_params$nzv$value$freq_cut
      [1] 19
      
      $prep_params$nzv$value$unique_cut
      [1] 10
      
      
      $prep_params$nzv$text
      [1] "Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10)."
      
      
      $prep_params$value
      [1] NA
      
      $prep_params$text
      [1] NA
      
      
      $removed
      $removed$rows
      $removed$rows$outlier_outcome
      NULL
      
      $removed$rows$na_outcome
      NULL
      
      $removed$rows$na_feature
      NULL
      
      
      $removed$cols
      $removed$cols$rm
        LAB1   LAB2   LAB3 
      "LAB1" "LAB2" "LAB3" 
      
      $removed$cols$nzv
      [1] "cardiac_arrhythmias"
      
      $removed$cols$corr
      [1] "BMI_adsl"
      
      $removed$cols$imp_ignore
      [1] "LAB1" "LAB2" "LAB3"
      
      
      

---

    Code
      ads_ml_surv
    Output
      $data_raw
      $data_raw$train
      # A tibble: 216 x 19
           .id .time .status .trt  AGEGR01      SEX   RACE    AGE BMI_adsl  LAB1  LAB2
         <dbl> <dbl>   <dbl> <fct> <fct>        <fct> <fct> <dbl>    <dbl> <dbl> <dbl>
       1 10018   270       0 PLC   at_least_75  M     WHITE    80     29.5    NA    NA
       2 10024    11       0 PLC   at_least_75  M     WHITE    83     24.4    NA    NA
       3 10026   174       0 PLC   at_least_75  F     WHITE    82     28.9    NA    NA
       4 10028    80       0 PLC   60_to_under~ M     WHITE    66     34.5    NA    NA
       5 10036     8       0 PLC   60_to_under~ F     WHITE    60     26      NA    NA
       6 10039     4       0 PLC   60_to_under~ F     WHITE    71     23.8    NA    NA
       7 10043   270       0 PLC   60_to_under~ M     WHITE    70     23.2    NA    NA
       8 10048   210       0 PLC   at_least_75  F     WHITE    76     24.8    NA    NA
       9 10067   270       0 PLC   60_to_under~ F     WHITE    60     25      NA    NA
      10 10071   122       0 PLC   under_60     F     WHITE    53     22.7    NA    NA
      # i 206 more rows
      # i 8 more variables: LAB3 <dbl>, BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>,
      #   HR <dbl>, WEIGHT <dbl>, cardiac_arrhythmias <int>,
      #   coronary_artery_disorders <int>
      
      $data_raw$test
      # A tibble: 73 x 19
           .id .time .status .trt  AGEGR01      SEX   RACE    AGE BMI_adsl  LAB1  LAB2
         <dbl> <dbl>   <dbl> <fct> <fct>        <fct> <fct> <dbl>    <dbl> <dbl> <dbl>
       1 10005    57       1 PLC   at_least_75  F     WHITE    76     29       3     4
       2 10007   200       0 TRT   under_60     M     WHITE    53     33.3    NA    NA
       3 10009    63       1 PLC   at_least_75  F     WHITE    77     36.4    NA    NA
       4 10010    30       1 PLC   at_least_75  M     BLACK    88     29.9    NA    NA
       5 10015     4       1 PLC   at_least_75  M     WHITE    81     31.9    NA    NA
       6 10022    85       0 PLC   60_to_under~ M     WHITE    64     24      NA    NA
       7 10025    18       1 TRT   under_60     F     WHITE    46     30.8    NA    NA
       8 10027    31       1 TRT   under_60     M     WHITE    53     24.1    NA    NA
       9 10030   135       0 PLC   60_to_under~ F     WHITE    72     26.6    NA    NA
      10 10031    32       1 PLC   under_60     F     <NA>     48     28      NA    NA
      # i 63 more rows
      # i 8 more variables: LAB3 <dbl>, BMI_advs <dbl>, BPDIA <dbl>, BPSYS <dbl>,
      #   HR <dbl>, WEIGHT <dbl>, cardiac_arrhythmias <int>,
      #   coronary_artery_disorders <int>
      
      
      $data_prep
      $data_prep$train
      # A tibble: 216 x 14
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10018 PLC   at_least_75   M     WHITE    80     29.5  86     128  64     95.1
       2 10024 PLC   at_least_75   M     WHITE    83     24.4  65     133  60     64.2
       3 10026 PLC   at_least_75   F     WHITE    82     28.9  68     102  68.6   95.6
       4 10028 PLC   60_to_under_~ M     WHITE    66     34.5  75     134  67    112. 
       5 10036 PLC   60_to_under_~ F     WHITE    60     26    69.6   128  56     62.6
       6 10039 PLC   60_to_under_~ F     WHITE    71     23.8  80      96  84     78.3
       7 10043 PLC   60_to_under_~ M     WHITE    70     23.2  85     134  48     64.9
       8 10048 PLC   at_least_75   F     WHITE    76     24.8  66     109  63     68  
       9 10067 PLC   60_to_under_~ F     WHITE    60     25    69     135  76     67  
      10 10071 PLC   under_60      F     WHITE    53     22.7  61     124  76     65.4
      # i 206 more rows
      # i 3 more variables: coronary_artery_disorders <int>, .time <dbl>,
      #   .status <dbl>
      
      $data_prep$test
      # A tibble: 73 x 14
           .id .trt  AGEGR01       SEX   RACE    AGE BMI_advs BPDIA BPSYS    HR WEIGHT
         <dbl> <fct> <fct>         <fct> <fct> <dbl>    <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 10005 PLC   at_least_75   F     WHITE    76     29      67   111    63   89.9
       2 10007 TRT   under_60      M     WHITE    53     33.3    68   127    59   79.4
       3 10009 PLC   at_least_75   F     WHITE    77     36.4    73   111    82  114. 
       4 10010 PLC   at_least_75   M     BLACK    88     29.9    71   121    60   81.9
       5 10015 PLC   at_least_75   M     WHITE    81     31.9    81   122    67   96.1
       6 10022 PLC   60_to_under_~ M     WHITE    64     24      54   101    80   73.4
       7 10025 TRT   under_60      F     WHITE    46     30.8    73   117    70   85.4
       8 10027 TRT   under_60      M     WHITE    53     24.1    80   152    69   50.2
       9 10030 PLC   60_to_under_~ F     WHITE    72     26.6    78    95    71   71.9
      10 10031 PLC   under_60      F     WHITE    48     28      63   116    78   84.3
      # i 63 more rows
      # i 3 more variables: coronary_artery_disorders <int>, .time <dbl>,
      #   .status <dbl>
      
      
      $split
      <Training/Testing/Total>
      <216/73/289>
      
      $outcome
      $outcome$name
      [1] ".time"   ".status"
      
      $outcome$mode
      [1] "survival"
      
      
      $dict
      # A tibble: 18 x 8
         param                     column       source label type  spec_id unit  logtr
         <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>
       1 .time                     .time        user_~ .time <NA>  <NA>    <NA>  <NA> 
       2 .status                   .status      user_~ .sta~ <NA>  <NA>    <NA>  <NA> 
       3 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> 
       4 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> 
       5 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> 
       6 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> 
       7 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> 
       8 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> 
       9 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> 
      10 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> 
      11 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> 
      12 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> 
      13 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> 
      14 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> 
      15 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> 
      16 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> 
      17 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> 
      18 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> 
      
      $prep_recipe
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    2
      predictor: 16
      ID:         1
      
      -- Training information 
      Training data contained 216 data points and 213 incomplete rows.
      
      -- Operations 
      * Variables removed: LAB1, LAB2, LAB3 | Trained
      * Removing rows with NA values in: .time, .status | Trained
      * K-nearest neighbor imputation for: RACE, BMI_adsl, BMI_advs, ... | Trained
      * Removing rows with NA values in: .trt, AGEGR01, SEX, RACE, AGE, ... | Trained
      * Sparse, unbalanced variable filter removed: cardiac_arrhythmias | Trained
      * Correlation filter on: BMI_adsl | Trained
      * Collapsing factor levels for: <none> | Trained
    Output
      
      $prep_params
      $prep_params$thres_log
      $prep_params$thres_log$value
      [1] 2
      
      $prep_params$thres_log$text
      [1] "Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details)."
      
      
      $prep_params$thres_count
      $prep_params$thres_count$value
      [1] NA
      
      $prep_params$thres_count$text
      [1] "Not applicable."
      
      
      $prep_params$thres_corr
      $prep_params$thres_corr$value
      [1] 0.9
      
      $prep_params$thres_corr$text
      [1] "The applied cutoff for removal of variables due to high correlations was 0.9."
      
      
      $prep_params$vars_keep_corr
      $prep_params$vars_keep_corr$value
      [1] NA
      
      $prep_params$vars_keep_corr$text
      [1] "No variables were excluded specifically due to high correlation with the variables in \"vars_keep_corr\""
      
      
      $prep_params$thres_lump
      $prep_params$thres_lump$value
      [1] 0.05
      
      $prep_params$thres_lump$text
      [1] "Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). "
      
      
      $prep_params$imp_ignore
      $prep_params$imp_ignore$value
      [1] 0.8
      
      $prep_params$imp_ignore$text
      [1] "Variables were dropped if the proportion of available data was less than 80%."
      
      
      $prep_params$nzv
      $prep_params$nzv$value
      $prep_params$nzv$value$freq_cut
      [1] 19
      
      $prep_params$nzv$value$unique_cut
      [1] 10
      
      
      $prep_params$nzv$text
      [1] "Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10)."
      
      
      
      $removed
      $removed$rows
      $removed$rows$outlier_outcome
      NULL
      
      $removed$rows$na_outcome
      NULL
      
      $removed$rows$na_feature
      NULL
      
      
      $removed$cols
      $removed$cols$rm
        LAB1   LAB2   LAB3 
      "LAB1" "LAB2" "LAB3" 
      
      $removed$cols$nzv
      [1] "cardiac_arrhythmias"
      
      $removed$cols$corr
      [1] "BMI_adsl"
      
      $removed$cols$imp_ignore
      [1] "LAB1" "LAB2" "LAB3"
      
      
      

