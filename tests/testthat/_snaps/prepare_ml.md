# prepare_ml snapshots

    Code
      ads_ml_class %>% capture_output_lines(print = TRUE)
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
<<<<<<< HEAD
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
      
      
      
=======
        [1] "$data_raw"                                                                                                                                                                                      
        [2] "$data_raw$train"                                                                                                                                                                                
        [3] "# A tibble: 215 x 18"                                                                                                                                                                           
        [4] "     .id .out  .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3 BMI_a~2"                                                                                                               
        [5] "   <dbl> <fct> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>"                                                                                                               
        [6] " 1 10002 event PLC   at_lea~ F     WHITE    90    30       4     4     4    30  "                                                                                                               
        [7] " 2 10006 event PLC   under_~ M     WHITE    47    39.1    NA    NA    NA    39.1"                                                                                                               
        [8] " 3 10015 event PLC   at_lea~ M     WHITE    81    31.9    NA    NA    NA    31.9"                                                                                                               
        [9] " 4 10022 event PLC   60_to_~ M     WHITE    64    24      NA    NA    NA    24  "                                                                                                               
       [10] " 5 10026 event PLC   at_lea~ F     WHITE    82    28.9    NA    NA    NA    28.9"                                                                                                               
       [11] " 6 10030 event PLC   60_to_~ F     WHITE    72    26.6    NA    NA    NA    26.6"                                                                                                               
       [12] " 7 10031 event PLC   under_~ F     <NA>     48    28      NA    NA    NA    28  "                                                                                                               
       [13] " 8 10032 event PLC   at_lea~ M     WHITE    77    31.9    NA    NA    NA    31.9"                                                                                                               
       [14] " 9 10037 event PLC   60_to_~ M     WHITE    65    29.5    NA    NA    NA    29.5"                                                                                                               
       [15] "10 10038 event PLC   under_~ M     WHITE    59    31.4    NA    NA    NA    31.4"                                                                                                               
       [16] "# ... with 205 more rows, 6 more variables: BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,"                                                                                                                
       [17] "#   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>,"                                                                                                                  
       [18] "#   and abbreviated variable names 1: BMI_adsl, 2: BMI_advs"                                                                                                                                    
       [19] ""                                                                                                                                                                                               
       [20] "$data_raw$test"                                                                                                                                                                                 
       [21] "# A tibble: 74 x 18"                                                                                                                                                                            
       [22] "     .id .out  .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3 BMI_a~2"                                                                                                               
       [23] "   <dbl> <fct> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>"                                                                                                               
       [24] " 1 10001 event TRT   at_lea~ M     WHITE    85    30.1     5     2     1    30.1"                                                                                                               
       [25] " 2 10005 event PLC   at_lea~ F     WHITE    76    29       3     4     4    29  "                                                                                                               
       [26] " 3 10017 no e~ PLC   under_~ F     WHITE    52    26      NA    NA    NA    26  "                                                                                                               
       [27] " 4 10018 no e~ PLC   at_lea~ M     WHITE    80    29.5    NA    NA    NA    29.5"                                                                                                               
       [28] " 5 10024 event PLC   at_lea~ M     WHITE    83    24.4    NA    NA    NA    24.4"                                                                                                               
       [29] " 6 10033 no e~ TRT   at_lea~ F     <NA>     77    20.7    NA    NA    NA    20.7"                                                                                                               
       [30] " 7 10041 no e~ PLC   60_to_~ F     WHITE    63    25.7    NA    NA    NA    25.7"                                                                                                               
       [31] " 8 10042 event PLC   under_~ F     WHITE    55    34.8    NA    NA    NA    34.8"                                                                                                               
       [32] " 9 10049 no e~ TRT   at_lea~ F     WHITE    84    27.1    NA    NA    NA    27.1"                                                                                                               
       [33] "10 10052 no e~ PLC   under_~ F     WHITE    47    31.9    NA    NA    NA    31.9"                                                                                                               
       [34] "# ... with 64 more rows, 6 more variables: BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,"                                                                                                                 
       [35] "#   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>,"                                                                                                                  
       [36] "#   and abbreviated variable names 1: BMI_adsl, 2: BMI_advs"                                                                                                                                    
       [37] ""                                                                                                                                                                                               
       [38] ""                                                                                                                                                                                               
       [39] "$data_prep"                                                                                                                                                                                     
       [40] "$data_prep$train"                                                                                                                                                                               
       [41] "# A tibble: 215 x 13"                                                                                                                                                                           
       [42] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [43] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [44] " 1 10002 PLC   at_least_75    F     WHITE    90    30      58   119  84     90.2"                                                                                                               
       [45] " 2 10006 PLC   under_60       M     WHITE    47    39.1    78   116  69    117. "                                                                                                               
       [46] " 3 10015 PLC   at_least_75    M     WHITE    81    31.9    81   122  67     96.1"                                                                                                               
       [47] " 4 10022 PLC   60_to_under_75 M     WHITE    64    24      54   101  80     73.4"                                                                                                               
       [48] " 5 10026 PLC   at_least_75    F     WHITE    82    28.9    68   102  68.6   95.6"                                                                                                               
       [49] " 6 10030 PLC   60_to_under_75 F     WHITE    72    26.6    78    95  71     71.9"                                                                                                               
       [50] " 7 10031 PLC   under_60       F     WHITE    48    28      63   116  78     84.3"                                                                                                               
       [51] " 8 10032 PLC   at_least_75    M     WHITE    77    31.9    74    92  56     99.5"                                                                                                               
       [52] " 9 10037 PLC   60_to_under_75 M     WHITE    65    29.5    76   136  50     88.9"                                                                                                               
       [53] "10 10038 PLC   under_60       M     WHITE    59    31.4    82   125  76     91  "                                                                                                               
       [54] "# ... with 205 more rows, 2 more variables: coronary_artery_disorders <int>,"                                                                                                                   
       [55] "#   .out <fct>, and abbreviated variable name 1: BMI_advs"                                                                                                                                      
       [56] ""                                                                                                                                                                                               
       [57] "$data_prep$test"                                                                                                                                                                                
       [58] "# A tibble: 74 x 13"                                                                                                                                                                            
       [59] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [60] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [61] " 1 10001 TRT   at_least_75    M     WHITE    85    30.1    74   116    79   92.9"                                                                                                               
       [62] " 2 10005 PLC   at_least_75    F     WHITE    76    29      67   111    63   89.9"                                                                                                               
       [63] " 3 10017 PLC   under_60       F     WHITE    52    26      58   121    70   82  "                                                                                                               
       [64] " 4 10018 PLC   at_least_75    M     WHITE    80    29.5    86   128    64   95.1"                                                                                                               
       [65] " 5 10024 PLC   at_least_75    M     WHITE    83    24.4    65   133    60   64.2"                                                                                                               
       [66] " 6 10033 TRT   at_least_75    F     WHITE    77    20.7    82   114    58   66.6"                                                                                                               
       [67] " 7 10041 PLC   60_to_under_75 F     WHITE    63    25.7    74   139    82   90.4"                                                                                                               
       [68] " 8 10042 PLC   under_60       F     WHITE    55    34.8    88   132    63   97.1"                                                                                                               
       [69] " 9 10049 TRT   at_least_75    F     WHITE    84    27.1    89   140    71   86.2"                                                                                                               
       [70] "10 10052 PLC   under_60       F     WHITE    47    31.9    85   143    68   87.5"                                                                                                               
       [71] "# ... with 64 more rows, 2 more variables: coronary_artery_disorders <int>,"                                                                                                                    
       [72] "#   .out <fct>, and abbreviated variable name 1: BMI_advs"                                                                                                                                      
       [73] ""                                                                                                                                                                                               
       [74] ""                                                                                                                                                                                               
       [75] "$split"                                                                                                                                                                                         
       [76] "<Training/Testing/Total>"                                                                                                                                                                       
       [77] "<215/74/289>"                                                                                                                                                                                   
       [78] ""                                                                                                                                                                                               
       [79] "$outcome"                                                                                                                                                                                       
       [80] "$outcome$name"                                                                                                                                                                                  
       [81] "[1] \".out\""                                                                                                                                                                                   
       [82] ""                                                                                                                                                                                               
       [83] "$outcome$mode"                                                                                                                                                                                  
       [84] "[1] \"classification\""                                                                                                                                                                         
       [85] ""                                                                                                                                                                                               
       [86] ""                                                                                                                                                                                               
       [87] "$dict"                                                                                                                                                                                          
       [88] "# A tibble: 17 x 8"                                                                                                                                                                             
       [89] "   param                     column       source label type  spec_id unit  logtr"                                                                                                               
       [90] "   <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>"                                                                                                               
       [91] " 1 .out                      .out         user_~ .out  <NA>  <NA>    <NA>  <NA> "                                                                                                               
       [92] " 2 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [93] " 3 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> "                                                                                                               
       [94] " 4 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [95] " 5 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> "                                                                                                               
       [96] " 6 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> "                                                                                                               
       [97] " 7 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [98] " 8 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> "                                                                                                               
       [99] " 9 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> "                                                                                                               
      [100] "10 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> "                                                                                                               
      [101] "11 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> "                                                                                                               
      [102] "12 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> "                                                                                                               
      [103] "13 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> "                                                                                                               
      [104] "14 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> "                                                                                                               
      [105] "15 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> "                                                                                                               
      [106] "16 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> "                                                                                                               
      [107] "17 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> "                                                                                                               
      [108] ""                                                                                                                                                                                               
      [109] "$prep_recipe"                                                                                                                                                                                   
      [110] ""                                                                                                                                                                                               
      [111] "$prep_params"                                                                                                                                                                                   
      [112] "$prep_params$thres_log"                                                                                                                                                                         
      [113] "$prep_params$thres_log$value"                                                                                                                                                                   
      [114] "[1] 2"                                                                                                                                                                                          
      [115] ""                                                                                                                                                                                               
      [116] "$prep_params$thres_log$text"                                                                                                                                                                    
      [117] "[1] \"Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details).\""
      [118] ""                                                                                                                                                                                               
      [119] ""                                                                                                                                                                                               
      [120] "$prep_params$thres_count"                                                                                                                                                                       
      [121] "$prep_params$thres_count$value"                                                                                                                                                                 
      [122] "[1] NA"                                                                                                                                                                                         
      [123] ""                                                                                                                                                                                               
      [124] "$prep_params$thres_count$text"                                                                                                                                                                  
      [125] "[1] \"Not applicable.\""                                                                                                                                                                        
      [126] ""                                                                                                                                                                                               
      [127] ""                                                                                                                                                                                               
      [128] "$prep_params$thres_corr"                                                                                                                                                                        
      [129] "$prep_params$thres_corr$value"                                                                                                                                                                  
      [130] "[1] 0.9"                                                                                                                                                                                        
      [131] ""                                                                                                                                                                                               
      [132] "$prep_params$thres_corr$text"                                                                                                                                                                   
      [133] "[1] \"The applied cutoff for removal of variables due to high correlations was 0.9.\""                                                                                                          
      [134] ""                                                                                                                                                                                               
      [135] ""                                                                                                                                                                                               
      [136] "$prep_params$vars_keep_corr"                                                                                                                                                                    
      [137] "$prep_params$vars_keep_corr$value"                                                                                                                                                              
      [138] "[1] NA"                                                                                                                                                                                         
      [139] ""                                                                                                                                                                                               
      [140] "$prep_params$vars_keep_corr$text"                                                                                                                                                               
      [141] "[1] \"No variables were excluded specifically due to high correlation with the variables in \\\"vars_keep_corr\\\"\""                                                                           
      [142] ""                                                                                                                                                                                               
      [143] ""                                                                                                                                                                                               
      [144] "$prep_params$thres_lump"                                                                                                                                                                        
      [145] "$prep_params$thres_lump$value"                                                                                                                                                                  
      [146] "[1] 0.05"                                                                                                                                                                                       
      [147] ""                                                                                                                                                                                               
      [148] "$prep_params$thres_lump$text"                                                                                                                                                                   
      [149] "[1] \"Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). \""                                                                                                  
      [150] ""                                                                                                                                                                                               
      [151] ""                                                                                                                                                                                               
      [152] "$prep_params$imp_ignore"                                                                                                                                                                        
      [153] "$prep_params$imp_ignore$value"                                                                                                                                                                  
      [154] "[1] 0.8"                                                                                                                                                                                        
      [155] ""                                                                                                                                                                                               
      [156] "$prep_params$imp_ignore$text"                                                                                                                                                                   
      [157] "[1] \"Variables were dropped if the proportion of available data was less than 80%.\""                                                                                                          
      [158] ""                                                                                                                                                                                               
      [159] ""                                                                                                                                                                                               
      [160] "$prep_params$nzv"                                                                                                                                                                               
      [161] "$prep_params$nzv$value"                                                                                                                                                                         
      [162] "$prep_params$nzv$value$freq_cut"                                                                                                                                                                
      [163] "[1] 19"                                                                                                                                                                                         
      [164] ""                                                                                                                                                                                               
      [165] "$prep_params$nzv$value$unique_cut"                                                                                                                                                              
      [166] "[1] 10"                                                                                                                                                                                         
      [167] ""                                                                                                                                                                                               
      [168] ""                                                                                                                                                                                               
      [169] "$prep_params$nzv$text"                                                                                                                                                                          
      [170] "[1] \"Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10).\""                                                                           
      [171] ""                                                                                                                                                                                               
      [172] ""                                                                                                                                                                                               
      [173] ""                                                                                                                                                                                               
      [174] "$removed"                                                                                                                                                                                       
      [175] "$removed$rows"                                                                                                                                                                                  
      [176] "$removed$rows$outlier_outcome"                                                                                                                                                                  
      [177] "NULL"                                                                                                                                                                                           
      [178] ""                                                                                                                                                                                               
      [179] "$removed$rows$na_outcome"                                                                                                                                                                       
      [180] "NULL"                                                                                                                                                                                           
      [181] ""                                                                                                                                                                                               
      [182] "$removed$rows$na_feature"                                                                                                                                                                       
      [183] "NULL"                                                                                                                                                                                           
      [184] ""                                                                                                                                                                                               
      [185] ""                                                                                                                                                                                               
      [186] "$removed$cols"                                                                                                                                                                                  
      [187] "$removed$cols$rm"                                                                                                                                                                               
      [188] "  LAB1   LAB2   LAB3 "                                                                                                                                                                          
      [189] "\"LAB1\" \"LAB2\" \"LAB3\" "                                                                                                                                                                    
      [190] ""                                                                                                                                                                                               
      [191] "$removed$cols$nzv"                                                                                                                                                                              
      [192] "[1] \"cardiac_arrhythmias\""                                                                                                                                                                    
      [193] ""                                                                                                                                                                                               
      [194] "$removed$cols$corr"                                                                                                                                                                             
      [195] "[1] \"BMI_adsl\""                                                                                                                                                                               
      [196] ""                                                                                                                                                                                               
      [197] "$removed$cols$imp_ignore"                                                                                                                                                                       
      [198] "[1] \"LAB1\" \"LAB2\" \"LAB3\""                                                                                                                                                                 
      [199] ""                                                                                                                                                                                               
      [200] ""                                                                                                                                                                                               
      [201] ""                                                                                                                                                                                               
>>>>>>> 2475568efaa1f7e240f72f5438b98748da21f865

---

    Code
      ads_ml_regr %>% capture_output_lines(print = TRUE)
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
        [1] "$data_raw"                                                                                                                                                                                      
        [2] "$data_raw$train"                                                                                                                                                                                
        [3] "# A tibble: 216 x 18"                                                                                                                                                                           
        [4] "     .id  .out .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3 BMI_a~2"                                                                                                               
        [5] "   <dbl> <dbl> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>"                                                                                                               
        [6] " 1 10002 -0.39 PLC   at_lea~ F     WHITE    90    30       4     4     4    30  "                                                                                                               
        [7] " 2 10005  0    PLC   at_lea~ F     WHITE    76    29       3     4     4    29  "                                                                                                               
        [8] " 3 10009 -0.03 PLC   at_lea~ F     WHITE    77    36.4    NA    NA    NA    36.4"                                                                                                               
        [9] " 4 10010 -0.01 PLC   at_lea~ M     BLACK    88    29.9    NA    NA    NA    29.9"                                                                                                               
       [10] " 5 10012 -0.06 PLC   under_~ F     WHITE    49    29.9    NA    NA    NA    29.9"                                                                                                               
       [11] " 6 10015  1.12 PLC   at_lea~ M     WHITE    81    31.9    NA    NA    NA    31.9"                                                                                                               
       [12] " 7 10017  1.13 PLC   under_~ F     WHITE    52    26      NA    NA    NA    26  "                                                                                                               
       [13] " 8 10018 -0.55 PLC   at_lea~ M     WHITE    80    29.5    NA    NA    NA    29.5"                                                                                                               
       [14] " 9 10022  0.47 PLC   60_to_~ M     WHITE    64    24      NA    NA    NA    24  "                                                                                                               
       [15] "10 10024  1.9  PLC   at_lea~ M     WHITE    83    24.4    NA    NA    NA    24.4"                                                                                                               
       [16] "# ... with 206 more rows, 6 more variables: BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,"                                                                                                                
       [17] "#   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>,"                                                                                                                  
       [18] "#   and abbreviated variable names 1: BMI_adsl, 2: BMI_advs"                                                                                                                                    
       [19] ""                                                                                                                                                                                               
       [20] "$data_raw$test"                                                                                                                                                                                 
       [21] "# A tibble: 73 x 18"                                                                                                                                                                            
       [22] "     .id  .out .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3 BMI_a~2"                                                                                                               
       [23] "   <dbl> <dbl> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>   <dbl>"                                                                                                               
       [24] " 1 10003 -0.79 TRT   60_to_~ F     <NA>     62    17.7     5     4     2    17.7"                                                                                                               
       [25] " 2 10004 -1.24 TRT   at_lea~ F     ASIAN    86    33.6     4     4     1    33.6"                                                                                                               
       [26] " 3 10006  0.78 PLC   under_~ M     WHITE    47    39.1    NA    NA    NA    39.1"                                                                                                               
       [27] " 4 10007 -2.2  TRT   under_~ M     WHITE    53    33.3    NA    NA    NA    33.3"                                                                                                               
       [28] " 5 10008 -1.73 TRT   at_lea~ M     WHITE    75    39.8    NA    NA    NA    39.8"                                                                                                               
       [29] " 6 10014  1.22 PLC   60_to_~ M     WHITE    64    23.5    NA    NA    NA    23.5"                                                                                                               
       [30] " 7 10019 -0.57 TRT   60_to_~ M     <NA>     62    22.1    NA    NA    NA    22.1"                                                                                                               
       [31] " 8 10021 -2.48 TRT   under_~ M     BLACK    52    31.3    NA    NA    NA    31.3"                                                                                                               
       [32] " 9 10027  1.07 TRT   under_~ M     WHITE    53    24.1    NA    NA    NA    24.1"                                                                                                               
       [33] "10 10029  1.07 TRT   under_~ M     WHITE    56    25.3    NA    NA    NA    25.3"                                                                                                               
       [34] "# ... with 63 more rows, 6 more variables: BPDIA <dbl>, BPSYS <dbl>, HR <dbl>,"                                                                                                                 
       [35] "#   WEIGHT <dbl>, cardiac_arrhythmias <int>, coronary_artery_disorders <int>,"                                                                                                                  
       [36] "#   and abbreviated variable names 1: BMI_adsl, 2: BMI_advs"                                                                                                                                    
       [37] ""                                                                                                                                                                                               
       [38] ""                                                                                                                                                                                               
       [39] "$data_prep"                                                                                                                                                                                     
       [40] "$data_prep$train"                                                                                                                                                                               
       [41] "# A tibble: 216 x 13"                                                                                                                                                                           
       [42] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [43] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [44] " 1 10002 PLC   at_least_75    F     WHITE    90    30      58   119    84   90.2"                                                                                                               
       [45] " 2 10005 PLC   at_least_75    F     WHITE    76    29      67   111    63   89.9"                                                                                                               
       [46] " 3 10009 PLC   at_least_75    F     WHITE    77    36.4    73   111    82  114. "                                                                                                               
       [47] " 4 10010 PLC   at_least_75    M     BLACK    88    29.9    71   121    60   81.9"                                                                                                               
       [48] " 5 10012 PLC   under_60       F     WHITE    49    29.9    77   109    83   98.1"                                                                                                               
       [49] " 6 10015 PLC   at_least_75    M     WHITE    81    31.9    81   122    67   96.1"                                                                                                               
       [50] " 7 10017 PLC   under_60       F     WHITE    52    26      58   121    70   82  "                                                                                                               
       [51] " 8 10018 PLC   at_least_75    M     WHITE    80    29.5    86   128    64   95.1"                                                                                                               
       [52] " 9 10022 PLC   60_to_under_75 M     WHITE    64    24      54   101    80   73.4"                                                                                                               
       [53] "10 10024 PLC   at_least_75    M     WHITE    83    24.4    65   133    60   64.2"                                                                                                               
       [54] "# ... with 206 more rows, 2 more variables: coronary_artery_disorders <int>,"                                                                                                                   
       [55] "#   .out <dbl>, and abbreviated variable name 1: BMI_advs"                                                                                                                                      
       [56] ""                                                                                                                                                                                               
       [57] "$data_prep$test"                                                                                                                                                                                
       [58] "# A tibble: 73 x 13"                                                                                                                                                                            
       [59] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [60] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [61] " 1 10003 TRT   60_to_under_75 F     WHITE    62    17.7    59   105    86   51.9"                                                                                                               
       [62] " 2 10004 TRT   at_least_75    F     ASIAN    86    33.6    84   134    64  106  "                                                                                                               
       [63] " 3 10006 PLC   under_60       M     WHITE    47    39.1    78   116    69  117. "                                                                                                               
       [64] " 4 10007 TRT   under_60       M     WHITE    53    33.3    68   127    59   79.4"                                                                                                               
       [65] " 5 10008 TRT   at_least_75    M     WHITE    75    39.8    87   141    75  113. "                                                                                                               
       [66] " 6 10014 PLC   60_to_under_75 M     WHITE    64    23.5    83   126    84   75.4"                                                                                                               
       [67] " 7 10019 TRT   60_to_under_75 M     WHITE    62    22.1    56   101    56   57.8"                                                                                                               
       [68] " 8 10021 TRT   under_60       M     BLACK    52    31.3    84   124    80   97.6"                                                                                                               
       [69] " 9 10027 TRT   under_60       M     WHITE    53    24.1    80   152    69   50.2"                                                                                                               
       [70] "10 10029 TRT   under_60       M     WHITE    56    25.3    67   130    70   73  "                                                                                                               
       [71] "# ... with 63 more rows, 2 more variables: coronary_artery_disorders <int>,"                                                                                                                    
       [72] "#   .out <dbl>, and abbreviated variable name 1: BMI_advs"                                                                                                                                      
       [73] ""                                                                                                                                                                                               
       [74] ""                                                                                                                                                                                               
       [75] "$split"                                                                                                                                                                                         
       [76] "<Training/Testing/Total>"                                                                                                                                                                       
       [77] "<216/73/289>"                                                                                                                                                                                   
       [78] ""                                                                                                                                                                                               
       [79] "$outcome"                                                                                                                                                                                       
       [80] "$outcome$name"                                                                                                                                                                                  
       [81] "[1] \".out\""                                                                                                                                                                                   
       [82] ""                                                                                                                                                                                               
       [83] "$outcome$mode"                                                                                                                                                                                  
       [84] "[1] \"regression\""                                                                                                                                                                             
       [85] ""                                                                                                                                                                                               
       [86] ""                                                                                                                                                                                               
       [87] "$dict"                                                                                                                                                                                          
       [88] "# A tibble: 17 x 8"                                                                                                                                                                             
       [89] "   param                     column       source label type  spec_id unit  logtr"                                                                                                               
       [90] "   <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>"                                                                                                               
       [91] " 1 .out                      .out         user_~ .out  <NA>  <NA>    <NA>  <NA> "                                                                                                               
       [92] " 2 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [93] " 3 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> "                                                                                                               
       [94] " 4 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [95] " 5 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> "                                                                                                               
       [96] " 6 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> "                                                                                                               
       [97] " 7 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [98] " 8 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> "                                                                                                               
       [99] " 9 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> "                                                                                                               
      [100] "10 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> "                                                                                                               
      [101] "11 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> "                                                                                                               
      [102] "12 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> "                                                                                                               
      [103] "13 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> "                                                                                                               
      [104] "14 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> "                                                                                                               
      [105] "15 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> "                                                                                                               
      [106] "16 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> "                                                                                                               
      [107] "17 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> "                                                                                                               
      [108] ""                                                                                                                                                                                               
      [109] "$prep_recipe"                                                                                                                                                                                   
      [110] ""                                                                                                                                                                                               
      [111] "$prep_params"                                                                                                                                                                                   
      [112] "$prep_params$thres_log"                                                                                                                                                                         
      [113] "$prep_params$thres_log$value"                                                                                                                                                                   
      [114] "[1] 2"                                                                                                                                                                                          
      [115] ""                                                                                                                                                                                               
      [116] "$prep_params$thres_log$text"                                                                                                                                                                    
      [117] "[1] \"Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details).\""
      [118] ""                                                                                                                                                                                               
      [119] ""                                                                                                                                                                                               
      [120] "$prep_params$thres_count"                                                                                                                                                                       
      [121] "$prep_params$thres_count$value"                                                                                                                                                                 
      [122] "[1] NA"                                                                                                                                                                                         
      [123] ""                                                                                                                                                                                               
      [124] "$prep_params$thres_count$text"                                                                                                                                                                  
      [125] "[1] \"Not applicable.\""                                                                                                                                                                        
      [126] ""                                                                                                                                                                                               
      [127] ""                                                                                                                                                                                               
      [128] "$prep_params$thres_corr"                                                                                                                                                                        
      [129] "$prep_params$thres_corr$value"                                                                                                                                                                  
      [130] "[1] 0.9"                                                                                                                                                                                        
      [131] ""                                                                                                                                                                                               
      [132] "$prep_params$thres_corr$text"                                                                                                                                                                   
      [133] "[1] \"The applied cutoff for removal of variables due to high correlations was 0.9.\""                                                                                                          
      [134] ""                                                                                                                                                                                               
      [135] ""                                                                                                                                                                                               
      [136] "$prep_params$vars_keep_corr"                                                                                                                                                                    
      [137] "$prep_params$vars_keep_corr$value"                                                                                                                                                              
      [138] "[1] NA"                                                                                                                                                                                         
      [139] ""                                                                                                                                                                                               
      [140] "$prep_params$vars_keep_corr$text"                                                                                                                                                               
      [141] "[1] \"No variables were excluded specifically due to high correlation with the variables in \\\"vars_keep_corr\\\"\""                                                                           
      [142] ""                                                                                                                                                                                               
      [143] ""                                                                                                                                                                                               
      [144] "$prep_params$thres_lump"                                                                                                                                                                        
      [145] "$prep_params$thres_lump$value"                                                                                                                                                                  
      [146] "[1] 0.05"                                                                                                                                                                                       
      [147] ""                                                                                                                                                                                               
      [148] "$prep_params$thres_lump$text"                                                                                                                                                                   
      [149] "[1] \"Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). \""                                                                                                  
      [150] ""                                                                                                                                                                                               
      [151] ""                                                                                                                                                                                               
      [152] "$prep_params$imp_ignore"                                                                                                                                                                        
      [153] "$prep_params$imp_ignore$value"                                                                                                                                                                  
      [154] "[1] 0.8"                                                                                                                                                                                        
      [155] ""                                                                                                                                                                                               
      [156] "$prep_params$imp_ignore$text"                                                                                                                                                                   
      [157] "[1] \"Variables were dropped if the proportion of available data was less than 80%.\""                                                                                                          
      [158] ""                                                                                                                                                                                               
      [159] ""                                                                                                                                                                                               
      [160] "$prep_params$nzv"                                                                                                                                                                               
      [161] "$prep_params$nzv$value"                                                                                                                                                                         
      [162] "$prep_params$nzv$value$freq_cut"                                                                                                                                                                
      [163] "[1] 19"                                                                                                                                                                                         
      [164] ""                                                                                                                                                                                               
      [165] "$prep_params$nzv$value$unique_cut"                                                                                                                                                              
      [166] "[1] 10"                                                                                                                                                                                         
      [167] ""                                                                                                                                                                                               
      [168] ""                                                                                                                                                                                               
      [169] "$prep_params$nzv$text"                                                                                                                                                                          
      [170] "[1] \"Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10).\""                                                                           
      [171] ""                                                                                                                                                                                               
      [172] ""                                                                                                                                                                                               
      [173] "$prep_params$value"                                                                                                                                                                             
      [174] "[1] NA"                                                                                                                                                                                         
      [175] ""                                                                                                                                                                                               
      [176] "$prep_params$text"                                                                                                                                                                              
      [177] "[1] NA"                                                                                                                                                                                         
      [178] ""                                                                                                                                                                                               
      [179] ""                                                                                                                                                                                               
      [180] "$removed"                                                                                                                                                                                       
      [181] "$removed$rows"                                                                                                                                                                                  
      [182] "$removed$rows$outlier_outcome"                                                                                                                                                                  
      [183] "NULL"                                                                                                                                                                                           
      [184] ""                                                                                                                                                                                               
      [185] "$removed$rows$na_outcome"                                                                                                                                                                       
      [186] "NULL"                                                                                                                                                                                           
      [187] ""                                                                                                                                                                                               
      [188] "$removed$rows$na_feature"                                                                                                                                                                       
      [189] "NULL"                                                                                                                                                                                           
      [190] ""                                                                                                                                                                                               
      [191] ""                                                                                                                                                                                               
      [192] "$removed$cols"                                                                                                                                                                                  
      [193] "$removed$cols$rm"                                                                                                                                                                               
      [194] "  LAB1   LAB2   LAB3 "                                                                                                                                                                          
      [195] "\"LAB1\" \"LAB2\" \"LAB3\" "                                                                                                                                                                    
      [196] ""                                                                                                                                                                                               
      [197] "$removed$cols$nzv"                                                                                                                                                                              
      [198] "[1] \"cardiac_arrhythmias\""                                                                                                                                                                    
      [199] ""                                                                                                                                                                                               
      [200] "$removed$cols$corr"                                                                                                                                                                             
      [201] "[1] \"BMI_adsl\""                                                                                                                                                                               
      [202] ""                                                                                                                                                                                               
      [203] "$removed$cols$imp_ignore"                                                                                                                                                                       
      [204] "[1] \"LAB1\" \"LAB2\" \"LAB3\""                                                                                                                                                                 
      [205] ""                                                                                                                                                                                               
      [206] ""                                                                                                                                                                                               
      [207] ""                                                                                                                                                                                               
---

    Code
      ads_ml_surv %>% capture_output_lines(print = TRUE)
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
        [1] "$data_raw"                                                                                                                                                                                      
        [2] "$data_raw$train"                                                                                                                                                                                
        [3] "# A tibble: 216 x 19"                                                                                                                                                                           
        [4] "     .id .time .status .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3"                                                                                                               
        [5] "   <dbl> <dbl>   <dbl> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>"                                                                                                               
        [6] " 1 10018   270       0 PLC   at_lea~ M     WHITE    80    29.5    NA    NA    NA"                                                                                                               
        [7] " 2 10024    11       0 PLC   at_lea~ M     WHITE    83    24.4    NA    NA    NA"                                                                                                               
        [8] " 3 10026   174       0 PLC   at_lea~ F     WHITE    82    28.9    NA    NA    NA"                                                                                                               
        [9] " 4 10028    80       0 PLC   60_to_~ M     WHITE    66    34.5    NA    NA    NA"                                                                                                               
       [10] " 5 10036     8       0 PLC   60_to_~ F     WHITE    60    26      NA    NA    NA"                                                                                                               
       [11] " 6 10039     4       0 PLC   60_to_~ F     WHITE    71    23.8    NA    NA    NA"                                                                                                               
       [12] " 7 10043   270       0 PLC   60_to_~ M     WHITE    70    23.2    NA    NA    NA"                                                                                                               
       [13] " 8 10048   210       0 PLC   at_lea~ F     WHITE    76    24.8    NA    NA    NA"                                                                                                               
       [14] " 9 10067   270       0 PLC   60_to_~ F     WHITE    60    25      NA    NA    NA"                                                                                                               
       [15] "10 10071   122       0 PLC   under_~ F     WHITE    53    22.7    NA    NA    NA"                                                                                                               
       [16] "# ... with 206 more rows, 7 more variables: BMI_advs <dbl>, BPDIA <dbl>,"                                                                                                                       
       [17] "#   BPSYS <dbl>, HR <dbl>, WEIGHT <dbl>, cardiac_arrhythmias <int>,"                                                                                                                            
       [18] "#   coronary_artery_disorders <int>, and abbreviated variable name 1: BMI_adsl"                                                                                                                 
       [19] ""                                                                                                                                                                                               
       [20] "$data_raw$test"                                                                                                                                                                                 
       [21] "# A tibble: 73 x 19"                                                                                                                                                                            
       [22] "     .id .time .status .trt  AGEGR01 SEX   RACE    AGE BMI_a~1  LAB1  LAB2  LAB3"                                                                                                               
       [23] "   <dbl> <dbl>   <dbl> <fct> <fct>   <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>"                                                                                                               
       [24] " 1 10005    57       1 PLC   at_lea~ F     WHITE    76    29       3     4     4"                                                                                                               
       [25] " 2 10007   200       0 TRT   under_~ M     WHITE    53    33.3    NA    NA    NA"                                                                                                               
       [26] " 3 10009    63       1 PLC   at_lea~ F     WHITE    77    36.4    NA    NA    NA"                                                                                                               
       [27] " 4 10010    30       1 PLC   at_lea~ M     BLACK    88    29.9    NA    NA    NA"                                                                                                               
       [28] " 5 10015     4       1 PLC   at_lea~ M     WHITE    81    31.9    NA    NA    NA"                                                                                                               
       [29] " 6 10022    85       0 PLC   60_to_~ M     WHITE    64    24      NA    NA    NA"                                                                                                               
       [30] " 7 10025    18       1 TRT   under_~ F     WHITE    46    30.8    NA    NA    NA"                                                                                                               
       [31] " 8 10027    31       1 TRT   under_~ M     WHITE    53    24.1    NA    NA    NA"                                                                                                               
       [32] " 9 10030   135       0 PLC   60_to_~ F     WHITE    72    26.6    NA    NA    NA"                                                                                                               
       [33] "10 10031    32       1 PLC   under_~ F     <NA>     48    28      NA    NA    NA"                                                                                                               
       [34] "# ... with 63 more rows, 7 more variables: BMI_advs <dbl>, BPDIA <dbl>,"                                                                                                                        
       [35] "#   BPSYS <dbl>, HR <dbl>, WEIGHT <dbl>, cardiac_arrhythmias <int>,"                                                                                                                            
       [36] "#   coronary_artery_disorders <int>, and abbreviated variable name 1: BMI_adsl"                                                                                                                 
       [37] ""                                                                                                                                                                                               
       [38] ""                                                                                                                                                                                               
       [39] "$data_prep"                                                                                                                                                                                     
       [40] "$data_prep$train"                                                                                                                                                                               
       [41] "# A tibble: 216 x 14"                                                                                                                                                                           
       [42] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [43] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [44] " 1 10018 PLC   at_least_75    M     WHITE    80    29.5  86     128  64     95.1"                                                                                                               
       [45] " 2 10024 PLC   at_least_75    M     WHITE    83    24.4  65     133  60     64.2"                                                                                                               
       [46] " 3 10026 PLC   at_least_75    F     WHITE    82    28.9  68     102  68.6   95.6"                                                                                                               
       [47] " 4 10028 PLC   60_to_under_75 M     WHITE    66    34.5  75     134  67    112. "                                                                                                               
       [48] " 5 10036 PLC   60_to_under_75 F     WHITE    60    26    69.6   128  56     62.6"                                                                                                               
       [49] " 6 10039 PLC   60_to_under_75 F     WHITE    71    23.8  80      96  84     78.3"                                                                                                               
       [50] " 7 10043 PLC   60_to_under_75 M     WHITE    70    23.2  85     134  48     64.9"                                                                                                               
       [51] " 8 10048 PLC   at_least_75    F     WHITE    76    24.8  66     109  63     68  "                                                                                                               
       [52] " 9 10067 PLC   60_to_under_75 F     WHITE    60    25    69     135  76     67  "                                                                                                               
       [53] "10 10071 PLC   under_60       F     WHITE    53    22.7  61     124  76     65.4"                                                                                                               
       [54] "# ... with 206 more rows, 3 more variables: coronary_artery_disorders <int>,"                                                                                                                   
       [55] "#   .time <dbl>, .status <dbl>, and abbreviated variable name 1: BMI_advs"                                                                                                                      
       [56] ""                                                                                                                                                                                               
       [57] "$data_prep$test"                                                                                                                                                                                
       [58] "# A tibble: 73 x 14"                                                                                                                                                                            
       [59] "     .id .trt  AGEGR01        SEX   RACE    AGE BMI_a~1 BPDIA BPSYS    HR WEIGHT"                                                                                                               
       [60] "   <dbl> <fct> <fct>          <fct> <fct> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>"                                                                                                               
       [61] " 1 10005 PLC   at_least_75    F     WHITE    76    29      67   111    63   89.9"                                                                                                               
       [62] " 2 10007 TRT   under_60       M     WHITE    53    33.3    68   127    59   79.4"                                                                                                               
       [63] " 3 10009 PLC   at_least_75    F     WHITE    77    36.4    73   111    82  114. "                                                                                                               
       [64] " 4 10010 PLC   at_least_75    M     BLACK    88    29.9    71   121    60   81.9"                                                                                                               
       [65] " 5 10015 PLC   at_least_75    M     WHITE    81    31.9    81   122    67   96.1"                                                                                                               
       [66] " 6 10022 PLC   60_to_under_75 M     WHITE    64    24      54   101    80   73.4"                                                                                                               
       [67] " 7 10025 TRT   under_60       F     WHITE    46    30.8    73   117    70   85.4"                                                                                                               
       [68] " 8 10027 TRT   under_60       M     WHITE    53    24.1    80   152    69   50.2"                                                                                                               
       [69] " 9 10030 PLC   60_to_under_75 F     WHITE    72    26.6    78    95    71   71.9"                                                                                                               
       [70] "10 10031 PLC   under_60       F     WHITE    48    28      63   116    78   84.3"                                                                                                               
       [71] "# ... with 63 more rows, 3 more variables: coronary_artery_disorders <int>,"                                                                                                                    
       [72] "#   .time <dbl>, .status <dbl>, and abbreviated variable name 1: BMI_advs"                                                                                                                      
       [73] ""                                                                                                                                                                                               
       [74] ""                                                                                                                                                                                               
       [75] "$split"                                                                                                                                                                                         
       [76] "<Training/Testing/Total>"                                                                                                                                                                       
       [77] "<216/73/289>"                                                                                                                                                                                   
       [78] ""                                                                                                                                                                                               
       [79] "$outcome"                                                                                                                                                                                       
       [80] "$outcome$name"                                                                                                                                                                                  
       [81] "[1] \".time\"   \".status\""                                                                                                                                                                    
       [82] ""                                                                                                                                                                                               
       [83] "$outcome$mode"                                                                                                                                                                                  
       [84] "[1] \"survival\""                                                                                                                                                                               
       [85] ""                                                                                                                                                                                               
       [86] ""                                                                                                                                                                                               
       [87] "$dict"                                                                                                                                                                                          
       [88] "# A tibble: 18 x 8"                                                                                                                                                                             
       [89] "   param                     column       source label type  spec_id unit  logtr"                                                                                                               
       [90] "   <chr>                     <chr>        <chr>  <chr> <chr> <chr>   <chr> <chr>"                                                                                                               
       [91] " 1 .time                     .time        user_~ .time <NA>  <NA>    <NA>  <NA> "                                                                                                               
       [92] " 2 .status                   .status      user_~ .sta~ <NA>  <NA>    <NA>  <NA> "                                                                                                               
       [93] " 3 .trt                      .trt         SL     Actu~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [94] " 4 AGE                       AGE          SL     Age   adsl  adsl    <NA>  <NA> "                                                                                                               
       [95] " 5 AGEGR01                   AGEGR01      SL     Age ~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [96] " 6 SEX                       SEX          SL     Sex   adsl  adsl    <NA>  <NA> "                                                                                                               
       [97] " 7 RACE                      RACE         SL     Race  adsl  adsl    <NA>  <NA> "                                                                                                               
       [98] " 8 BMI_adsl                  BMI_adsl     SL     Body~ adsl  adsl    <NA>  <NA> "                                                                                                               
       [99] " 9 LAB1                      LAB1         LB     Labo~ bds   adlb    unit1 <NA> "                                                                                                               
      [100] "10 LAB2                      LAB2         LB     Labo~ bds   adlb    unit2 <NA> "                                                                                                               
      [101] "11 LAB3                      LAB3         LB     Labo~ bds   adlb    unit3 <NA> "                                                                                                               
      [102] "12 BMI_advs                  BMI_advs     VS     Body~ bds   advs    kg/m2 <NA> "                                                                                                               
      [103] "13 BPDIA                     BPDIA        VS     Dias~ bds   advs    mmHg  <NA> "                                                                                                               
      [104] "14 BPSYS                     BPSYS        VS     Syst~ bds   advs    mmHg  <NA> "                                                                                                               
      [105] "15 HR                        HR           VS     Hear~ bds   advs    beat~ <NA> "                                                                                                               
      [106] "16 WEIGHT                    WEIGHT       VS     Weig~ bds   advs    kg    <NA> "                                                                                                               
      [107] "17 cardiac_arrhythmias       cardiac_arr~ MH     Card~ occds admh    <NA>  <NA> "                                                                                                               
      [108] "18 coronary_artery_disorders coronary_ar~ MH     Coro~ occds admh    <NA>  <NA> "                                                                                                               
      [109] ""                                                                                                                                                                                               
      [110] "$prep_recipe"                                                                                                                                                                                   
      [111] ""                                                                                                                                                                                               
      [112] "$prep_params"                                                                                                                                                                                   
      [113] "$prep_params$thres_log"                                                                                                                                                                         
      [114] "$prep_params$thres_log$value"                                                                                                                                                                   
      [115] "[1] 2"                                                                                                                                                                                          
      [116] ""                                                                                                                                                                                               
      [117] "$prep_params$thres_log$text"                                                                                                                                                                    
      [118] "[1] \"Variables were log transformed (base e) if e1071::skewness() > 2. Variables that are assumed to be count variables were excluded from the transformation (see thres_count for details).\""
      [119] ""                                                                                                                                                                                               
      [120] ""                                                                                                                                                                                               
      [121] "$prep_params$thres_count"                                                                                                                                                                       
      [122] "$prep_params$thres_count$value"                                                                                                                                                                 
      [123] "[1] NA"                                                                                                                                                                                         
      [124] ""                                                                                                                                                                                               
      [125] "$prep_params$thres_count$text"                                                                                                                                                                  
      [126] "[1] \"Not applicable.\""                                                                                                                                                                        
      [127] ""                                                                                                                                                                                               
      [128] ""                                                                                                                                                                                               
      [129] "$prep_params$thres_corr"                                                                                                                                                                        
      [130] "$prep_params$thres_corr$value"                                                                                                                                                                  
      [131] "[1] 0.9"                                                                                                                                                                                        
      [132] ""                                                                                                                                                                                               
      [133] "$prep_params$thres_corr$text"                                                                                                                                                                   
      [134] "[1] \"The applied cutoff for removal of variables due to high correlations was 0.9.\""                                                                                                          
      [135] ""                                                                                                                                                                                               
      [136] ""                                                                                                                                                                                               
      [137] "$prep_params$vars_keep_corr"                                                                                                                                                                    
      [138] "$prep_params$vars_keep_corr$value"                                                                                                                                                              
      [139] "[1] NA"                                                                                                                                                                                         
      [140] ""                                                                                                                                                                                               
      [141] "$prep_params$vars_keep_corr$text"                                                                                                                                                               
      [142] "[1] \"No variables were excluded specifically due to high correlation with the variables in \\\"vars_keep_corr\\\"\""                                                                           
      [143] ""                                                                                                                                                                                               
      [144] ""                                                                                                                                                                                               
      [145] "$prep_params$thres_lump"                                                                                                                                                                        
      [146] "$prep_params$thres_lump$value"                                                                                                                                                                  
      [147] "[1] 0.05"                                                                                                                                                                                       
      [148] ""                                                                                                                                                                                               
      [149] "$prep_params$thres_lump$text"                                                                                                                                                                   
      [150] "[1] \"Low frequency factor levels were lumped using recipes::step_other(threshold = 0.05). \""                                                                                                  
      [151] ""                                                                                                                                                                                               
      [152] ""                                                                                                                                                                                               
      [153] "$prep_params$imp_ignore"                                                                                                                                                                        
      [154] "$prep_params$imp_ignore$value"                                                                                                                                                                  
      [155] "[1] 0.8"                                                                                                                                                                                        
      [156] ""                                                                                                                                                                                               
      [157] "$prep_params$imp_ignore$text"                                                                                                                                                                   
      [158] "[1] \"Variables were dropped if the proportion of available data was less than 80%.\""                                                                                                          
      [159] ""                                                                                                                                                                                               
      [160] ""                                                                                                                                                                                               
      [161] "$prep_params$nzv"                                                                                                                                                                               
      [162] "$prep_params$nzv$value"                                                                                                                                                                         
      [163] "$prep_params$nzv$value$freq_cut"                                                                                                                                                                
      [164] "[1] 19"                                                                                                                                                                                         
      [165] ""                                                                                                                                                                                               
      [166] "$prep_params$nzv$value$unique_cut"                                                                                                                                                              
      [167] "[1] 10"                                                                                                                                                                                         
      [168] ""                                                                                                                                                                                               
      [169] ""                                                                                                                                                                                               
      [170] "$prep_params$nzv$text"                                                                                                                                                                          
      [171] "[1] \"Highly sparse and unbalanced variables were dropped using recipes::step_nzv(freq_cut = 19, unique_cut = 10).\""                                                                           
      [172] ""                                                                                                                                                                                               
      [173] ""                                                                                                                                                                                               
      [174] ""                                                                                                                                                                                               
      [175] "$removed"                                                                                                                                                                                       
      [176] "$removed$rows"                                                                                                                                                                                  
      [177] "$removed$rows$outlier_outcome"                                                                                                                                                                  
      [178] "NULL"                                                                                                                                                                                           
      [179] ""                                                                                                                                                                                               
      [180] "$removed$rows$na_outcome"                                                                                                                                                                       
      [181] "NULL"                                                                                                                                                                                           
      [182] ""                                                                                                                                                                                               
      [183] "$removed$rows$na_feature"                                                                                                                                                                       
      [184] "NULL"                                                                                                                                                                                           
      [185] ""                                                                                                                                                                                               
      [186] ""                                                                                                                                                                                               
      [187] "$removed$cols"                                                                                                                                                                                  
      [188] "$removed$cols$rm"                                                                                                                                                                               
      [189] "  LAB1   LAB2   LAB3 "                                                                                                                                                                          
      [190] "\"LAB1\" \"LAB2\" \"LAB3\" "                                                                                                                                                                    
      [191] ""                                                                                                                                                                                               
      [192] "$removed$cols$nzv"                                                                                                                                                                              
      [193] "[1] \"cardiac_arrhythmias\""                                                                                                                                                                    
      [194] ""                                                                                                                                                                                               
      [195] "$removed$cols$corr"                                                                                                                                                                             
      [196] "[1] \"BMI_adsl\""                                                                                                                                                                               
      [197] ""                                                                                                                                                                                               
      [198] "$removed$cols$imp_ignore"                                                                                                                                                                       
      [199] "[1] \"LAB1\" \"LAB2\" \"LAB3\""                                                                                                                                                                 
      [200] ""                                                                                                                                                                                               
      [201] ""                                                                                                                                                                                               
      [202] ""                                                                                                            