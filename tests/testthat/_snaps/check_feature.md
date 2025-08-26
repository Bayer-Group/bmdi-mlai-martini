# check_feature() works

    Code
      check_feature(martini_feat, thres_low_freq = 15)
    Output
      $low_freq
      $low_freq$vars
      [1] "angina_pectoris"
      
      $low_freq$counts
      $low_freq$counts$angina_pectoris
      # A tibble: 2 x 2
        fct       n
        <fct> <int>
      1 no      275
      2 yes      14
      
      
      $low_freq$overall_min
      angina_pectoris 
                   14 
      
      $low_freq$finding
      [1] TRUE
      
      $low_freq$threshold
      [1] 15
      
      $low_freq$check
      [1] "check_freq()"
      
      
      $other
      $other$vars
      character(0)
      
      $other$counts
      named list()
      
      $other$finding
      [1] FALSE
      
      $other$class
      [1] "other_ml"
      
      $other$check
      [1] "check_other_class()"
      
      
      $missing
      $missing$vars
      character(0)
      
      $missing$prop_missing
      named numeric(0)
      
      $missing$finding
      [1] FALSE
      
      $missing$threshold
      [1] 0.8
      
      $missing$check
      [1] "check_non_missing()"
      
      
      $count
      $count$vars
      [1] "AGE"   "BPDIA" "BPSYS" "HR"   
      
      $count$finding
      [1] TRUE
      
      $count$threshold
      [1] 30
      
      $count$check
      [1] "check_count()"
      
      

