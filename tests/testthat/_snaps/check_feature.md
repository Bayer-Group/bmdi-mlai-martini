# check_feature() works

    Code
      check_feature(martini_feat, thres_low_freq = 15) %>% purrr::modify_tree(leaf = tibble_to_JSON)
    Message
      i Data set was checked for causes for potential downstream issues with ML preparation.
      ! Potential issues were identified.
      * Run check_freq() and check_nzv() on the input to `prepare_ml()`'s `feature` to learn more.
    Output
      $summary
             check_freq() check_other_class() check_non_missing()         check_nzv() 
                     TRUE               FALSE               FALSE                TRUE 
            check_count() 
                    FALSE 
      
      $details
      $details$low_freq
      $details$low_freq$vars
      [1] "angina_pectoris"
      
      $details$low_freq$counts
      $details$low_freq$counts$angina_pectoris
      [
        {
          "fct": "no",
          "n": 275
        },
        {
          "fct": "yes",
          "n": 14
        }
      ] 
      
      
      $details$low_freq$overall_min
      angina_pectoris 
                   14 
      
      $details$low_freq$finding
      [1] TRUE
      
      $details$low_freq$threshold
      [1] 15
      
      $details$low_freq$check
      [1] "check_freq()"
      
      
      $details$other
      $details$other$vars
      character(0)
      
      $details$other$counts
      named list()
      
      $details$other$finding
      [1] FALSE
      
      $details$other$class
      [1] "other_ml"
      
      $details$other$check
      [1] "check_other_class()"
      
      
      $details$missing
      $details$missing$vars
      character(0)
      
      $details$missing$prop_missing
      named numeric(0)
      
      $details$missing$finding
      [1] FALSE
      
      $details$missing$threshold
      [1] 0.8
      
      $details$missing$check
      [1] "check_non_missing()"
      
      
      $details$nzv
      $details$nzv$vars
      $details$nzv$vars$constant
      character(0)
      
      $details$nzv$vars$nzv
      [1] "angina_pectoris"
      
      
      $details$nzv$finding
      [1] TRUE
      
      $details$nzv$threshold
      unique   freq 
          10     19 
      
      $details$nzv$check
      [1] "check_nzv()"
      
      
      $details$count
      $details$count$vars
      character(0)
      
      $details$count$finding
      [1] FALSE
      
      $details$count$threshold
      [1] 30
      
      $details$count$check
      [1] "check_count()"
      
      
      

# check_freq() works

    Code
      res2 <- x %>% dplyr::select(-fct_risky) %>% check_freq(thres = thres)
    Message
      No factors with low frequency class (<10) detected in data set.

---

    Code
      check_freq(x, thres = thres)
    Message
      i The following factor has low frequencies (<10) in at least one class: fct_risky

# check_other_class() works

    Code
      x %>% purrr::modify_tree(leaf = tibble_to_JSON)
    Output
      $vars
      [1] "incorporate" "clash"      
      
      $counts
      $counts$incorporate
      [
        {
          "f": "large",
          "n": 3
        },
        {
          "f": "collapse",
          "n": 1
        },
        {
          "f": "other_ml",
          "n": 1
        }
      ] 
      
      $counts$clash
      [
        {
          "f": "other_ml",
          "n": 3
        },
        {
          "f": "collapse1",
          "n": 1
        },
        {
          "f": "collapse2",
          "n": 1
        }
      ] 
      
      
      $finding
      [1] TRUE
      
      $class
      [1] "other_ml"
      
      $check
      [1] "check_other_class()"
      

