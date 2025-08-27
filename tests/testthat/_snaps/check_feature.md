# check_feature() works

    Code
      check_feature(martini_feat, thres_low_freq = 15)
    Message
      i Data set was checked for causes for potential downstream issues with ML preparation.
      ! Potential issues were identified.
      * Run check_freq() and check_count() on the input to `prepare_ml()`'s `feature` to learn more.

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
      

