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

