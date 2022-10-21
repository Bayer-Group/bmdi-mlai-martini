test_that("build_out_tte works", {
  
  cut  <- 20
  unit <- 'days'
  data <- tibble::tribble(
    ~SUBJID, ~AVAL, ~CNSR,
      10001, 10,    0,
      10002, 30,    0,
      10003, 10,    1,
      10004, 40,    1
  ) %>% 
    dplyr::mutate(PARAMCD = 'A') %>% 
    dplyr::bind_rows(
      .,
      dplyr::mutate(., PARAMCD = 'B') %>% 
        dplyr::mutate(CNSR = 1 - CNSR)
    )
  
  # discard subjects censored before cut
  testthat::expect_true(
    !
    #data %>% 
    #  filter(AVAL < cut & CNSR == 1) %>% 
    #  pull(SUBJID)
    '10003'
    
    %in%
    
    build_out_tte(
      data = data, 
      filter = 'PARAMCD == "A"', 
      cut = cut
    )$.id
  
  )
  
  
  testthat::expect_setequal(
    c('.id', '.out'),
    build_out_tte(
      data = data, 
      filter = 'PARAMCD == "A"', 
      cut = cut
    ) %>% names()
  )
  
  testthat::expect_setequal(
    c('.id', '.time', '.status'),
    build_out_tte(
      data = data, 
      filter = 'PARAMCD == "A"'
    ) %>% names()
  )
  
})
