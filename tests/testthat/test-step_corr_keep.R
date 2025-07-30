test_that("step_corr_keep works", {
  
  require(modeldata)
  require(recipes)
  
  set.seed(3535)
  biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))

  biomass_tr <- biomass[biomass$dataset == "Training", ]
  biomass_te <- biomass[biomass$dataset == "Testing", ]

  rec <- recipe(
    HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + duplicate,
    data = biomass_tr
  )

  corr_filter <- rec |>
    step_corr_keep(all_numeric_predictors(), threshold = .5)

  filter_obj <- prep(corr_filter, training = biomass_tr)

  filtered_te <- bake(filter_obj, biomass_te)
  round(abs(cor(biomass_tr[, c(3:7, 9)])), 2)
  round(abs(cor(filtered_te)), 2)

  tidy(corr_filter, number = 1)
  tidy(filter_obj, number = 1)
  
  # 
  # x_orig <- martini::martini_feat %>% 
  #   na.omit()
  # 
  # 
  # 
  # expect_equal(2 * 2, 4)
})
