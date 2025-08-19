
library(tidymodels)

set.seed(1409)

# modify the `taxi` data set from {modeldata}
# - add an 'id' column
# - add random NAs to the 'distance' variable
taxi_na <- taxi %>% 
  mutate(id = stringr::str_pad(1:n(), width = 6, pad = "0"), .before = 1) %>% 
  mutate(distance = if_else(
    rbinom(n = n(), size = 1, prob = 0.01) == 1, 
    NA_real_,
    distance
  )) 

taxi_na %>% 
  imap(~{
    tibble(variable = .y, n_na = sum(is.na(.x)))
  }) %>% 
  list_rbind()

# draw a random subset
taxi_split <- initial_split(taxi_na, prop = 1/10)
taxi_train <- training(taxi_split)

# create a recipe that omits rows with missing outcome and set `skip = FALSE`
# (defaults to TRUE)
rcp <- recipe(distance~., data = taxi_train, strings_as_factors = TRUE) %>% 
  update_role(id, new_role = "id") %>% 
  step_naomit(all_outcomes(), skip = FALSE) %>% 
  step_normalize(all_numeric_predictors())

# ... unprepared recipe: skip = FALSE
tidy(rcp)

# ... prepared recipe: skip = FALSE
rcp_prepped <- prep(rcp)
tidy(rcp_prepped)

# set skip to TRUE for naomit step
rcp_skip <- rcp
rcp_skip$steps[[1]]$skip <- TRUE

# ... unprepared recipe: skip = TRUE
tidy(rcp_skip)

# ... prepared recipe: skip = TRUE
rcp_prepped_skip <- prep(rcp_skip)
tidy(rcp_prepped_skip)

# BAKE

# ... outcome column present
bake(rcp_prepped, new_data = NULL)
bake(rcp_prepped_skip, new_data = NULL)

# ... outcome column absent
bake(rcp_prepped, new_data = taxi_train %>% select(-distance))
bake(rcp_prepped_skip, new_data = taxi_train %>% select(-distance))

# USING WORKFLOWS

mod <- rand_forest(mode = "regression", engine = "ranger", trees = 100)

wf <- workflow(
  preprocessor = rcp,
  spec = mod
)

wf_skip <- wf %>% update_recipe(rcp_skip)

# ... outcome column present

fit(wf, data = taxi_train)

fit(wf_skip, data = taxi_train)

# ... outcome column absent

fit(wf, data = taxi_train %>% select(-distance))

fit(wf_skip, data = taxi_train %>% select(-distance))

