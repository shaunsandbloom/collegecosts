library(tidyverse)
library(tidymodels)
library(jsonlite)
library(httr)
library(rscorecard)
library(sf)
library(tigris)
library(tidycensus)
library(usmap)
library(recipes)
library(rpart)

#### PRE-PROCESSING ####

# setting processing performance
doParallel::registerDoParallel(cores = 4)

# partitioning data
predictions <- master_final %>% 
  select(-id, -location.lat, -location.lon)
split <- initial_split(predictions, prop = 0.75)
m_training <- training(split)
m_testing <- testing(split)

# creating folds
folds <- vfold_cv(data = m_training, v = 10)

#### RECIPE ####

# creating recipe
cost_recipe <- recipe(tuition_rate ~ ., data = m_training) %>% 
  step_nzv(all_predictors())


#### LINEAR REGRESSION ####

cost_lm <- linear_reg() %>% 
  set_engine("lm")

cost_lm_wf <- workflow() %>% 
  add_recipe(cost_recipe) %>% 
  add_model(cost_lm)

cost_lm_cv <- cost_lm_wf %>% 
  fit_resamples(resamples = folds)

cost_lm_metrics <- cost_lm_cv %>% 
  collect_metrics(summarize = FALSE) %>% 
  filter(.estimate == 'rmse')

#### K-NEAREST NEIGHBORS ####

cost_knn <- nearest_neighbor() %>% 
  set_engine('kknn')

cost_knn_wf <- workflow() %>% 
  add_recipe(cost_recipe) %>% 
  add_model(cost_knn)

cost_knn_cv <- cost_knn_wf() %>% 
  fit_resamples(resamples = folds) %>% 

cost_knn_metrics <- cost_knn_cv %>%   
  collect_metrics(summarize = FALSE) %>% 
  filter(.estimate == 'rmse')

#### SAVE CV OBJECTS ####

save(cost_lm_cv, file = "objects/cost_lm_cv.rda")
save(cost_knn_cv, file = "objects/cost_knn_cv.rda")
save(cost_lasso_cv, file = "objects/cost_lasso_cv.rda")

#### VISUALIZE ALL METHODS ####

all_metrics <- bind_rows(`lm` = cost_lm_metrics,
                         `knn` = cost_knn_metrics,
                         `lasso` = cost_lasso_metrics,
                         .id = 'model')

ggplot(all_metrics, aes(id, .estimate, group = .estimator, color = model)) +
  geom_line() +
  geom_point() +
  labs(title = 'Calculated RMSE for all Models',
       y = 'Model RMSE') +
  theme_minimal()

# FOR PROJECT...
# LM is being validated across 5 folds...
# KNN is being validated across 10 folds...
# BOTH will be plotted...
# it would be nice to explore the coefficients of most optimal model for each approach...