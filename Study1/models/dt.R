library(tidymodels)
library(doParallel)
library(baguette)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

dt_hyperopt <- function(data) {
  set.seed(2022)
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  dt_model <-
    bag_tree(
      cost_complexity = tune(),
      tree_depth = tune(),
      min_n = tune(),
      class_cost = 33
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  dt_grid <- grid_max_entropy(cost_complexity(),
                              tree_depth(),
                              min_n(),
                              size = 30)
  
  dt_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(dt_model)
  
  dt_tune <- dt_workflow %>%
    tune_grid(
      resamples = folds,
      grid = dt_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    dt_model %>%
    finalize_model(select_best(dt_tune))
  
  print(show_best(dt_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  return(tuned_model)
}
