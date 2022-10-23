library(tidymodels)
library(doParallel)
library(baguette)

dt_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/dt.rds")){
    model <- readRDS("out/dt.rds")
    
    return(model)
  }  
  
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
                              size = grid.size)
  
  dt_workflow <- workflow() %>%
    add_model(dt_model) %>%
    add_formula(ofi ~ .)
  
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
  
  saveRDS(tuned_model, "out/dt.rds")
  
  return(tuned_model)
}
