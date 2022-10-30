library(tidymodels)
library(doParallel)

xgb_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/xgb.rds")){
    model <- readRDS("out/xgb.rds")
    
    return(model)
  }  
  
  xgb_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  xgb_grid <- grid_max_entropy(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), hyperopt.folds$splits[[1]]$data),
    learn_rate(),
    size = grid.size
  )
  
  xgb_workflow <- workflow() %>%
    add_model(xgb_model) %>%
    add_formula(ofi ~ .)
  
  
  xgb_tune <- xgb_workflow %>%
    tune_grid(
      resamples = folds,
      grid = xgb_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    xgb_model %>%
    finalize_model(select_best(xgb_tune))
  
  saveRDS(tuned_model, "out/xgb.rds")
  
  return(tuned_model)
}
