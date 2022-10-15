library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

xgb_hyperopt <- function(data) {
  if(file.exists("out/xgb.rds")){
    model <- readRDS("out/xgb.rds")
    
    return(model)
  }  
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
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
    finalize(mtry(), data),
    learn_rate(),
    size = 30
  )
  
  xgb_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(xgb_model)
  
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
  
  print(show_best(xgb_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/xgb.rds")
  
  return(tuned_model)
}
