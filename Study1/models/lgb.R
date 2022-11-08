library(tidymodels)
library(treesnip)
library(doParallel)

lgb_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/lgb.rds")){
    model <- readRDS("out/lgb.rds")
    
    return(model)
  }    
  
  lgb_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      #sample_size = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("lightgbm", nthread = 1) %>%
    set_mode("classification")
 
  lgb_grid <- grid_max_entropy(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    #sample_size = sample_prop(c(0.4, 0.9)),
    finalize(mtry(), hyperopt.folds$splits[[1]]$data),
    learn_rate(),
    size = grid.size
  )
  
  lgb_workflow <- workflow() %>%
    add_model(lgb_model) %>%
    add_formula(ofi ~ .)
  
  
  lgb_tune <- lgb_workflow %>%
    tune_grid(
      resamples = folds,
      grid = lgb_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    lgb_model %>%
    finalize_model(select_best(lgb_tune))
  
  saveRDS(tuned_model, "out/lgb.rds")
  
  return(tuned_model)
}
