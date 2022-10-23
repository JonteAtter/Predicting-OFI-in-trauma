library(tidymodels)
library(treesnip)
library(doParallel)

lgb_hyperopt <- function(data, grid.size = 30, n.folds = 5) {
  if(file.exists("out/lgb.rds")){
    model <- readRDS("out/lgb.rds")
    
    return(model)
  }    
  
  folds <- vfold_cv(data, v = n.folds, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
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
    set_engine("lightgbm") %>%
    set_mode("classification")
  
  lgb_grid <- grid_max_entropy(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    #sample_size = sample_prop(c(0.4, 0.9)),
    finalize(mtry(), data),
    learn_rate(),
    size = grid.size
  )
  
  lgb_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(lgb_model)
  
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
  
  print(show_best(lgb_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/lgb.rds")
  
  return(tuned_model)
}