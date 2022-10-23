library(tidymodels)
library(doParallel)
library(tidypredict)

lr_hyperopt <- function(data, grid.size = 30, n.folds = 5) {
  if(file.exists("out/lr.rds")){
    model <- readRDS("out/lr.rds")
    
    return(model)
  }
  
  folds <- vfold_cv(data, v = n.folds, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  lr_model <-
    logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  lr_grid <- grid_max_entropy(penalty(),
                              size = grid.size)
  
  lr_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(lr_model)
  
  lr_tune <- lr_workflow %>%
    tune_grid(
      resamples = folds,
      grid = lr_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    lr_model %>%
    finalize_model(select_best(lr_tune))
  
  print(show_best(lr_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/lr.rds")
  
  return(tuned_model)
}