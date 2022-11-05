library(tidymodels)
library(tidypredict)

lr_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/lr.rds")){
    model <- readRDS("out/lr.rds")
    
    return(model)
  }
  
  lr_model <-
    logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  lr_grid <- grid_max_entropy(penalty(),
                              size = grid.size)
  
  lr_workflow <- workflow() %>%
    add_model(lr_model) %>%
    add_formula(ofi ~ .)
  
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
  
  saveRDS(tuned_model, "out/lr.rds")
  
  return(tuned_model)
}