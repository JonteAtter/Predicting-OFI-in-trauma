library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

rf_hyperopt <- function(data) {
  if(file.exists("out/rf.rds")){
    model <- readRDS("out/rf.rds")
    
    return(model)
  }  
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  rf_model <-
    rand_forest(mtry = tune(),
                trees = tune(),
                min_n = tune()) %>%
    set_mode("classification")
  
  rf_grid <- grid_max_entropy(mtry(range = c(4, 9)),
                              trees(),
                              min_n(),
                              size = 30)
  
  rf_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(rf_model)
  
  rf_tune <- rf_workflow %>%
    tune_grid(
      resamples = folds,
      grid = rf_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    rf_model %>%
    finalize_model(select_best(rf_tune))
  
  print(show_best(rf_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/rf.rds")
  
  return(tuned_model)
}
