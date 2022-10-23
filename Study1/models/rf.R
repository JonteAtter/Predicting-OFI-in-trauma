library(tidymodels)
library(doParallel)

rf_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/rf.rds")){
    model <- readRDS("out/rf.rds")
    
    return(model)
  }  
  

  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data, importance = "impurity")
  
  rf_model <-
    rand_forest(mtry = tune(),     ### lägg till importance
                trees = tune(),
                min_n = tune()) %>%
    set_mode("classification") %>% 
    set_engine("ranger",importance = "impurity")
  
  rf_grid <- grid_max_entropy(mtry(range = c(4, 9)),
                              trees(),
                              min_n(),
                              size = grid.size)
  
  rf_workflow <- workflow() %>%
    add_model(rf_model) %>%
    add_formula(ofi ~ .)
  
  
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
  
  saveRDS(tuned_model, "out/rf.rds")
  
  return(tuned_model)
}
