library(tidymodels)

svm_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/svm.rds")){
    model <- readRDS("out/svm.rds")
    
    return(model)
  }  
  
  svm_model <-
    svm_poly(cost = tune(),
             degree= tune(),
             scale_factor= tune()
             ) %>%
    set_mode("classification")
  
  svm_grid <- grid_max_entropy(cost(),
                               degree(),
                               scale_factor(),
                               size = grid.size)
  
  svm_workflow <- workflow() %>%
    add_model(svm_model) %>%
    add_formula(ofi ~ .)
  
  
  svm_tune <- svm_workflow %>%
    tune_grid(
      resamples = folds,
      grid = svm_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    svm_model %>%
    finalize_model(select_best(svm_tune))
  
  saveRDS(tuned_model, "out/svm.rds")
  
  return(tuned_model)
}