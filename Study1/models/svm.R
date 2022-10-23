library(tidymodels)
library(doParallel)

svm_hyperopt <- function(data, grid.size = 30, n.folds = 5) {
  if(file.exists("out/svm.rds")){
    model <- readRDS("out/svm.rds")
    
    return(model)
  }  
  
  folds <- vfold_cv(data, v = n.folds, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  # svm_linear
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
    add_recipe(rec_obj) %>%
    add_model(svm_model)
  
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
  
  print(show_best(svm_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/svm.rds")
  
  return(tuned_model)
}