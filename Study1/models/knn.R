library(tidymodels)

knn_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/knn.rds")){
    model <- readRDS("out/knn.rds")
    
    return(model)
  }  
  
  knn_model <-
    nearest_neighbor(
      neighbors = tune(),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
    set_mode("classification")
  
  knn_grid <- grid_max_entropy(neighbors(),
                               weight_func(),
                               dist_power(),
                               size = grid.size)
  
  knn_workflow <- workflow() %>%
    add_model(knn_model) %>%
    add_formula(ofi ~ .)
  
  knn_tune <- knn_workflow %>%
    tune_grid(
      resamples = folds,
      grid = knn_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
      
    )
  
  tuned_model <-
    knn_model %>%
    finalize_model(select_best(knn_tune))
  
  saveRDS(tuned_model, "out/knn.rds")
  
  return(tuned_model)
}
