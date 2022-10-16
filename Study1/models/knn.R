library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

knn_hyperopt <- function(data) {
  if(file.exists("out/knn.rds")){
    model <- readRDS("out/knn.rds")
    
    return(model)
  }  
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
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
                               size = 30)
  
  knn_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(knn_model)
  
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
  
  print(show_best(knn_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/knn.rds")
  
  return(tuned_model)
}
