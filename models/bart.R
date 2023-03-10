library(tidymodels)

bart_hyperopt <- function(data) {
  if(file.exists("out/bart.rds")){
    model <- readRDS("out/bart.rds")
    
    return(model)
  }
  
  set.seed(2022)
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  bart_model <-
    bart(trees=tune()) %>%
    set_mode("classification")
  
  bart_grid <- grid_max_entropy(trees(),
                              size = 30)
  
  bart_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(bart_model)
  
  bart_tune <- bart_workflow %>%
    tune_grid(
      resamples = folds,
      grid = bart_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    bart_model %>%
    finalize_model(select_best(bart_tune))
  
  print(show_best(bart_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  saveRDS(tuned_model, "out/bart.rds")
  
  return(tuned_model)
}
