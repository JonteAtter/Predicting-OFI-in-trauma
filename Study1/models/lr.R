library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

lr_hyperopt <- function(data) {
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  lr_model <-
    logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  lr_grid <- grid_max_entropy(penalty(),
                              size = 30)
  
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
  
  return(tuned_model)
}