library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')
dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')

dataset <- dataset[,-grep("VK_", colnames(dataset))]
dataset <- dataset[,-grep("ais_", colnames(dataset))]
dataset <- dataset[,-grep("icd_", colnames(dataset))]
dataset <- dataset[,-grep("pac_", colnames(dataset))]
dataset <- dataset[,-grep("filter_", colnames(dataset))]
dataset <-
  subset(
    dataset,
    select = -c(
      iva_dagar_n,
      iva_vardtillfallen_n,
      waran_beh_vid_ank,
      noak_vid_ankomst,
      fold,
      ofi_raw
    )
  )

dataset$ofi <- as.factor(dataset$ofi)

data <- dataset

knn_hyperopt <- function(data) {
  set.seed(2022)
  
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
  
  return(tuned_model)
}

# auc: 0.6605978
#neighbors = 10
#weight_func = biweight
#dist_power = 1.74020659551024
hyperopt_knn <- knn_hyperopt(dataset)
