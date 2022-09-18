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

rf_hyperopt <- function(data) {
  set.seed(2022)
  
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
  
  return(tuned_model)
}

# auc: 0.8183015
#mtry = 8
#trees = 1099
#min_n = 7

hyperopt_rf <- rf_hyperopt(dataset)
