library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')
dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')
#dataset <- combined.datasets
dataset <- dataset[, -grep("VK_", colnames(dataset))]
dataset <- dataset[, -grep("ais_", colnames(dataset))]
dataset <- dataset[, -grep("icd_", colnames(dataset))]
dataset <- dataset[, -grep("pac_", colnames(dataset))]
dataset <- dataset[, -grep("filter_", colnames(dataset))]
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

lr_hyperopt <- function(data) {
  set.seed(2022)
  
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

# auc: 0.8078382
#penalty = 0.0013578618558214
#mixture = 1
hyperopt_lr <- lr_hyperopt(dataset)
