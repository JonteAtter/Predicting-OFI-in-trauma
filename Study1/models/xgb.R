library(tidymodels)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

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

xgb_hyperopt <- function(data) {
  set.seed(2022)
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  xgb_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  xgb_grid <- grid_max_entropy(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), data),
    learn_rate(),
    size = 30
  )
  
  xgb_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(xgb_model)
  
  xgb_tune <- xgb_workflow %>%
    tune_grid(
      resamples = folds,
      grid = xgb_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    xgb_model %>%
    finalize_model(select_best(xgb_tune))
  
  print(show_best(xgb_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  return(tuned_model)
}

#auc: 0.8168925
#mtry = 85
#trees = 980
#min_n = 8
#tree_depth = 15
#learn_rate = 0.00473405086467338
#loss_reduction = 8.90862223763915e-06
#sample_size = 0.625788101670332

hyperopt_xgb <- xgb_hyperopt(dataset)
