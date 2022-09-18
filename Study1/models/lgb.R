library(tidymodels)
library(treesnip)
library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')
dataset <-
  read.csv(file = 'data/ofi.onehotencoded.imputed.standardised.csv')

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

lgb_hyperopt <- function(data) {
  set.seed(2022)
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  lgb_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      #sample_size = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("lightgbm") %>%
    set_mode("classification")
  
  lgb_grid <- grid_max_entropy(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    #sample_size = sample_prop(c(0.4, 0.9)),
    finalize(mtry(), data),
    learn_rate(),
    size = 30
  )
  
  lgb_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(lgb_model)
  
  lgb_tune <- lgb_workflow %>%
    tune_grid(
      resamples = folds,
      grid = lgb_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    lgb_model %>%
    finalize_model(select_best(lgb_tune))
  
  print(show_best(lgb_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  return(tuned_model)
}

hyperopt_lgb <- lgb_hyperopt(dataset)
