library(tidymodels)
# remotes::install_github("curso-r/treesnip@catboost")
library(treesnip)
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

cat_hyperopt <- function(data) {
  set.seed(2022)
  
  folds <- vfold_cv(data, v = 5, strata = ofi)
  
  rec_obj <- recipe(ofi ~ ., data = data)
  
  cat_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      #loss_reduction = tune(),
      #sample_size = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("catboost") %>%
    set_mode("classification")
  
  cat_grid <- grid_max_entropy(trees(),
                               tree_depth(),
                               min_n(),
                               #loss_reduction(),
                               #sample_size = sample_prop(),
                               finalize(mtry(), data),
                               learn_rate(),
                               size = 30)
  
  cat_workflow <- workflow() %>%
    add_recipe(rec_obj) %>%
    add_model(cat_model)
  
  cat_tune <- cat_workflow %>%
    tune_grid(
      resamples = folds,
      grid = cat_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(verbose = TRUE)
    )
  
  tuned_model <-
    cat_model %>%
    finalize_model(select_best(cat_tune))
  
  print(show_best(cat_tune, "roc_auc")$mean[1])
  print(tuned_model)
  
  return(tuned_model)
}

#auc: 0.8235009
#mtry = 126
#trees = 1369
#min_n = 15
#tree_depth = 12
#learn_rate = 0.00500591309652896
hyperopt_cat <- cat_hyperopt(dataset)
