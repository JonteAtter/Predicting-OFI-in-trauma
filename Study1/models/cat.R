library(tidymodels)
# remotes::install_github("curso-r/treesnip@catboost")
library(treesnip)
library(doParallel)
library(yaml)

cat_hyperopt <- function(folds, grid.size = 30) {
  if(file.exists("out/cat.rds")){
    model <- readRDS("out/cat.rds")
    
    return(model)
  }  
  
  cat_model <-
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      #loss_reduction = tune(),
      #subsample = tune(),
      mtry = tune(),
      learn_rate = tune(),
    ) %>%
    set_engine("catboost") %>%
    set_mode("classification")
  
  cat_grid <- grid_max_entropy(trees(),
                               tree_depth(),
                               min_n(),
                               #loss_reduction(),
                               #subsample = sample_prop(),
                               finalize(mtry(), hyperopt.folds$splits[[1]]$data),
                               learn_rate(),
                               size = grid.size)
  
  cat_workflow <- workflow() %>%
    add_model(cat_model) %>%
    add_formula(ofi ~ .)
  
  
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
  
  saveRDS(tuned_model, "out/cat.rds")
  
  return(tuned_model)
}