#!/usr/bin/env Rscript

## to uppdate rofi
#remove.packages("rofi")
#library(devtools)
#install_github("martingerdin/rofi")

# Treesnip version: remotes::install_github("curso-r/treesnip@catboost")
# Catboost: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')

## Lock seed
set.seed(2022)

## Activate multithreading (do not use when hyperopting lgb due to memory usage. May need to use nthread = 1 in lgb or lower trees)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

## Load packages
packages <- c("rofi","finetune","tabnet","Gmisc", "stringr", "dplyr", "labelled", "DBI", 
              "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", 
              "tableone", "table1", "dplyr", "kableExtra", "lattice",
              "treesnip", "tidymodels", "treesnip", "baguette", 
              "gmish", "progress", "dbarts", "lightgbm", "catboost", "rpart",
              "kknn", "Rmisc", "smotefamily","caret")

#library(tabnet)
#library(finetune)
for (package in packages) library(package, character.only = TRUE)

# Check if working dir is Study1, else try to switch to Study1
setwd("~/R/dynamic-identification-ofi/Study1")

## Load functions
source("functions/functions.R")

# Load models
source("models/models.R")

# Make sure out dir exists
dir.create("out", showWarnings = FALSE)

# Select which models to run
models.hyperopt <- c(
  #"bart" = bart_hyperopt, # unused tree argument bug?
#  "cat" = cat_hyperopt,
#  "dt" = dt_hyperopt,
#  "knn" = knn_hyperopt,
#  "lgb" = lgb_hyperopt,
#  "lr" = lr_hyperopt,
#  "rf" = rf_hyperopt,
#  "svm" = svm_hyperopt,
  "xgb" = xgb_hyperopt
)

# Settings
data.fraction <- 1 # Used for debugging
hyperopt.grid.size <- 30
hyperopt.n.folds <- 5
n.resamples <- 1000
train.fraction <- 0.8
n.varimp.permutations <- 5

# Create run out dir
run.time <- format(Sys.time(), "%y-%m-%d-%H-%M")
run.out.dir <- sprintf("out/%s", run.time)
dir.create(run.out.dir)

models <- c()
results <- list()
variable.importances <- list()

dataset <- create.dataset(data.fraction)

pb <- progress::progress_bar$new(format = "RESAMPLING :spin [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
                                 total = n.resamples, show_after=0) 
for(idx.resample in 1:n.resamples){
  pb$tick(0)
  pb$message(sprintf("%s | Starting resample %s of %s", Sys.time(), idx.resample, n.resamples))
  
  train.sample <- sample(seq_len(nrow(dataset)), size = floor(train.fraction * nrow(dataset)))

  ## Remove unnecessary/non-swetrau columns each resample. 
  ## The audit filters features in dataset are needed later in the resample
  ## to calculate their performance.
  train.data.orig <- dataset[train.sample, ] %>% remove_columns()
  test.data.orig <- dataset[-train.sample, ] %>% remove_columns()
  
  trained.preprocessor  <- train.data.orig %>% preprocess_data()
  
  train.data <- trained.preprocessor %>% bake(new_data = NULL)
  test.data <- trained.preprocessor %>% bake(new_data = test.data.orig)
  
  test.target <- test.data$ofi
  test.data <- subset(test.data, select = -ofi)
  
  # hyperopt/save dataset for first resample
  if(idx.resample == 1){
    saveRDS(train.data, file = sprintf("%s/train_data.rds", run.out.dir))
    saveRDS(test.data.orig, file = sprintf("%s/test_data.rds", run.out.dir))
    
    hyperopt.folds <- vfold_cv(train.data, v = hyperopt.n.folds, strata = ofi)
    
    # Upsample training data in each fold to avoid data leakage
    for(idx.fold in 1:length(hyperopt.folds$splits)){
      fold.train.data <- as.data.frame(hyperopt.folds$splits[[idx.fold]])
      fold.data <- hyperopt.folds$splits[[idx.fold]]$data
      
      fold.length <- nrow(fold.data)
      
      fold.syn.data <- smotefamily::ADAS(subset(fold.train.data, select = -ofi), fold.train.data$ofi)$syn_data
      colnames(fold.syn.data)[colnames(fold.syn.data) == "class"] ="ofi"
      fold.syn.data$ofi <- as.factor(fold.syn.data$ofi)
      
      hyperopt.folds$splits[[idx.fold]]$data <- rbind(fold.data, fold.syn.data)
      
      syn.data.idxs <- fold.length:nrow(hyperopt.folds$splits[[idx.fold]]$data)
      
      hyperopt.folds$splits[[idx.fold]]$in_id <- append(hyperopt.folds$splits[[idx.fold]]$in_id, syn.data.idxs)
    }
    
    
    for (model.name in names(models.hyperopt)){
      hyperopt <- models.hyperopt[model.name][[1]]
      
      pb$message(sprintf("%s | Hyperopting %s.", Sys.time(), model.name))
      models[[model.name]] <- hyperopt(hyperopt.folds, grid.size = hyperopt.grid.size)
    } 
  }
  
  # upsample train data
  train.data <- smotefamily::ADAS(subset(train.data, select = -ofi), train.data$ofi)$data
  colnames(train.data)[colnames(train.data) == "class"] ="ofi"
  train.data$ofi <- as.factor(train.data$ofi)
  
  resample.results <- list("target" = test.target)
 # resample.var.imps <- list()
  
  for (model.name in names(models)){
    model <- models[model.name][[1]]
    
    # Train model
    pb$message(sprintf("%s | Fitting %s.", Sys.time(), model.name))
    model.fitted <- fit(model, ofi ~ ., data = train.data)
    
    # Get prediction probabilities for each test case
    pb$message(sprintf("%s | Predicting %s.", Sys.time(), model.name))
    resample.results[[model.name]] <- predict(model.fitted, test.data, type = "prob") %>% pull(2)
    
    #pb$message(sprintf("%s | Calculating variable importance for %s.", Sys.time(), model.name))
    #resample.var.imps[[model.name]] <- permutation.importance(test.data.orig, model.fitted, n.varimp.permutations, trained.preprocessor)
  }
  
  resample.results[["auditfilter"]] <- audit_filters_predict(dataset[-train.sample, ])
  
  results <- append(results, list(resample.results))
  #variable.importances <- append(variable.importances, list(resample.var.imps))
  
  pb$message(sprintf("%s | Resample %s of %s done.", Sys.time(), idx.resample, n.resamples))
  pb$tick()
}

saveRDS(results, file = sprintf("%s/results.rds", run.out.dir))
#saveRDS(variable.importances, file = sprintf("%s/variable_importances.rds", run.out.dir))
