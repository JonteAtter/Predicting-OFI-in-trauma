#!/usr/bin/env Rscript

## to uppdate rofi
#remove.packages("rofi")
#library(devtools)
#install_github("martingerdin/rofi")

# Treesnip version: remotes::install_github("curso-r/treesnip@catboost")
# Catboost: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')

## Lock seed
set.seed(2022)

## Activate multithreading
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

## Load packages
packages <- c("rofi","Gmisc", "stringr", "dplyr", "labelled", "DBI", 
              "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", 
              "tableone", "table1", "dplyr", "kableExtra", "lattice", "caret",
              "treesnip", "tidymodels", "doParallel", "treesnip", "baguette", 
              "gmish", "progress", "dbarts", "lightgbm", "catboost", "rpart",
              "kknn", "Rmisc", "smotefamily")
for (package in packages) library(package, character.only = TRUE)

## Load functions
source("functions/functions.R")

# Load models
source("models/models.R")

# Make sure out dir exists
dir.create("out", showWarnings = FALSE)

## Import data
datasets <- rofi::import_data()

## Merge data
combined.datasets <- rofi::merge_data(datasets)

## Create OFI column
combined.datasets$ofi <- rofi::create_ofi(combined.datasets)

dataset.clean.af <- clean_audit_filters(combined.datasets)

## Separate and store cases without known outcome
missing.outcome <- is.na(dataset.clean.af$ofi)
n.missing.outcome <- sum(missing.outcome)
dataset.clean.af <- dataset.clean.af[!missing.outcome, ]


## Fix formating and remove wrong values like 999
clean.dataset <- clean_all_predictors(dataset.clean.af)

## Integrate RTS 
clean.dataset <- combine_rts(clean.dataset)

# Select which models to run
models.hyperopt <- c(
  #"bart" = bart_hyperopt, # unused tree argument bug?
  #"cat" = cat_hyperopt,
  "dt" = dt_hyperopt,
  "knn" = knn_hyperopt,
  "lgb" = lgb_hyperopt,
  "lr" = lr_hyperopt,
  "rf" = rf_hyperopt,
  "svm" = svm_hyperopt,
  "xgb" = xgb_hyperopt
)

# Settings
data.fraction = 1 # Used for debugging
hyperopt.grid.size = 10
hyperopt.n.folds = 3
n.resamples = 10
train.fraction <- 0.8

# Create run out dir
run.time <- format(Sys.time(), "%y-%m-%d-%H-%M")
run.out.dir <- sprintf("out/%s", run.time)
dir.create(run.out.dir)

models <- c()
results <- list()

# Use a fraction of the dataset for debugging fast
clean.dataset <- clean.dataset[sample(nrow(clean.dataset), floor(nrow(clean.dataset) * data.fraction)),]

# First resample
train.sample <- sample(seq_len(nrow(clean.dataset)), size = floor(train.fraction * nrow(clean.dataset)))

trained.preprocessor  <- clean.dataset[train.sample, ] %>% remove_columns() %>% preprocess_data()

train.data <- clean.dataset[train.sample, ] %>% remove_columns()
test.data <- clean.dataset[-train.sample, ] %>% remove_columns()

saveRDS(train.data, file = sprintf("%s/train_data.rds", run.out.dir))
saveRDS(test.data, file = sprintf("%s/test_data.rds", run.out.dir))

train.data <- trained.preprocessor %>% bake(new_data = NULL)
test.data <- trained.preprocessor %>% bake(new_data = test.data)

test.target <- test.data$ofi
test.data <- subset(test.data, select = -ofi)

train.data <- smotefamily::ADAS(subset(train.data, select = -ofi), train.data$ofi)$data
colnames(train.data)[colnames(train.data) == "class"] ="ofi"
train.data$ofi <- as.factor(train.data$ofi)

resample.results <- list("target" = test.target)

pb <- progress::progress_bar$new(format = "HYPEROPTING :spin [:bar] :current/:total | :elapsedfull",
                                 total = length(models.hyperopt), show_after=0) 

for (model.name in names(models.hyperopt)){
  pb$tick(0)

  hyperopt <- models.hyperopt[model.name][[1]]
  
  
  model <- hyperopt(train.data, grid.size = hyperopt.grid.size, n.folds = hyperopt.n.folds)
  
  model.fitted <- fit(model, ofi ~ ., data = train.data)
  
  # Get prediction probabilities for each test case
  resample.results[[model.name]] <- predict(model.fitted, test.data, type = "prob") %>% pull(2)
  
  models[[model.name]] <- model
  
  pb$tick()
}

resample.results[["auditfilter"]] <- audit_filters_predict(clean.dataset[-train.sample, ])

results <- append(results, list(resample.results))


pb <- progress::progress_bar$new(format = "RESAMPLING :spin [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
                                 total = n.resamples, show_after=0) 
for(i in 1:n.resamples){
  pb$tick(0)
  train.sample <- sample(seq_len(nrow(clean.dataset)), size = floor(train.fraction * nrow(clean.dataset)))
  
  trained.preprocessor  <- clean.dataset[train.sample, ] %>% remove_columns() %>% preprocess_data()
  
  test.data <- clean.dataset[-train.sample, ] %>% remove_columns()
  
  train.data <- trained.preprocessor %>% bake(new_data = NULL)
  test.data <- trained.preprocessor %>% bake(new_data = test.data)
  
  test.target <- test.data$ofi
  test.data <- subset(test.data, select = -ofi)
  
  train.data <- smotefamily::ADAS(subset(train.data, select = -ofi), train.data$ofi)$data
  colnames(train.data)[colnames(train.data) == "class"] ="ofi"
  train.data$ofi <- as.factor(train.data$ofi)
  
  resample.results <- list("target" = test.target)
  
  for (model.name in names(models)){
    model <- models[model.name][[1]]
    
    # Train model
    model.fitted <- fit(model, ofi ~ ., data = train.data)
    
    # Get prediction probabilities for each test case
    resample.results[[model.name]] <- predict(model.fitted, test.data, type = "prob") %>% pull(2)
  }
  
  resample.results[["auditfilter"]] <- audit_filters_predict(clean.dataset[-train.sample, ])
  
  results <- append(results, list(resample.results))
  
  pb$tick()
}

saveRDS(results, file = sprintf("%s/results.rds", run.out.dir))

statistics <- c()

for(resample in results){
  target <- as.numeric(resample[["target"]])
  model.names <-  names(resample)[-1]
  
  for(model.name in model.names){
    test.probs <- resample[[model.name]]
    
    test.preds <- ROCR::prediction(test.probs, target)
    
    statistics[[model.name]][["auc"]] <- statistics[[model.name]][["auc"]] %>% 
      append(ROCR::performance(test.preds, measure = "auc")@y.values[[1]][1])

    statistics[[model.name]][["aucpr"]] <- statistics[[model.name]][["aucpr"]] %>% 
      append(ROCR::performance(test.preds, measure = "aucpr")@y.values[[1]][1])
    
    statistics[[model.name]][["f"]] <- statistics[[model.name]][["f"]] %>% 
      append(ROCR::performance(test.preds, measure = "f")@y.values[[1]][1])
    
    # Get predicted classes using 0.5 as cut off
    test.pred.classes <- as.numeric(test.probs >= 0.5) + 1
    
    # Calculate accuracy using said cut off
    statistics[[model.name]][["acc"]] <- statistics[[model.name]][["acc"]] %>%
      append(sum(test.pred.classes == target, na.rm = TRUE) / length(test.probs))
    
    # ICI recommends > 1000 observations
    statistics[[model.name]][["ici"]] <- statistics[[model.name]][["ici"]] %>%
      append(gmish::ici(test.probs, target - 1))
  }
}

for (model.name in names(statistics)){
  message(model.name)
  
  message("AUC")
  print(CI(statistics[[model.name]][["auc"]], ci=0.95))
  message("")
  
  message("ACC")
  print(CI(statistics[[model.name]][["acc"]], ci=0.95))
  message("")
  
  message("AUCPR")
  print(CI(statistics[[model.name]][["aucpr"]], ci=0.95))
  message("")
  
  message("f")
  print(CI(statistics[[model.name]][["f"]], ci=0.95))
  message("")
  
  message("ICI")
  print(CI(statistics[[model.name]][["ici"]], ci=0.95))
  message("\n")
}

