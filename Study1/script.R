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
              "kknn", "Rmisc")
for (package in packages) library(package, character.only = TRUE)

## Load functions
source("functions/functions.R")

# Load models
source("models/models.R")

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

# Select which models to run
models.hyperopt <- c(
  #"bart" = bart_hyperopt, # unused tree argument bug?
  "cat" = cat_hyperopt,
  "dt" = dt_hyperopt,
  "knn" = knn_hyperopt,
  "lgb" = lgb_hyperopt,
  "lr" = lr_hyperopt,
  "rf" = rf_hyperopt,
  "svm" = svm_hyperopt,
  "xgb" = xgb_hyperopt
)

models <- c()
results <- list()
n.resamples = 10
pb <- progress::progress_bar$new(format = "HYPEROPTING :spin [:bar] :current/:total | :elapsedfull",
                                 total = length(models.hyperopt), show_after=0) 

complete.preprocessed.data  <- clean.dataset %>% remove_columns() %>%
  preprocess_data(verbose = TRUE)

complete.preprocessed.data.summary <- summary(complete.preprocessed.data)
complete.preprocessed.data <- bake(complete.preprocessed.data, new_data = NULL)

for (model.name in names(models.hyperopt)){
  pb$tick(0)

  hyperopt <- models.hyperopt[model.name][[1]]
  
  
  model <- hyperopt(complete.preprocessed.data)
  
  models[[model.name]] <- model
  
  pb$tick()
}

pb <- progress::progress_bar$new(format = "RESAMPLING :spin [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
                                 total = n.resamples, show_after=0) 
for(i in 1:n.resamples){
  pb$tick(0)
  train.fraction <- 0.8
  train.sample <- sample(seq_len(nrow(clean.dataset)), size = floor(train.fraction * nrow(clean.dataset)))
  
  trained.preprocessor  <- clean.dataset[train.sample, ] %>% remove_columns() %>% preprocess_data()
  
  test.data <- clean.dataset[-train.sample, ] %>% remove_columns()
  
  train.data <- trained.preprocessor %>% bake(new_data = NULL)
  test.data <- trained.preprocessor %>% bake(new_data = test.data)
  test.target <- test.data$ofi
  test.data <- subset(test.data, select = -ofi)
  
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

saveRDS(results, file = sprintf("out/%s_results.rds", format(Sys.time(), "%y-%m-%d-%H-%M")))

statistics <- c()

for(resample in results){
  target <- as.numeric(resample[["target"]])
  model.names <-  names(resample)[-1]
  
  for(model.name in model.names){
    test.probs <- resample[[model.name]]
    
    test.preds <- ROCR::prediction(test.probs, target)
    
    statistics[[model.name]][["auc"]] <- statistics[[model.name]][["auc"]] %>% 
      append(ROCR::performance(test.preds, measure = "auc")@y.values[[1]][1])
    
    # This might be "cheating" since it calculates the optimal cut off value for highest accuracy.
    statistics[[model.name]][["acc"]] <- statistics[[model.name]][["acc"]] %>%
      append(ROCR::performance(test.preds, measure = "acc")@y.values[[1]][1])
    
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
  
  message("ICI")
  print(CI(statistics[[model.name]][["ici"]], ci=0.95))
  message("\n")
}
