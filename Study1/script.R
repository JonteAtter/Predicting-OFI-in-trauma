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
              "kknn")
for (package in packages) library(package, character.only = TRUE)

## Load functions
source("functions/functions.R")

# Load models
source("models/models.R")

## Import data
datasets <- rofi::import_data()

## Merge data
combined.datasets <- rofi::merge_data(datasets)

## Create OFI collumn
combined.datasets$ofi <- rofi::create_ofi(combined.datasets)

####################
### Cleaning data ##
####################
### Clean audit filters
dataset.clean.af <- clean_audit_filters(combined.datasets)

## Separate and store cases without known outcome
missing.outcome <- is.na(dataset.clean.af$ofi)
n.missing.outcome <- sum(missing.outcome)
dataset.clean.af <- dataset.clean.af[!missing.outcome, ]

## Fix formating and remove wrong values like 999
clean.dataset <- clean_all_predictors(dataset.clean.af)

#### Remove columns not used for prediction - NEED TO EXPAND
## Keep some ID variabel? Removes time and date, korrekt?
smaller.data <- remove_columns(clean.dataset)

## Imputation Need consensus on how we imputate. 
imputed.dataset <- imputation(smaller.data)
### Remove predicts without variance (imputed without missing data)
variance.data <- Filter(function(x)(length(unique(x))>1), imputed.dataset)

## Preprocess the data. Cant handle dates/times. from mikropml, have a better way?? 
#is.POSIXct <- function(x) inherits(x, "POSIXct")
#time.cols <- colnames(smaller.data %>% select_if(is.POSIXct))

#test <-  smaller.data[ , -which(names(smaller.data) %in% time.cols)]

preprocessed.data <- preprocess_data(variance.data)

# Test train split data 70%-30%
sample <- sample(c(TRUE, FALSE), nrow(preprocessed.data), replace=TRUE, prob=c(0.7,0.3))
data.train  <- preprocessed.data[sample, ]
data.test   <- preprocessed.data[!sample, ]

# Select wich models to run
models <- c(
  #"bart" = bart_hyperopt, # unused tree argument bug?
  #"cat" = cat_hyperopt,
  "dt" = dt_hyperopt,
  #"knn" = knn_hyperopt,
  #"lgb" = lgb_hyperopt,
  "lr" = lr_hyperopt,
  "rf" = rf_hyperopt
  #"svm" = svm_hyperopt,
  #"xgb" = xgb_hyperopt
)


results <- c()
# Number of boots to run (the framework adds 1)
n.boots = 99

# Run hyperopt + bootstrapping for selected models
message("RUNNING MODELS: ", paste(names(models), collapse = ', '))
for (model.name in names(models)){
  message("#---------------#")
  message(sprintf("STARTING MODEL: %s", model.name))
  
  model.hyperopt <- models[model.name][[1]]
  
  message("HYPEROPTING MODEL")
  model <- model.hyperopt(data.train)
  
  message("BOOTSTRAPPING MODEL")
  pb <- progress::progress_bar$new(format = paste(model.name, "[:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)", sep = " | "),
                                   total = n.boots + 1) 
  
  results[[ model.name  ]]  <- boot(data=data.train, statistic=bootstrap, R=n.boots, model=model,
                                   test=data.test, prog = pb)
  
  message(sprintf("DONE WITH MODEL: %s", model.name))
}
message("#---------------#")
message("DONE TESTING MODELS")


## test data requires VK columns
#pb <- progress::progress_bar$new(format = "audit filters | [:bar] :current/:total (:tick_rate/s) | :elapsedfull (:eta)",
#                                 total = n.boots + 1) 
#results[[ "auditfilters" ]]  <- boot(data=data.train, statistic=bootstrap, R=n.boots, model=audit_filters_predict(),
#                                  test=data.test, prog = pb, audit.filter=TRUE)


#### Boot test
#
#results.boot3 <- boot(data=preprocessed.data, statistic=bootstrap,
#                      R=100)
#
# Change index to access different performance meassures
#results.ci <- boot.ci(results.boot3, index = 1, type = "norm")
#