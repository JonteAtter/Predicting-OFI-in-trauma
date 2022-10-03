#!/usr/bin/env Rscript

## to uppdate rofi
#remove.packages("rofi")
#library(devtools)
#install_github("martingerdin/rofi")

## Load packages
packages <- c("rofi","Gmisc", "stringr", "mikropml", "dplyr", "labelled", "DBI", "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", "tableone", "table1", "dplyr", "kableExtra", "lattice", "caret")
for (package in packages) library(package, character.only = TRUE)

## Load functions
source("functions/functions.R")

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
smaller.data <- remove_columns(clean.dataset)

## Imputation ---- OLD Function. Need consensus on how we imputate. 
#imputed.dataset <- imputation(clean.dataset)
### Remove predicts without variance (imputed without missing data)
#### variance.data <- Filter(function(x)(length(unique(x))>1), imputed.dataset)

## Preprocess the data. from mikropml, have a better way??
preprocessed.data <- preprocess_data(smaller.data)

## TODO:
## -Sync the above pipeline to your models.
## -Extract optimized hyperparameters based on trainingdata
## -create performance measures, vector with probabilities? 
## -Input these parameters in models applied to test data -> put in bootstrap inkl perfmormance measure



#### Boot test
#
#results.boot3 <- boot(data=preprocessed.data, statistic=bootstrap,
#                      R=100)
#
# Change index to access different performance meassures
#results.ci <- boot.ci(results.boot3, index = 1, type = "norm")
#