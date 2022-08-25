#!/usr/bin/env Rscript

## Load packages
packages <- c("Gmisc", "stringr", "mikropml", "dplyr", "labelled", "DBI", "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", "tableone", "table1", "dplyr", "kableExtra", "lattice", "caret")
for (package in packages) library(package, character.only = TRUE)

## Source functions
# source("functions2.R")

## Import data
datasets <- rofi::import_data()

## Merge data
#combined.datasets <- rofi::merge_data(datasets)
combined.datasets <- merge_data2(datasets)
## Create OFI collumn
#combined.datasets$ofi <- rofi::create_ofi(combined.datasets)
combined.datasets$ofi <- create_ofi2(combined.datasets)
### Cleaning data

### Clean previous audit filters
dataset.clean.af <- clean_audit_filters(combined.datasets)

## Only those who are done with VK / Dodsfallskonferans
dataset.clean.af <- subset(dataset.clean.af, dataset.clean.af$tra_DodsfallsanalysGenomford == 1 | dataset.clean.af$VK_avslutad == "Yes" )

## Separate and store cases without known outcome
missing.outcome <- is.na(dataset.clean.af$ofi)
n.missing.outcome <- sum(missing.outcome)
dataset.clean.af <- dataset.clean.af[!missing.outcome, ]

## Clean predictors (Correct NA classification and predictor choice)
clean.dataset <- clean_predictor(dataset.clean.af)

## Imputation
imputed.dataset <- imputation(clean.dataset)
### Remove predicts without variance (imputed without missing data)
variance.data <- Filter(function(x)(length(unique(x))>1), imputed.dataset)
preprocessed.data <- preprocess_data(variance.data)
tv <- c(1:round(nrow(clean.dataset)*0.8, digits = 0))

### Load results to make it faste for kniting
## Save the output of mikropml as an rds object in order to do fast kniting
##
#######
results.boost <- readRDS("results/results.boost.rds")
results.lr <- readRDS("results/results.lr.rds")
results.forest <- readRDS("results/results.forest.rds")

### data to predict against
test.data <- results.lr$test_data
labels <- test.data$ofi
test.data$ofi <- NULL

## log reg
final.model.lr <- results.lr$trained_model$finalModel
prediction.lr <- predict(final.model.lr, newx = as.matrix(test.data), s = 0.1, type = "response")
pred.lr <- ROCR::prediction(prediction.lr, labels)
auc.lr <- ROCR::performance(pred.lr, measure = "auc")@y.values
auc.lr <- unlist(auc.lr)
accuracy.lr <- ROCR::performance(pred.lr, measure = "acc")@y.values
accuracy.lr <- mean(unlist(accuracy.lr))
labels.ici <- as.character(labels)
labels.ici[labels.ici == "Yes"] <- 1
labels.ici[labels.ici == "No"] <- 0 
labels.ici <- as.numeric(labels.ici)
prediction.lr.vector <- as.vector(prediction.lr)
ici.lr <- gmish::ici(prediction.lr.vector, labels.ici)

## Randrom forest
 final.model.forest <- results.forest$trained_model$finalModel
 pred.rf <- predict(final.model.forest, newdata = as.matrix(results.forest$test_data), predict.all = TRUE)
 pred.forest <- pred.rf$aggregate
 prediction.rf <- ROCR::prediction(as.numeric(pred.forest), as.numeric(labels))
 auc.forest <- unlist(ROCR::performance(prediction.rf, measure = "auc")@y.values)
 accuracy.forest <- ROCR::performance(prediction.rf, measure = "acc")@y.values
 accuracy.forest <- mean(unlist(accuracy.forest))
 prediction.rf.vector <- as.vector(unlist(prediction.rf@predictions))
 ici.rf <- gmish::ici(prediction.rf.vector, labels.ici)

### SVM
## final.model.vector.machine <- results.vector.machine$trained_model$finalModel
## prediction.vector.machine <- predict(final.model.vector.machine, newx = as.matrix(test.data), s = 0.1, type = "response")
## pred.vector.machine <- ROCR::prediction(prediction.vector.machine, labels)
## auc.vector.machine <- ROCR::performance(pred.vector.machine, measure = "auc")@y.values
## accuracy.vector.machine <- ROCR::performance(pred.vector.machine, measure = "acc")@y.values

## xgboost
final.model.boost <- results.boost$trained_model$finalModel
prediction.boost <- xgboost:::predict.xgb.Booster(final.model.boost, newdata = as.matrix(test.data), type = "response")
pred.boost <- ROCR::prediction(prediction.boost, labels)
auc.boost <- unlist(ROCR::performance(pred.boost, measure = "auc")@y.values)
auc.boost <- 1-as.numeric(auc.boost)
accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values
accuracy.boost <- 1-mean(unlist(accuracy.boost))
prediction.boost.vector <- as.vector(prediction.boost)
ici.boost <- gmish::ici(prediction.boost.vector, labels.ici)
ici.boost <- 1-ici.boost

## Result summary

## Delta AUC 
lr.rf.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.forest))
lr.boost.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.boost))
rf.boost.auc <- abs(as.numeric(auc.boost) - as.numeric(auc.forest))

auc <- c(lr.auc = auc.lr,
         boost.auc = auc.boost,
         delta.lr.boost.auc = lr.boost.auc)

##    auc <- c(lr.auc = auc.lr,
##             rf.auc = auc.forest,
##             boost.auc = auc.boost,
##             delta.lr.rf.auc = lr.rf.auc,
##             delta.lr.boost.auc = lr.boost.auc,
##             delta.rf.boost.auc = rf.boost.auc)

## Delta accuracy
lr.rf.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.forest))
lr.boost.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.boost))
rf.boost.acc <- abs(as.numeric(accuracy.boost) - as.numeric(accuracy.forest))

accuracy <- c(lr.accuracy = accuracy.lr,
              boost.accuracy = accuracy.boost,
              delta.lr.boost.accuracy = lr.boost.acc)

