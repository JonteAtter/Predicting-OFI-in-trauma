#!/usr/bin/env Rscript
setwd("~/R/dynamic-identification-ofi/Study1")
set.seed(2022)
## Activate multithreading
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

library(ROCR)
library(gmish)

dir <- "out/22-11-08-10-24"
xgb.dir <- "out/23-01-10-19-05"

results <- readRDS(sprintf("%s/results.rds", dir))
xgb.results <- readRDS(sprintf("%s/results.rds", xgb.dir))

metrics <- list()
cutoff <- list()
auditfilter.sens <- 0

model.names <- names(results[[1]])[names(results[[1]]) %in% c("target", "auditfilter") == FALSE]

for (resample.idx in 1:length(results)){
  resample <- results[[resample.idx]]
  target <- as.numeric(resample[["target"]])
  
  preds <- ROCR::prediction(resample[["auditfilter"]], target)
  
  auc <- ROCR::performance(preds, measure = "auc")
  acc <- ROCR::performance(preds, measure = "acc")
  sens <- ROCR::performance(preds, measure = "sens")
  spec <- ROCR::performance(preds, measure = "spec")
  
  metrics[["auditfilter"]][["auc"]] <- metrics[["auditfilter"]][["auc"]] %>%
    append(auc@y.values[[1]][[1]])
  
  metrics[["auditfilter"]][["acc"]] <- metrics[["auditfilter"]][["acc"]] %>%
    append(acc@y.values[[1]][[2]])
  
  metrics[["auditfilter"]][["spec"]] <- metrics[["auditfilter"]][["spec"]] %>%
    append(spec@y.values[[1]][[2]])
  
  metrics[["auditfilter"]][["sens"]] <- metrics[["auditfilter"]][["sens"]] %>%
    append(sens@y.values[[1]][[2]])
  
  if(resample.idx){
    auditfilter.sens <- sens@y.values[[1]][[2]]
  }
}

for (resample.idx in 1:length(results)){
  for(model.name in model.names){
    # override xgb due to bug in code during first run
    if(model.name == "xgb"){
      resample <- xgb.results[[resample.idx]]
      probs <- resample[[model.name]]
      target <- as.numeric(resample[["target"]])
      
    } else {
      resample <- results[[resample.idx]]
      probs <- resample[[model.name]]
      target <- as.numeric(resample[["target"]])
    }
    
    if(resample.idx == 1){
      if(model.name == "knn"){
        preds <- ROCR::prediction(probs, target)
        sens <- ROCR::performance(preds, measure = "sens")
        
        cutoff[[model.name]] <- tail(sens@x.values[[1]], n=2)[[1]]
      } else {
        for(i in rev(seq(min(probs), max(probs), by=0.0001))){
          pred.classes <- as.numeric(probs >= i) + 1
          preds <- ROCR::prediction(pred.classes, target)
          sens <- ROCR::performance(preds, measure = "sens")
          
          if(length(sens@y.values[[1]]) == 2){
            next
          }
          
          if(sens@y.values[[1]][[2]] >= auditfilter.sens){
            cutoff[[model.name]] <- i
            break
          }
        }
      }
    } 
    
    preds <- ROCR::prediction(probs, target)
    
    auc <- ROCR::performance(preds, measure = "auc")
    
    pred.classes <- as.numeric(probs >= cutoff[[model.name]]) + 1
    preds <- ROCR::prediction(pred.classes, target)
    
    acc <- ROCR::performance(preds, measure = "acc")
    sens <- ROCR::performance(preds, measure = "sens")
    spec <- ROCR::performance(preds, measure = "spec")
    ici <- gmish::ici(probs, target - 1)
    
    metrics[[model.name]][["auc"]] <- metrics[[model.name]][["auc"]] %>%
      append(auc@y.values[[1]][[1]])
    
    metrics[[model.name]][["acc"]] <- metrics[[model.name]][["acc"]] %>%
      append(acc@y.values[[1]][[2]])
    
    metrics[[model.name]][["sens"]] <- metrics[[model.name]][["sens"]] %>%
      append(sens@y.values[[1]][[2]])
    
    metrics[[model.name]][["spec"]] <- metrics[[model.name]][["spec"]] %>%
      append(spec@y.values[[1]][[2]])
    
    metrics[[model.name]][["ici"]] <- metrics[[model.name]][["ici"]] %>%
      append(ici)
  }
}

saveRDS(metrics, file = sprintf("%s/metrics.rds", dir))