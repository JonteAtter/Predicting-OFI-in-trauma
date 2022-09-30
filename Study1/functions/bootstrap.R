###############
## Bootstrap ##  
###############

bootstrap <- function(data, index) {
  random.dataset <- data[index,]
  
  ##if (!is.null(index)) {
  ##  random.datasets <- data[index,]. #### Fungerade ej direkt, och tog bort i ett försök att minimera felkällor. 
  ##}
  
  ## Create OFI collumn
  random.dataset$ofi <- rofi::create_ofi(random.dataset)
  
  ## Clean previous audit filters
  random.clean.af <- clean_audit_filters(random.dataset)
  
  ## Only those who are done with VK / Dodsfallskonferans
  random.clean.af <- subset(random.clean.af, random.clean.af$tra_DodsfallsanalysGenomford == 1 | random.clean.af$VK_avslutad == "Yes" )
  
  ## Separate and store cases without known outcome
  missing.outcome <- is.na(random.clean.af$ofi)
  random.clean.af <- random.clean.af[!missing.outcome, ]
  
  ## Clean predictors (Correct NA classification and predictor choice)
  clean.random.dataset <- clean_predictor(random.clean.af)
  
  ## clean.random.dataset <-clean.random.dataset[order(clean.random.dataset$arrival), ]
  ## tv <- c(1:round(nrow(clean.random.dataset)*0.8, digits = 0))
  
  ## Imputation
  random.imputed.dataset <- imputation(clean.random.dataset)
  
  ### Remove predicts without variance (imputed without missing data)
  random.variance.data <- Filter(function(x)(length(unique(x))>1), random.imputed.dataset)
  random.preprocessed.data <- preprocess_data(random.variance.data)
  
  results.lr <- run_ml(dataset = random.preprocessed.data,
                       method = 'glmnet',
                       outcome_colname = "ofi",
                       perf_metric_name = "AUC",
                       kfold = 5,
                       cv_times = 5,
                       training_frac = 0.8,
                       seed = 2019)
  
  #  results.forest <- run_ml(dataset = preprocessed.data,
  #                             method = 'rf',
  #                             outcome_colname = "ofi",
  #                             perf_metric_name = "AUC",
  #                             kfold = 5,
  #                             cv_times = 5,
  #                             training_frac = 0.8,
  #                             seed = 2019)
  #    
  #    results.boost <- run_ml(dataset = preprocessed.data,
  #                            method = 'xgbTree',
  #                            outcome_colname = "ofi",
  #                            perf_metric_name = "AUC",
  #                            kfold = 5,
  #                            cv_times = 5,
  #                            training_frac = 0.8,
  #                            seed = 2019) 
  #    
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
  
  auc <- auc.lr
  accuracy <- accuracy.lr
  ici <- ici.lr
  
  boot.result <- c(accuracy,auc,ici)    
  
  ## Randrom forest
  ## final.model.forest <- results.forest$trained_model$finalModel
  ## pred.rf <- predict(final.model.forest, newdata = as.matrix(results.forest$test_data), predict.all = TRUE)
  ## pred.forest <- pred.rf$aggregate
  ## prediction.forest3 <- ROCR::prediction(as.numeric(pred.forest), as.numeric(labels))
  ## auc.forest <- unlist(ROCR::performance(prediction.forest3, measure = "auc")@y.values)
  ## accuracy.forest <- ROCR::performance(prediction.forest3, measure = "acc")@y.values
  ## accuracy.forest <- mean(unlist(accuracy.forest))
  
  ### SVM
  ## final.model.vector.machine <- results.vector.machine$trained_model$finalModel
  ## prediction.vector.machine <- predict(final.model.vector.machine, newx = as.matrix(test.data), s = 0.1, type = "response")
  ## pred.vector.machine <- ROCR::prediction(prediction.vector.machine, labels)
  ## auc.vector.machine <- ROCR::performance(pred.vector.machine, measure = "auc")@y.values
  ## accuracy.vector.machine <- ROCR::performance(pred.vector.machine, measure = "acc")@y.values
  
  ## xgboost
  #    final.model.boost <- results.boost$trained_model$finalModel
  #    prediction.boost <- xgboost:::predict.xgb.Booster(final.model.boost, newdata = as.matrix(test.data), type = "response")
  #    pred.boost <- ROCR::prediction(prediction.boost, labels)
  #    auc.boost <- unlist(ROCR::performance(pred.boost, measure = "auc")@y.values)
  #    auc.boost <- 1-as.numeric(auc.boost)
  #    accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values
  #    accuracy.boost <- 1-mean(unlist(accuracy.boost))
  
  ## Result summary
  
  ## Delta AUC 
  #lr.rf.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.forest))
  #lr.boost.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.boost))
  #rf.boost.auc <- abs(as.numeric(auc.boost) - as.numeric(auc.forest))
  
  #auc <- c(lr.auc = auc.lr,
  #         boost.auc = auc.boost,
  #         delta.lr.boost.auc = lr.boost.auc)
  
  ##    auc <- c(lr.auc = auc.lr,
  ##             rf.auc = auc.forest,
  ##             boost.auc = auc.boost,
  ##             delta.lr.rf.auc = lr.rf.auc,
  ##             delta.lr.boost.auc = lr.boost.auc,
  ##             delta.rf.boost.auc = rf.boost.auc)
  
  ## Delta accuracy
  ##    lr.rf.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.forest))
  ##    lr.boost.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.boost))
  ##    rf.boost.acc <- abs(as.numeric(accuracy.boost) - as.numeric(accuracy.forest))
  
  ##    accuracy <- c(lr.accuracy = accuracy.lr,
  ##                  boost.accuracy = accuracy.boost,
  ##                  delta.lr.boost.accuracy = lr.boost.acc)
  
  ##    accuracy <- c(lr.accuracy = accuracy.lr,
  ##                  rf.accuracy = accuracy.forest,
  ##                  boost.accuracy = accuracy.boost,
  ##                  delta.lr.rf.accuracy = lr.rf.acc,
  ##                  delta.lr.boost.accuracy = lr.boost.acc,
  ##                  delta.rf.boost.accuracy = rf.boost.acc)
  
  
  
  ## if (!boot) {
  ##  boot.result <- list(boot.result,
  ##                     lr.model = results.lr,
  ##                    rf.model = results.forest,
  ##                   boost.model = results.boost)
  ## }
  
  if (!dir.exists("out"))
    dir.create("out")
  filename <- paste0("out/boot.result.", gsub(".", "", as.numeric(Sys.time()), fixed = TRUE), ".Rds")
  saveRDS(boot.result, filename)
  message("Bootstrap analysis completed and results saved")
  return(boot.result)
}