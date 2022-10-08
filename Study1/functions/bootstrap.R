###############
## Bootstrap ##  
###############

###### data should be test.data?

bootstrap <- function(data, index, model, test, prog, audit.filter=FALSE) {
  test.labels <- as.numeric(test$ofi)
  test.data <- subset(test, select = -ofi)
  
  if (audit.filter){
    # Audit filter does not needed to be fitted
    test.probs <- model(test.data)
  } else {
    # Get resampled data to train on
    boot.data <- data[index,]
    
    # Train model
    model.fitted <- fit(model, ofi ~ ., data = boot.data)
    
    # Get prediction probabilities for each test case
    test.probs <- pull(predict(model.fitted, test.data, type = "prob"), .pred_Yes)
  }
  
  test.preds <- ROCR::prediction(test.probs, test.labels)

  auc <- ROCR::performance(test.preds, measure = "auc")@y.values[[1]][1]
  # This might be "cheating" since it calculates the optimal cut off value for highest accuracy.
  accuracy <- ROCR::performance(test.preds, measure = "acc")@y.values[[1]][1]
  
  # ICI recommends > 1000 observations
  ici <- gmish::ici(test.probs, test.labels - 1)

  
  boot.result <- c(accuracy, auc, ici)
  
  #if (!dir.exists("out"))
  #  dir.create("out")
  #filename <- paste0("out/boot.result.", gsub(".", "", as.numeric(Sys.time()), fixed = TRUE), ".Rds")
  #saveRDS(boot.result, filename)
  #message("Bootstrap analysis completed and results saved")
  
  # Increment progress bar
  prog$tick()
  
  
  return(boot.result)
}
