###############
## Bootstrap ##  
###############

bootstrap <- function(data, index, model, prog, audit.filter=FALSE) {
  if(length(unique(index)) == nrow(data)){
    # On first iteration split randomly 70%-30% since boot package provides all data on first iteration
    sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
    train.data  <- data[sample, ]
    test.data   <- data[!sample, ]
  } else {
    # On all other iterations use data not included in boot as test data
    test.index <- setdiff(unique(index), 1:length(data))
    
    train.data <- data[index,]
    test.data <- data[test.index,]
  }
  
  test.labels <- as.numeric(test.data$ofi)
  test.data <- subset(test.data, select = -ofi)
  
  if (audit.filter){
    # Audit filter does not needed to be fitted
    test.probs <- model(test.data)
  } else {
    # Train model
    model.fitted <- fit(model, ofi ~ ., data = train.data)
    
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
