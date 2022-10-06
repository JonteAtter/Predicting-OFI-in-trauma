###############
## Bootstrap ##  
###############

###### data should be test.data?

bootstrap <- function(data, index, model, test, prog, audit.filter=FALSE) {
  boot.data <- data[index,]
  
  ##if (!is.null(index)) {
  ##  random.datasets <- data[index,]. #### Fungerade ej direkt, och tog bort i ett försök att minimera felkällor. 
  ##}
  
  ##### Since data == test.data(?) in a raw format we must clean a standard way -> script.
  
  ####################
  ### Cleaning data ##
  ####################
  ### Clean audit filters
  #dataset.clean.af <- clean_audit_filters(random.dataset)

  ## Fix formating and remove wrong values like 999
  #clean.dataset <- clean_all_predictors(dataset.clean.af)
  
  #### Remove columns not used for prediction
  ## Keep some ID variabel? Removes time and date, korrekt?
  #smaller.data <- remove_columns(clean.dataset)
  
  ## Imputation Need consensus on how we imputate. 
  #imputed.dataset <- imputation(smaller.data)
  ### Remove predicts without variance (imputed without missing data)
  #variance.data <- Filter(function(x)(length(unique(x))>1), imputed.dataset)
  

  #preprocessed.data <- preprocess_data(variance.data)
  
######## Ok Kelvin, we need to implement your models below!
#
# So for log reg: Antar att dessa funktioner osv måste sparas i env innan vi kör?
# hyperopt_lr <- lr_hyperopt(preprocessed.data) 
# 
#  lr_fit <- fit(hyperopt_lr, ofi ~ ., data = train_data)
#  preds <- predict(lr_fit, test_data, type = "prob") 
#  
#  labels <- facit för testdata
#  
#  pred.lr <- ROCR::prediction(preds, labels)
#  auc.lr <- ROCR::performance(pred.lr, measure = "auc")@y.values
#  accuracy.lr <- ROCR::performance(pred.lr, measure = "acc")@y.values
#  ici.lr <- gmish::ici(pred.lr, labels)
# 
#  boot.results <- c(accuracy.lr,auc.lr,ici.lr)
#
# Repetera för varje modell, där man får spara prestationsmåtten i listor som kan läggas i boot.results
#  
########  
  test.labels <- as.numeric(test$ofi)
  test.data <- subset(test, select = -ofi)
  
  if (audit.filter){
    test.probs <- model(test.data)
  } else {
    model.fitted <- fit(model, ofi ~ ., data = boot.data)
    
    test.probs <- pull(predict(model.fitted, test.data, type = "prob"), .pred_Yes)
  }

  test.preds <- ROCR::prediction(test.probs, test.labels)

  auc <- ROCR::performance(test.preds, measure = "auc")@y.values[[1]][1]
  # This might be "cheating" since it calculates the optimal cut off value for highest accuracy.
  accuracy <- ROCR::performance(test.preds, measure = "acc")@y.values[[1]][1]
  
  # ICI requires class prediction and > 1000 observations
  #test.ofi.preds <- pull(predict(model.fitted, test.data), .pred_class)
  #ici <- gmish::ici(test.ofi.preds, test.labels)

  
  boot.result <- c(accuracy, auc)#, ici)
  
  #if (!dir.exists("out"))
  #  dir.create("out")
  #filename <- paste0("out/boot.result.", gsub(".", "", as.numeric(Sys.time()), fixed = TRUE), ".Rds")
  #saveRDS(boot.result, filename)
  #message("Bootstrap analysis completed and results saved")
#####  
# This function can then be used to get CI:s for boot.result via:
# results.boot <- boot(data=test.data, statistic=bootstrap,R=1000)  
#  
#### 
  
  # Increment progress bar
  prog$tick()
  
  return(boot.result)
}