results.lr <- readRDS("results/resultslr.Rds")
results.forest <- readRDS("results/resultsforest.Rds")
results.vector.machine <- readRDS("results/resultsvektormachine.Rds")
results.boost <- readRDS("results/resultsboost.Rds")
#results.forest <- run_ml(dataset = preprocessed.data,
#                 method = 'rf',
#                outcome_colname = "ofi",
#               kfold = 5,
#              cv_times = 5,
#             training_frac = 0.8,
#            seed = 2019)


#results.lr.tv <- readRDS("resultslrtv.Rds")
#results.forest.tv <- readRDS("resultsforesttv.Rds")
#results.vector.machine.tv <- readRDS("resultsvektormachinetv.Rds")
#results.boost.tv <- readRDS("resultsboost.Rds")

######### for random vektor

test.data <- results.lr$test_data
labels <- test.data$ofi
test.data$ofi <- NULL

### log reg
final.model.lr <- results.lr$trained_model$finalModel
prediction.lr <- predict(final.model.lr, newx = as.matrix(test.data), s = 0.1, type = "response")
pred.lr <- ROCR::prediction(prediction.lr, labels)
auc.lr <- ROCR::performance(pred.lr, measure = "auc")@y.values
auc.lr <- unlist(auc.lr)
accuracy.lr <- ROCR::performance(pred.lr, measure = "acc")@y.values
accuracy.lr <- mean(unlist(accuracy.lr))
#ici.lr <- ici(prediction.lr, labels)

### Randrom forest
final.model.forest <- results.forest$trained_model$finalModel
pred.rf <- predict(final.model.forest, newdata = as.matrix(results.forest$test_data), predict.all = TRUE)
pred.forest <- pred.rf$aggregate
prediction.forest3 <- ROCR::prediction(as.numeric(pred.forest), as.numeric(labels))
auc.forest <- ROCR::performance(prediction.forest3, measure = "auc")@y.values
accuracy.forest <- ROCR::performance(prediction.forest3, measure = "acc")@y.values
accuracy.forest <- mean(unlist(accuracy.forest))

### SVM
#final.model.vector.machine <- results.vector.machine$trained_model$finalModel
#prediction.vector.machine <- predict(final.model.vector.machine, newx = as.matrix(test.data), s = 0.1, type = "response")
#pred.vector.machine <- ROCR::prediction(prediction.vector.machine, labels)
#auc.vector.machine <- ROCR::performance(pred.vector.machine, measure = "auc")@y.values
#accuracy.vector.machine <- ROCR::performance(pred.vector.machine, measure = "acc")@y.values

### xgboost
final.model.boost <- results.boost$trained_model$finalModel
prediction.boost <- xgboost:::predict.xgb.Booster(final.model.boost, newdata = as.matrix(test.data), type = "response")
pred.boost <- ROCR::prediction(prediction.boost, labels)
auc.boost <- ROCR::performance(pred.boost, measure = "auc")@y.values
auc.boost <- 1-as.numeric(auc.boost)
accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values
accuracy.boost <- mean(unlist(accuracy.boost))

### Result summary
## Delta AUC 

lr.rf.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.forest))
lr.boost.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.boost))
rf.boost.auc <- abs(as.numeric(auc.boost) - as.numeric(auc.forest))

auc <- c(lr.auc = auc.lr,
        rf.auc = auc.forest,
        boost.auc = auc.boost,
        delta.lr.rf.auc = lr.rf.auc,
        delta.lr.boost.auc = lr.boost.auc,
        delta.rf.boost.auc = rf.boost.auc)

## Delta accuracy

lr.rf.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.forest))
lr.boost.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.boost))
rf.boost.acc <- abs(as.numeric(accuracy.boost) - as.numeric(accuracy.forest))

accuracy <- c(lr.accuracy = accuracy.lr,
         rf.accuracy = accuracy.forest,
         boost.accuracy = accuracy.boost,
         delta.lr.rf.accuracy = lr.rf.acc,
         delta.lr.boost.accuracy = lr.boost.acc,
         delta.rf.boost.accuracy = rf.boost.acc)

boot.results <- c(accuracy,auc)
