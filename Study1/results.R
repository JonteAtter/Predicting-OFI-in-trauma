results.lr <- readRDS("resultslr.Rds")
results.forest <- readRDS("resultsforest.Rds")
results.vector.machine <- readRDS("resultsvektormachine.Rds")
results.boost <- readRDS("resultsboost.Rds")

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
accuracy.lr <- ROCR::performance(pred.lr, measure = "acc")@y.values
# ici <- gmish::ici(prediction, labels)

### Randrom forest
final.model.forest <- results.forest$trained_model$finalModel
#prediction.forest <- predict(final.model.forest, newx = as.matrix(test.data), s = 0.1, type = "response")
pred.rf <- predict(final.model.forest, newdata = as.matrix(test.data), predict.all = TRUE)
prediktions.forest <- pred.rf$aggregate
pred.forest <- ROCR::prediction(prediktions.forest, labels)
auc.forest <- ROCR::performance(pred.forest, measure = "auc")@y.values
accuracy.forest <- ROCR::performance(pred.forest, measure = "acc")@y.values

### SVM
final.model.vector.machine <- results.vector.machine$trained_model$finalModel
prediction.vector.machine <- predict(final.model.vector.machine, newx = as.matrix(test.data), s = 0.1, type = "response")
pred.vector.machine <- ROCR::prediction(prediction.vector.machine, labels)
auc.vector.machine <- ROCR::performance(pred.vector.machine, measure = "auc")@y.values
accuracy.vector.machine <- ROCR::performance(pred.vector.machine, measure = "acc")@y.values

### xgboost
final.model.boost <- results.boost$trained_model$finalModel
prediction.boost <- predict(final.model.boost, newx = as.matrix(test.data), s = 0.1, type = "response")
pred.boost <- ROCR::prediction(prediction.boost, labels)
auc.boost <- ROCR::performance(pred.boost, measure = "auc")@y.values
accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values

