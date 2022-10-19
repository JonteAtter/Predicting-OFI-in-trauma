#
library(vip)

cat <- readRDS("~/R/dynamic-identification-ofi/Study1/out/cat.rds")
lr <- readRDS("~/R/dynamic-identification-ofi/Study1/out/lr.rds")
rf <- readRDS("~/R/dynamic-identification-ofi/Study1/out/rf.rds")
xgb <- readRDS("~/R/dynamic-identification-ofi/Study1/out/xgb.rds")
svm <- readRDS("~/R/dynamic-identification-ofi/Study1/out/svm.rds")
lgb <- readRDS("~/R/dynamic-identification-ofi/Study1/out/lgb.rds")
knn <- readRDS("~/R/dynamic-identification-ofi/Study1/out/knn.rds")


lr.fitted <- fit(lr, ofi ~ ., data = train.data)
cat.fitted <- fit(cat, ofi ~ ., data = train.data)
rf.fitted <- fit(rf, ofi ~ ., data = train.data)
xgb.fitted <- fit(xgb, ofi ~ ., data = train.data)
svm.fitted <- fit(svm, ofi ~ ., data = train.data)
lgb.fitted <- fit(lgb, ofi ~ ., data = train.data)
knn.fitted <- fit(knn, ofi ~ ., data = train.data)


## LR
lr.vi <- vi(lr.fitted)
sum(lr.vi$Importance) ## 53? Jag fattar faktiskt inte "sign" pos/neg för glmnet modeller?

# cat
cat.vi <- catboost.get_feature_importance(extract_fit_engine(cat.fitted), 
                                          pool = NULL, 
                                          type = 'FeatureImportance',
                                          thread_count = -1)
## RF
#install.packages("randomForestExplainer")
#library(randomForestExplainer)
# rf.vi<-measure_importance(rf.fitted$fit).  

# Kräver att vi tränar om modellern, där man i ranger (kan ej tydligt hitta?) sätter importance='impurity' 

#lgb

lgb.vi <- lgb.importance(extract_fit_engine(lgb.fitted), percentage =TRUE)
lgb.vi$Gain <- lgb.vi$Gain * 100 
#### Osäker på om det är gain som representeras VI? Och kommer dessutom ut som procent varpå * 100.

## xgb

xgb.vi <- vip::vi(xgb.fitted)
xgb.vi$Importance <- xgb.vi$Importance * 100
sum(xgb.vi$Importance)

## KNN
# Inte möjlit att räkna var imp med knn?
knn.vi <- catboost.get_feature_importance(extract_fit_engine(knn.fitted), percentage =TRUE)
knn.vi <- vip::vi(knn.fitted)
