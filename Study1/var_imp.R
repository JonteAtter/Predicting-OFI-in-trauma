#bart <- readRDS("out/bart.old.rds")

### Laddar modeller
rf <- readRDS("out/rf.old2.rds")
cat<- readRDS("out/cat.old.rds")
dt<- readRDS("out/dt.old.rds")
knn<- readRDS("out/knn.old.rds") # ej klart
lgb<- readRDS("out/lgb.old.rds")
lr<- readRDS("out/lr.old.rds")
svm<- readRDS("out/rf.old.rds") # Ej klart
xgb<- readRDS("out/xgb.old.rds") 

# VI kräver ofta fit
rf.fit  <- fit(rf, ofi ~ ., data = train.data)
cat.fit <- fit(cat, ofi ~ ., data = train.data)
dt.fit <- fit(dt, ofi ~ ., data = train.data)
knn.fit <- fit(knn, ofi ~ ., data = train.data)
lgb.fit <- fit(lgb, ofi ~ ., data = train.data)
lr.fit <- fit(lr, ofi ~ ., data = train.data)
svm.fit <- fit(svm, ofi ~ ., data = train.data)
xgb.fit <- fit(xgb, ofi ~ ., data = train.data)

## RF
##### Kräver att du hyperopimerar med: set_mode("classification") %>% 
##                                     set_engine("ranger",importance = "impurity")

library(vip) 
vi.rf <- rf.fit[["fit"]][["variable.importance"]]
vi.rf <- vi(rf.fit, scale = TRUE)
vi.rf$Importance <- vi.rf$Importance * 100/1882.342
vi.rf$rf <- vi.rf$Importance
vi.rf$Importance <- NULL

## CAT
vi.cat <- catboost.get_feature_importance(extract_fit_engine(cat.fit), 
                                          pool = NULL, 
                                          type = 'FeatureImportance',
                                          thread_count = -1)


## lgb

vi.lgb <- lgb.importance(extract_fit_engine(lgb.fit), percentage =TRUE)
vi.lgb$Gain <- vi.lgb$Gain * 100
vi.lgb$lgb <- vi.lgb$Gain
vi.lgb$Gain <- NULL

## DT

vi.dt <- dt.fit[["fit"]][["imp"]]
vi.dt$value <- vi.dt$value *100/1539.956

sum(vi.dt$value)
vi.dt$dt <- vi.dt$value
vi.dt$value <- NULL
vi.dt$Variable <- vi.dt$term
vi.dt <- vi.dt[,c("Variable","dt")] #### Bara 87 variabler av någon anledning?
## XGB

vi.xgb  <- xgb.fit %>% vi(scale=FALSE)
vi.xgb$Importance <- vi.xgb$Importance *100
vi.xgb$xgb <- vi.xgb$Importance
vi.xgb$Importance <- NULL

## LR

vi.lr <- vi(lr.fit) ## Pos/neg värden där jag inte är 100% hur det ska tolkas
vi.lr$Importance <- vi.lr$Importance*100/72.95979
vi.lr$lr <- vi.lr$Importance
vi.lr$Importance <- NULL

##### Bygg ihop till dataframe för smidigare plot
t.rf.vi <- as.data.frame(vi.rf)
t.xgb.vi <- as.data.frame(vi.xgb)
t.lgb.vi <- as.data.frame(vi.lgb[,c("Feature","lgb")])
t.dt.vi <- as.data.frame(vi.dt)
t.lr.vi <- as.data.frame(vi.lr)
t.lr.vi$Sign <- NULL

t.lgb.vi$Variable <- t.lgb.vi$Feature
t.lgb.vi$Feature <- NULL
t.cat.vi <- as.data.frame(vi.cat)
t.cat.vi$Variable <- NA
t.cat.vi$Variable <- rownames(vi.cat)

varimptable <- merge(t.rf.vi,t.xgb.vi,by="Variable")

varimptable2 <- merge(varimptable,t.lgb.vi,by="Variable")

varimptable3 <- merge(varimptable2,t.cat.vi,by="Variable")
varimptable3$cat<-varimptable3$V1
varimptable3$V1 <- NULL
varimptable4 <- merge(varimptable3,t.dt.vi,by="Variable",all = TRUE)
varimptable5 <- merge(varimptable4,t.lr.vi,by="Variable")
varimptable <- varimptable5

#saveRDS(varimptable, "out/varimptable.rds")

#varimptable <- varimptable[order(varimptable$rf, decreasing = TRUE),]
##label <- varimptable$Variable
#varimptable$Variable <- factor(varimptable$Variable, levels = rev(label))

#require(ggplot2)
#p <- ggplot(varimptable, aes(x=rf, y=Variable)) + geom_point(colour = "blue") #+ geom_line (colour = "blue")
#p <- p + geom_point (aes(x = xgb, y=Variable), colour = "red")
#p <- p + geom_point (aes(x = lgb, y=Variable), colour = "green")

