connected.features <- list(
  "hosp_vent_days" = "host_vent_days_NotDone",
  "ed_inr" = "ed_inr_NotDone",
  "dt_ed_first_ct" = "FirstTraumaDT_NotDone",
  "ed_be_art" = "dt_ed_norm_be"
)

ignored.features <- c("ofi")
  
for(feature in connected.features){
  ignored.features <- append(ignored.features, feature)
}

permutation.importance <- function(data, model, n.permutations, preprocessor) {
  importance.results <- c()
  
  features <- colnames(data)
  
  # Get original data performance
  original.data <- preprocessor %>% bake(new_data = NULL)
  
  original.y <- original.data$ofi
  original.X <- subset(original.data, select = -ofi)
  
  # calculate auc metric
  original.probs <- predict(model, original.X, type = "prob") %>% pull(2)
  original.preds <- ROCR::prediction(original.probs, original.y)
  original.auc <- ROCR::performance(original.preds, measure = "auc")@y.values[[1]][1]
  
  for(feature in features){
    if(feature %in% ignored.features){
      next
    }
    
    # Accumulate to total auc score for each permutation to later calculate average
    total.feature.auc <- 0
    
    for(idx.permutation in 1:n.permutations){
      data.permuted <- data.frame(data) # copy df
      idx.permute.order <- sample(1:nrow(data.permuted)) # shuffle order of columns
      
      # shuffle column
      data.permuted[[feature]] <- data.permuted[[feature]][idx.permute.order] 
      
      # If feature is connected to another shuffle that feature as well
      if(!is.null(connected.features[[feature]])){
        connected.feature <- connected.features[[feature]]
        
        data.permuted[[connected.feature]] <- data.permuted[[connected.feature]][idx.permute.order]
      }
      
      # preprocess corrupted data
      data.permuted <- preprocessor %>% bake(new_data = data.permuted)
      
      y <- data.permuted$ofi
      X <- subset(data.permuted, select = -ofi)
      
      # get permuted data performance
      permuted.probs <- predict(model, X, type = "prob") %>% pull(2)
      permuted.preds <- ROCR::prediction(permuted.probs, y)
      permuted.auc <- ROCR::performance(permuted.preds, measure = "auc")@y.values[[1]][1]
      
      total.feature.auc <- total.feature.auc + permuted.auc
    }
    
    # calculate average auc score across permutations
    average.permutation.auc <- total.feature.auc / n.permutations
    
    # calculate variable importance
    importance.results[[feature]] <- original.auc - average.permutation.auc
  }
  
  return(importance.results)
}
