create.dataset <- function(data.fraction = 1.0, include.doa = TRUE) {
  ## Import data
  datasets <- rofi::import_data()
  
  ## Merge data
  combined.dataset <- rofi::merge_data(datasets)
  
  # Use a fraction of the dataset for debugging fast
  combined.dataset <- combined.dataset[sample(nrow(combined.dataset), floor(nrow(combined.dataset) * data.fraction)),]
  
  ## Create OFI column
  combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
  
  combined.dataset <- clean_audit_filters(combined.dataset)
  
  ## Separate and store cases without known outcome
  missing.outcome <- is.na(combined.dataset$ofi)
  combined.dataset <- combined.dataset[!missing.outcome,]
  
  combined.dataset <-
    combined.dataset[combined.dataset$pt_age_yrs > 14,]
  
  ## Fix formating and remove wrong values like 999
  #combined.dataset <- clean_predictor(combined.dataset)
  combined.dataset <- clean_all_predictors(combined.dataset)
  
  if(!include.doa){
    # Remove DOA
    combined.dataset <- DOA(combined.dataset)
  }
  
  ## Integrate RTS
  combined.dataset <- combine_rts(combined.dataset)
  
  return(combined.dataset)
}