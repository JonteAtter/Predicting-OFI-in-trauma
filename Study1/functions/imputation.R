
##############
# Imputation # 
##############


imputation <- function(dataset) {
  
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value","hosp_los_days","iva_dagar_n") 
  
  ## categorical variables
  cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ## Variables used for sorting
  time.id.var <- c("arrival", "id")                                              
  variables <- c(cont.var, cat.var, time.id.var)
  model.variables <- c(cont.var, cat.var)
  
  # Create new dataframe - missing.indicator.variables - Containing true/false if a value is imputet
  missing.indicator.variables <- as.data.frame(lapply(dataset, function(data) is.na(data)))
  missing.indicator.variables[, c("ofi", "arrival", "id")] <- NULL
  names(missing.indicator.variables) <- paste0("missing_", names(missing.indicator.variables))
  
  ## Convert categorical values to factors
  for (variable.name in cat.var) {
    dataset[, variable.name] <- as.factor(dataset[, variable.name])
  }
  
  ## Convert continuous variables to numeric
  for (variable.name in cont.var) {
    dataset[, variable.name] <- as.numeric(dataset[, variable.name])
  }
  
  ## Imputation 
  dataset.imputed <- as.data.frame(lapply(dataset, function(data) {
    new.data <- data
    if (is.factor(data))
      new.data <- as.character(data) ## change to character to be able to identify/separate?
    if (is.numeric(data))
      new.data[is.na(data)] <- mean(new.data, na.rm = TRUE) ## continuous  - mean
    if (is.character(new.data))                                                 
      new.data[is.na(data)] <- tail(names(sort(table(new.data))), 1) ## categorical - most frequent value
    if (is.factor(data))
      new.data <- as.factor(new.data)
    return (new.data)
  }))
  
  ## Create new dataframe - dataset - combining imputed data and
  ## corresponding variables if the data is imputed true/false.
  dataset <- cbind(dataset.imputed[model.variables], missing.indicator.variables)
  
  return(dataset)  
}
