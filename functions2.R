#### Function to clean data
# Clean audit filters
# Clean predictor variables
# Imputation
# Preprocess data

clean_audit_filters <- function(combined.datasets) {
  
  #################################
  # Clean audit-filters to Yes/No #
  #################################
  
  audit.filter <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                    "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                    "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                    "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE","VK_avslutad","VK_annat")
  
  audit.filter2 <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                     "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                     "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                     "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE","VK_annat")
  
  combined.datasets[,audit.filter][combined.datasets[,audit.filter] == "Ja"| 
                                     combined.datasets[,audit.filter] == "ja"] <- "Yes"
  
  combined.datasets[,audit.filter][combined.datasets[,audit.filter] == "Nej"| 
                                     combined.datasets[,audit.filter] == "nej" | 
                                     combined.datasets[,audit.filter] == "nj\r\nNej" | 
                                     combined.datasets[,audit.filter] == "nj"] <- "No"
##### Is nn = NA or No???
  
  combined.datasets[,audit.filter][combined.datasets[,audit.filter] == "nn"] <- NA
  
### Create reference vector to check for false inputs in the audit filters.  
 Levels.audit.filters <- unique(as.vector(as.matrix(combined.datasets[,audit.filter])))
 Levels.audit.filters <- Levels.audit.filters[!is.na(Levels.audit.filters)]

##Chose to remove NA above becouse sort(c("Yes", NA, "No")) only returns "No"  "Yes"?
 
  original.levels.audit.filters <- sort(c("Yes", NA, "No"))
  if (!identical(Levels.audit.filters, original.levels.audit.filters))
    stop ("Levels in Audit filters have changed")

#########
#  Convert NA:s in VK rows to No if VK_avslutad = Yes (To be able to calc false neg)
#########
  
#  for (i in 1:nrow(combined.datasets)) {
#    if (is.na(combined.datasets[i,"VK_avslutad"]) == FALSE && combined.datasets[i,"VK_avslutad"] == "Yes" && is.na(combined.datasets[i,audit.filter2]) == TRUE ) {
#      combined.datasets[i,audit.filter2] <- "No"
#    }
#  }
  
  combined.datasets[, audit.filter2] <- lapply(combined.datasets[, audit.filter2], function(column) {
    column[is.na(column) & combined.datasets$VK_avslutad == "Yes"] <- "No"
    return (column)
  })    
  
  return(combined.datasets)
}

####################
# Clean Predictors #
####################
# Change values that are suposed to be NA
# Change Predictors to the right one
# Remove cases without OFI

clean_predictor <- function(dataset) {
  
  ## Create vectors for variables, separated by type and use.
  ## Make sure "na.values.list" is up to date if you change variables
  
  ## Create new variable for intubation status: 1: Intubated in hospital: 2: Not intubated: 3 Intubated prehospital
  dataset$intub <- with(dataset, ifelse(`pre_intubated` == 1 & is.na(dataset$pre_intubated) == FALSE, 3, `ed_intubated`))
  
  ## Create vectors for variable, separated by type and use - Make sure "na.values.list" is up to date
  
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value") 
  
  ## categorical variables
  cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ## Variables used for sorting
  time.id.var <- c("arrival", "id")                                              
  variables <- c(cont.var, cat.var, time.id.var)
  model.variables <- c(cont.var, cat.var)
  dpc <- dataset[variables]
  
  ## A list that governs values in what variables that should be converted to NA
  na.values.list <- list(ed_gcs_sum = c(99, 999),
                         ed_rr_value_ = c(99), ## The manual states that RR should not be over 70
                         ISS = c(0)) 
  
  #' Convert values in variable to NA
  #' 
  #' @param data The column, or variable.
  #' @param na.values The values that should be treated as missing. Will be converted to NA.
  convert_to_na <- function(data, na.values) {
    new.data <- data
    if (is.factor(data))
      new.data <- as.character(data)
    for (value in na.values) {
      new.data[new.data == value] <- NA
    }
    if (is.factor(data))
      new.data <- as.factor(new.data)
    return (new.data)
  }
  
  ## Convert
  dpc[] <- lapply(names(dpc), function(variable.name) {
    data <- dpc[, variable.name]
    na.values <- na.values.list[[variable.name]]
    if (!is.null(na.values))
      data <- convert_to_na(data, na.values)
    return (data)
  })
 
return(dpc)   
}
  
##############
# Imputation # - DOES NOT WORK, CANT FIND NEW DATA
##############
  

imputation <- function(dataset) {
  
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value") 
  ## categorical variables
  cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ## Variables used for sorting
  time.id.var <- c("arrival", "id")                                              
  variables <- c(cont.var, cat.var, time.id.var)
  model.variables <- c(cont.var, cat.var)
  
  # Create new dataframe - missing.indicator.variables - Containing true/false if a value is imputet
  missing.indicator.variables <- as.data.frame(lapply(dataset, function(data) is.na(data)))
  missing.indicator.variables[, c("ofi", "Ankomst_te", "id")] <- NULL
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

#####################
## Preprocess data ##
#####################

preprocess_data <- function(data) {
  preprocessed.data <- mikropml::preprocess_data(data, outcome_colname = "ofi", to_numeric = FALSE)
  return (preprocessed.data$dat_transformed)
}
