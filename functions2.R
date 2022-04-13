#### Function to clean data
# Clean audit filters
# Clean predictor variables
# Imputation

clean_Audit_filters <- function(combined.datasets) {
  
  #################################
  # Clean audit-filters to Yes/No #
  #################################
  
  audit.filter <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                    "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                    "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                    "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE","VK_avslutad")
  
  audit.filter2 <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                     "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                     "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                     "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE")
  
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

Clean_predictor <- function(data.prob) {
  
  ## Create vectors for variables, separated by type and use.
  ## Make sure "na.values.list" is up to date if you change variables
  
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value") 
  ## categorical variables
  cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ## Variables used for sorting
  time.id.var <- c("arrival", "id")                                              
  variables <- c(cont.var, cat.var, time.id.var)
  model.variables <- c(cont.var, cat.var)

  ## Convert categorical values to factors
  #for (variable.name in cat.var) {
  #  data.prob[, variable.name] <- as.factor(data.prob[, variable.name])
  #}
  
  ## Convert continuous variables to numeric
#  for (variable.name in cont.var) {
#    data.prob[, variable.name] <- as.numeric(data.prob[, variable.name])
#  }
  
  
#####
## CHANGE OTHER VARIABLES???? INTEGRATE PREHOSP VALUES THE SAME WAY AS HUSSEIN BEFORE IMPUTATION?
####  
## Create new variable for intubation status: 1: Intubated in hospital: 2: Not intubated: 3 Intubated prehospital
  data.prob$intub <- with(data.prob, ifelse(`pre_intubated` == 1 & is.na(data.prob$pre_intubated) == FALSE, 3, `ed_intubated`))
  
## A list that governs values in what variables that should be converted to NA
  na.values.list <- list(ed_gcs_sum = c(99, 999),
                         ed_rr_value_ = c(99),
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
  data.prob[variables][] <- lapply(names(data.prob[variables]), function(variable.name) {
    data <- data.prob[variables][, variable.name]
    na.values <- na.values.list[[variable.name]]
    if (!is.null(na.values))
      data <- convert_to_na(data, na.values)
    return (data)
  })
  
  return(data.prob)
}

##############
# Imputation # - DOES NOT WORK, CANT FIND NEW DATA
##############

imputation <- function(data) {
  # Create new dataframe - missing.indicator.variables - Containing true/false if a value is imputet
  missing.indicator.variables <- as.data.frame(lapply(data, function(data) is.na(data)))
  missing.indicator.variables[, c("ofi", "Ankomst_te", "id")] <- NULL
  names(missing.indicator.variables) <- paste0("missing_", names(missing.indicator.variables))
  
  ## Imputation 
  data.imputed <- as.data.frame(lapply(data, function(data) {
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
  return(new.data)
}
