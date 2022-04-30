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

#####################
## Preprocess data ##
#####################

preprocess_data <- function(data) {
  preprocessed.data <- mikropml::preprocess_data(data, outcome_colname = "ofi", to_numeric = FALSE)
  return (preprocessed.data$dat_transformed)
}


####### Backup for create ofi2
#' Create Opportunities For Improvement Variable
#'
#' Create opportunities for improvement variable, which is either yes
#' or no
#'
#' Yes: The case was flagged and reviewed in a meeting and the
#' consensus was that there were opportunities for improvement
#' (variable Problemomrade_.FMP), or that the patient died and the
#' death was determined as preventable or potentially preventable
#' (variable Fr1.14 2 or 3).
#' 
#' No: The consensus was that there were no opportunities for
#' improvement, or the nurses in the initial review did not send the
#' case for review because everything was okay (variable VK_avslutad).
#'
#' NA: The case was never selected for review or the patient died but
#' whether the death was preventable is registered as unknown (Fr1.14
#' 999)
#' @param data A data.frame. The data needed to create the
#'     opportunities for improvements variable. Has to include the
#'     columns VK_avslutad, Problemomrade_.FMP,
#'     tra_DodsfallsanalysGenomford, and Fr1.14. No default.
#' @export
create_ofi2 <- function(data) {
  ## Check arguments
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(all(c("VK_avslutad", "Problemomrade_.FMP", "tra_DodsfallsanalysGenomford", "Fr1.14") %in% names(data)))
  ## Create ofi variable
  data$Problemomrade_.FMP <- tolower(data$Problemomrade_.FMP)
  levels.Problemomrade_.FMP <- unique(data$Problemomrade_.FMP)
  original.levels.Problemomrade_.FMP <- c(NA, "ok", "triage på akutmottagningen",
                                          "resurs", "lång tid till op", "lång tid till dt",
                                          "vårdnivå", "traumakriterier/styrning",
                                          "missad skada", "kommunikation", "neurokirurg",
                                          "föredömligt handlagd", "logistik/teknik",
                                          "dokumentation", "dokumetation", "bristande rutin", 
                                          "handläggning", "kompetens brist", "tertiär survey")
  if (!all(levels.Problemomrade_.FMP %in% original.levels.Problemomrade_.FMP))
    stop ("Levels in Problemomrade._FMP have changed.")
  levels.Fr1.14 <- unique(data$`Fr1.14`)
  original.levels.Fr1.14 <- c(NA, "1", "3", "2", "999")
  if (!all(levels.Fr1.14 %in% original.levels.Fr1.14))
    stop ("Levels in Fr1.14 have changed.")
  prob.filters <- with(data, `Problemomrade_.FMP` != "ok" & `Problemomrade_.FMP` != "föredömligt handlagd")
  prob.mortality <- with(data, `Fr1.14` == "2" | `Fr1.14` == "3")
  prob <- prob.filters | prob.mortality
  mortality.peer.review.done <- data$tra_DodsfallsanalysGenomford == "1"
  data$VK_avslutad <- tolower(data$VK_avslutad)
  levels.VK_avslutad <- unique(data$VK_avslutad)
  original.levels.VK_avslutad <- c("ja", NA, "nej")
  if (!all(levels.VK_avslutad %in% original.levels.VK_avslutad))
    stop ("Levels in VK_avslutad have changed.")
  quality.process.done <- data$VK_avslutad == "ja" | mortality.peer.review.done
  ofi <- ifelse(prob, "Yes",
                ifelse(quality.process.done & !prob, "No", NA))
  ofi[quality.process.done & is.na(prob)] <- "No"
  return (ofi)
}
