
##############
# Imputation # 
##############


imputation <- function(dataset) {
  
  ## continuous variables
  #cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value","hosp_los_days","iva_dagar_n") 
  
  ## categorical variables
  #cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ###cont
  
  cont.var <- colnames(dataset %>% select_if(is.numeric))


  cat.var <- colnames(dataset %>% select_if(is.factor))
#  cont.var<- c("pt_age_yrs","pre_gcs_sum","pre_gcs_motor","ed_gcs_sum","ed_gcs_motor",
#               "pre_sbp_value","ed_sbp_value","pre_rr_value","ed_rr_value","ed_be_art",
#               "ed_inr","hosp_vent_days","hosp_los_days","res_gos_dischg","ISS","NISS",
#               "NumberOfActions","NumberOfInjuries","iva_dagar_n","iva_vardtillfallen_n")  
#  
  ###cat 
#  cat.var <- c("Gender","pt_Gender","ed_be_art_NotDone","hosp_dischg_dest","res_survival",           
#               "inj_dominant","inj_mechanism","inj_intention","pt_asa_preinjury","pre_card_arrest",
#               "pre_sbp_rtscat","ed_sbp_rtscat","pre_rr_rtscat","ed_rr_rtscat","ed_inr_NotDone",
#               "host_vent_days_NotDone","pre_provided","pre_intubated","pre_intub_type",
#               "ed_intubated","ed_intub_type","ed_tta","ed_emerg_proc_other","ed_emerg_proc","pre_transport",
#               "Deceased","bedomn_primar_granskning","Riktlinje",
#               "waran_beh_vid_ank","noak_vid_ankomst",
#               "TraumaAlarmCriteria","TraumaAlarmAtHospital","AlarmRePrioritised",
#               "FirstTraumaDT_NotDone","ISS_less_15_trauma_1_2","korrekt_triage",
#               "tid_skadeplats_moore_20min","host_transfered", "host_care_level",
#               "tra_DodsfallsanalysGenomford","ISS_moore_15_trauma_3","GCS_less13_DT_moore_120",
#               "Kön","Tr_Nivå","tillavdelning_Direkt_eft_TE_AKM","IVA_efter_TE",
#               "IVA_eft_avd","Flyttad_till_avd_eft_iva","Problemomrade_.FMP","ofi")
  
  ## Variables used for sorting
  #time.id.var <- c("arrival", "id")                                              
  variables <- c(cont.var, cat.var)
  model.variables <- c(cont.var, cat.var)
  
  # Create new dataframe - missing.indicator.variables - Containing true/false if a value is imputet
  missing.indicator.variables <- as.data.frame(lapply(dataset, function(data) is.na(data)))
  missing.indicator.variables[, c("ofi")] <- NULL
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
