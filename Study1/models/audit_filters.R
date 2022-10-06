audit_filters_create <- function(data){
  audit_filters <- data.frame(matrix(nrow = nrow(data), ncol = 0)) 
  
  # VK_hlr_thorak
  audit_filters$hlr_thorak <- as.logical(data$VK_hlr_thorak)
  
  # VK_sap_less90
  audit_filters$sap_less90 <- data$ed_sbp_value < 90 #| data$ed_sbp_rtscat <= 3
  audit_filters$sap_less90[is.na(audit_filters$sap_less90)] <- FALSE
  
  # VK_leverskada
  # ais codes?
  audit_filters$leverskada <- as.logical(data$VK_leverskada)
  
  
  # VK_gcs_less9_ej_intubTE
  # Include ed_intub_type?
  audit_filters$gcs_less9_ej_intubTE <- data$ed_gcs_sum < 9 & data$ed_intubated != 1
  audit_filters$gcs_less9_ej_intubTE[is.na(audit_filters$gcs_less9_ej_intubTE)] <- FALSE
  
  # VK_mjaltskada
  # ais codes?
  audit_filters$mjaltskada <- as.logical(data$VK_mjaltskada)
  
  
  # VK_mer_30min_DT
  audit_filters$mer_30min_DT <- data$dt_ed_first_ct > 30
  audit_filters$mer_30min_DT[is.na(audit_filters$mer_30min_DT)] <- FALSE
  
  
  # VK_mass_transf
  audit_filters$mass_transf <- as.logical(data$VK_mass_transf)
  
  
  # VK_mer_60_min_interv
  audit_filters$mer_60_min_interv <- data$dt_ed_emerg_proc > 60
  audit_filters$mer_60_min_interv[is.na(audit_filters$mer_60_min_interv)] <- FALSE
  
  # VK_iss_15_ej_iva
  # Check if vent days done?
  audit_filters$iss_15_ej_iva <- data$ISS >= 15 & data$host_care_level != 5
  audit_filters$iss_15_ej_iva[is.na(audit_filters$iss_15_ej_iva)] <- FALSE
  
  # VK_ej_trombrof_TBI_72h
  audit_filters$ej_trombrof_TBI_72h <- as.logical(data$VK_ej_trombrof_TBI_72h)
  
  
  # VK_iss_15_ej_TE
  # Include reprioritised alarms?
  audit_filters$iss_15_ej_TE <- data$ISS >= 15 & data$TraumaAlarmAtHospital != 1
  audit_filters$iss_15_ej_TE[is.na(audit_filters$iss_15_ej_TE)] <- FALSE
  
  return(audit_filters)
}

audit_filters_predict <- function(data){
  audit_filters <- audit_filters_create(data)
  
  preds <- as.integer(rowSums(within(audit_filters, rm(id))) >= 1)
  
  return(preds)
}