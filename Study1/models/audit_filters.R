# Create dataset and ofi column
#datasets <- rofi::import_data(test = TRUE)
#combined.datasets <- merge(datasets$fmp_scrambled, datasets$problem_scrambled, by = "id", all.x = TRUE)
#combined.datasets <- merge(combined.datasets, datasets$swetrau_scrambled, by = "id", all.x = TRUE)
#combined.datasets$mom <-combined.datasets$Problemomrade_.FMP.is.nay
#combined.datasets$ofi <- rofi::create_ofi(combined.datasets)


dataset <- read.csv(file = 'data/ofi.csv')


audit_filters_create <- function(data){
  audit_filters <- data.frame(id=data$id)
  
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

test_audit_filters_create <- function(data){
  audit_filters <- audit_filters_create(dataset)
  
  test <- data.frame(id=data$id)
  
  test$hlr_thorak <- audit_filters$hlr_thorak == dataset$VK_hlr_thorak
  test$sap_less90 <- audit_filters$sap_less90 == dataset$VK_sap_less90
  test$leverskada <- audit_filters$leverskada == dataset$VK_leverskada
  test$gcs_less9_ej_intubTE <- audit_filters$gcs_less9_ej_intubTE == dataset$VK_gcs_less9_ej_intubTE
  test$mjaltskada <- audit_filters$mjaltskada == dataset$VK_mjaltskada
  test$mer_30min_DT <- audit_filters$mer_30min_DT == dataset$VK_mer_30min_DT
  test$mass_transf <- audit_filters$mass_transf == dataset$VK_mass_transf
  test$mer_60_min_interv <- audit_filters$mer_60_min_interv == dataset$VK_mer_60min_interv
  test$iss_15_ej_iva <- audit_filters$iss_15_ej_iva == dataset$VK_iss_15_ej_iva
  test$ej_trombrof_TBI_72h <- audit_filters$ej_trombrof_TBI_72h == dataset$VK_ej_trombrof_TBI_72h
  test$iss_15_ej_TE <- audit_filters$iss_15_ej_TE == dataset$VK_iss_15_ej_TE
  
  print("Manual vs Auto")
  print(nrow(test) - colSums(within(test, rm(id))))
}

test_audit_filters_create(dataset)

# AUC: 0.6399