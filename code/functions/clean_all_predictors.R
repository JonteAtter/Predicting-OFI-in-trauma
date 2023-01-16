#######################
### Clean predictors ##
#######################
#
# TODO:something beside summary() to check correct formats?
# Fix dates?
# Integrate RTS?
# Not done columns?
#data <- dataset.clean.af
clean_all_predictors <- function(data) {

  data <- as.data.frame(data)
  data$intub <- with(data, ifelse(`pre_intubated` == 1 & is.na(data$pre_intubated) == FALSE, 3, `ed_intubated`))  
  
###cont
cont.var<- c("pt_age_yrs","pre_gcs_sum","pre_gcs_motor","ed_gcs_sum","ed_gcs_motor",
             "pre_sbp_value","ed_sbp_value","pre_rr_value","ed_rr_value","ed_be_art",
             "ed_inr","hosp_vent_days","hosp_los_days","res_gos_dischg","ISS","NISS",
             "NumberOfActions","NumberOfInjuries","iva_dagar_n","iva_vardtillfallen_n")  

###cat 
cat.var <- c("intub","Gender","pt_Gender","ed_be_art_NotDone","dt_ed_norm_be","hosp_dischg_dest","res_survival",           
             "inj_dominant","inj_mechanism","inj_intention","pt_asa_preinjury","pre_card_arrest",
             "pre_sbp_rtscat","ed_sbp_rtscat","pre_rr_rtscat","ed_rr_rtscat","ed_inr_NotDone",
             "host_vent_days_NotDone","pre_provided","pre_intubated","pre_intub_type",
             "ed_intubated","ed_intub_type","ed_tta","ed_emerg_proc_other","ed_emerg_proc","pre_transport",
             "Deceased","bedomn_primar_granskning","Riktlinje",
             "waran_beh_vid_ank","noak_vid_ankomst",
             "TraumaAlarmCriteria","TraumaAlarmAtHospital","AlarmRePrioritised",
             "FirstTraumaDT_NotDone","ISS_less_15_trauma_1_2","korrekt_triage",
             "tid_skadeplats_moore_20min","host_transfered", "host_care_level",
             "tra_DodsfallsanalysGenomford","ISS_moore_15_trauma_3","GCS_less13_DT_moore_120",
             "Kön","Tr_Nivå","tillavdelning_Direkt_eft_TE_AKM","IVA_efter_TE",
             "IVA_eft_avd","Flyttad_till_avd_eft_iva","Problemomrade_.FMP","ofi")

vars <- c(cat.var,cont.var)

#############################################
# Change obvios values to NA (Like 999 etc) #
#############################################


## Vars.99 = Vars that that should contain 99
# "ed_emergency_proc" 99 == No action
# "TraumaAlarmAtHospital" 99 == No trauma alarm
# Note that ed_gcs_sum 99 == Intubated in a prehospital seting but we need to remove to calculate correct median/mean
var.99 <- c("ed_emergency_pro","TraumaAlarmAtHospital","pt_age_yrs","pre_sbp_value","ed_sbp_value","hosp_vent_days","hosp_los_days","iva_dagar_n","iva_vardtillfallen_n")

test.data <- data[,vars]
test.data[test.data == 999] <- NA
test.data[test.data == 9999] <- NA #### Consider removing since 9999 actually means not relevant?
test.data[, -which(names(test.data) %in% var.99)][test.data[, -which(names(test.data) %in% var.99)] == 99 ] <- NA


##### Fix formating to factor/numeric for easy screening

formated.data <- test.data
formated.data[cat.var] <- lapply(formated.data[cat.var], factor)
formated.data$ed_be_art = str_replace(formated.data$ed_be_art,",",".")
formated.data$ed_inr = str_replace(formated.data$ed_inr,",",".")
formated.data$ed_inr <- as.numeric(formated.data$ed_inr)
formated.data$ed_be_art <- as.numeric(formated.data$ed_inr)
formated.data$NumberOfActions <- as.numeric(formated.data$NumberOfActions)
formated.data$NumberOfInjuries <- as.numeric(formated.data$NumberOfInjuries)

formated.data$dt_ed_norm_be <- as.numeric(formated.data$dt_ed_norm_be)
###### I Used summary to check each category, to my knowledge they follow the swetrau-manual
###### Need to add some better screening tool

data[vars] <- formated.data

return(data)

}
