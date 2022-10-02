#######################
### Clean predictors ##
#######################
#
# Todo: Clean all possible predictors
# Integrate cont/cat.var.no.99 into cont/cat.var instead of having two different vectors.
# something beside summary() to check correct formats?

clean_all_predictors <- function(data) {

###cont
cont.var<- c("pt_age_yrs","pre_gcs_sum","pre_gcs_motor","ed_gcs_sum","ed_gcs_motor",
             "pre_sbp_value","ed_sbp_value","pre_rr_value","ed_rr_value","ed_be_art",
             "ed_inr","hosp_vent_days","hosp_los_days","res_gos_dischg","ISS","NISS",
             "NumberOfActions","NumberOfInjuries")  

###cat 
cat.var <- c("Gender","pt_Gender","ed_be_art_NotDone","hosp_dischg_dest","res_survival",           
             "inj_dominant","inj_mechanism","inj_intention","pt_asa_preinjury","pre_card_arrest",
             "pre_sbp_rtscat","ed_sbp_rtscat","pre_rr_rtscat","ed_rr_rtscat","ed_inr_NotDone",
             "host_vent_days_NotDone","pre_provided","pre_intubated","pre_intub_type",
             "ed_intubated","ed_intub_type","ed_tta","ed_emerg_proc_other","ed_emerg_proc","pre_transport")

######
# get the collumns not to be checked.
######
test.data <- remove_columns(data)
test.data <- as.data.frame(test.data)
test.data <- test.data[,-grep("Fr", colnames(test.data))]
test.data <- test.data[,-grep("DateTime_", colnames(test.data))]
test.data <- test.data[,-grep("dt_", colnames(test.data))]


######## Need to check these collumns, in the future so they follow the swetrau manual and is formated correctly. 

columnnames <- colnames(subset(test.data,
                               select = -c(did,tra_id,pat_id,Sjukhuskod,PersonIdentity,TempIdentity,DOB,Deceased,DeceasedDate,
                                           TraumaAlarmCriteria,TraumaAlarmAtHospital,AlarmRePrioritised,FirstTraumaDT_NotDone,   
                                           host_transfered, host_care_level,Date_Discharged,tra_DodsfallsanalysGenomford,
                                           id,arrival,ISS_moore_15_trauma_3,bedomn_primar_granskning,pat_personnummer,
                                           ISS_less_15_trauma_1_2,pat_TempPersonnummer,korrekt_triage,tid_skadeplats_moore_20min,
                                           tid_skadeplats,GCS_less13_DT_moore_120,Riktlinje,origin,Ankomst_te,Personnummer,
                                           Reservnummer,Kön,Tr_Nivå,tillavdelning_Direkt_eft_TE_AKM,IVA_efter_TE,IVA_eft_avd,
                                           iva_dagar_n,iva_vardtillfallen_n,Död.datum,uppfolj_30_dgr,Klar,SweTr,
                                           Flyttad_till_avd_eft_iva,inlagd_swetrau,waran_beh_vid_ank,noak_vid_ankomst,
                                           Problemomrade_.FMP,ofi )
)
)

#####
# Change obvios values to NA (Like 999)
#####

cont.var.no.99<- c("pre_gcs_sum","pre_gcs_motor","ed_gcs_sum","ed_gcs_motor","pre_rr_value","ed_rr_value","ed_be_art",
                   "ed_inr","ISS","NISS","NumberOfActions","NumberOfInjuries") 

# Note that ED_GCS 99 == Intubated in a prehospital seting but we need to remove to calculate correct median/mean

cat.var.no.99 <- c("Gender","pt_Gender","ed_be_art_NotDone","hosp_dischg_dest","res_survival",           
             "inj_dominant","inj_mechanism","inj_intention","pt_asa_preinjury","pre_card_arrest",
             "pre_sbp_rtscat","ed_sbp_rtscat","pre_rr_rtscat","ed_rr_rtscat","ed_inr_NotDone",
             "host_vent_days_NotDone","pre_provided","pre_intubated","pre_intub_type",
             "ed_intubated","ed_intub_type","ed_tta","ed_emerg_proc_other","res_gos_dischg",
             "pre_transport")

# Note that ed_emergency_proc 99 == No action

test.data <- data[,columnnames]

test.data[test.data == 999] <- NA
test.data[test.data == 9999] <- NA #### Consider removing since 9999 actually means not relevant?
## 99 is only rarely something of meaning
test.data[, cont.var.no.99][test.data[, cont.var.no.99] == 99]<- NA 
test.data[, cat.var.no.99][test.data[, cat.var.no.99] == 99] <- NA

##### Fix formating to factor/numeric for easy screening
clean.data <- test.data

clean.data[cat.var] <- lapply(clean.data[cat.var], factor)
clean.data$ed_be_art = str_replace(clean.data$ed_be_art,",",".")
clean.data$ed_inr = str_replace(clean.data$ed_inr,",",".")
clean.data$ed_inr <- as.numeric(clean.data$ed_inr)
clean.data$ed_be_art <- as.numeric(clean.data$ed_inr)
clean.data$NumberOfActions <- as.numeric(clean.data$NumberOfActions)
clean.data$NumberOfInjuries <- as.numeric(clean.data$NumberOfInjuries)

###### I Used summary to check each category, to my knowledge they follow the swetrau-manual
###### Need to add some better screening tool

data[columnnames] <- clean.data
return(data)

}
