######### You need to have "id" already done in 
######### combined.datasets and kvalgranskning2014.2017.

data1 <- combined.datasets
data2 <- kvalgranskning2014.2017

### Paket som möjliggör visualisering och jämförelse av exv class för olika kolumner/dataset
install.packages("janitor")
library("janitor")
jmf <- compare_df_cols(combined.datasets, kvalgranskning2014.2017)
k <- subset(jmf, is.na(jmf$kvalgranskning2014.2017)== FALSE & is.na(jmf$combined.datasets)== FALSE & jmf$combined.datasets != jmf$data2)

### Make sure collumns are same class ###

data1$dt_alarm_hosp <- as.numeric(data1$dt_alarm_hosp)
data1$dt_alarm_scene <- as.numeric(data1$dt_alarm_scene)
data1$dt_ed_emerg_proc <- as.numeric(data1$dt_ed_emerg_proc)
data1$dt_ed_first_ct <- as.numeric(data1$dt_ed_first_ct)
data1$ISS <- as.numeric(data1$ISS)
data1$NISS <- as.numeric(data1$NISS)
data1$pt_age_yrs <- as.numeric(data1$pt_age_yrs)

data1$DateTime_ArrivalAtHospital <- as.POSIXct(data1$DateTime_ArrivalAtHospital)
data1$DateTime_LeaveScene  <- as.POSIXct(data1$DateTime_LeaveScene )
data1$DateTime_of_Alarm  <- as.POSIXct(data1$DateTime_of_Alarm)
data1$DateTime_Of_Trauma <- as.POSIXct(data1$DateTime_Of_Trauma)
data1$DOB <- as.POSIXct(data1$DOB)
data1$DateTime_ArrivalAtScene <- as.POSIXct(data1$DateTime_ArrivalAtScene)

#### Merge with all.x == TRUE and all.y=TRUE to not loose patients,
#### THIS is wrong, patients are doubled. check patient wir "Triage på akm" under problemområden.

data3 <- merge(setDT(data1), setDT(data2), all.x = TRUE, all.y = TRUE,
               by = c("id"))

### IF k have 0 observations below alla collumns have same classes?
jmf <- compare_df_cols(data1, data2)
k <- subset(jmf, is.na(jmf$data2)== FALSE & is.na(jmf$data1)== FALSE & jmf$data1 != jmf$data2)

### Was converted to data.table and data.frame before. 
data4 <- as.data.frame(data3)

##### Arrival is allready dublicated in previous merges?
data5 <- subset(data4, select = -c(arrival.x,arrival.y))
#### Get collumns that overlap, .x = from combined.datasets and .y is from kvalgranskning2014.2017???
col.names.x <- names(select(data5,ends_with(".x")))

### Needed matching orders for .x and .y so i manually did it. 
col.names.y <- c("Ankomst_te.y","Personnummer.y","Reservnummer.y","tra_id.y","pat_id.y",
                 "DOB.y", "pt_age_yrs.y","Gender.y","pt_Gender.y","pre_card_arrest.y",
                 "ed_gcs_sum.y","hosp_dischg_dest.y","res_gos_dischg.y","ISS.y","NISS.y",
                 "DateTime_Of_Trauma.y","DateTime_of_Alarm.y","DateTime_ArrivalAtScene.y",
                 "DateTime_LeaveScene.y","DateTime_ArrivalAtHospital.y","DateTime_FirstTraumaDT.y",
                 "DateTime_StartofTreatment.y","dt_alarm_hosp.y","pre_intubated.y","pre_intub_type.y",
                 "ed_intubated.y","ed_intub_type.y","ed_emerg_proc.y","ed_emerg_proc_other.y",
                 "host_care_level.y","dt_alarm_scene.y","dt_ed_first_ct.y","dt_ed_emerg_proc.y")

######
# Loop keeps values from .x (combined.datasets), 
# but if they are empty insert values from kvalgranskning2014.2017 insted.
######
for (x in 1:length(col.names.y)){
  data4[,col.names.x[x]] <- with(data4, ifelse(is.na(data4[,col.names.x[x]]) == TRUE & is.na(data4[,col.names.y[x]]) == FALSE,
                                               data4[,col.names.y[x]], data4[,col.names.x[x]]))
}

### Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
data4[,col.names.y] <- NULL

### Columns that should be translated into another column, cant find another way but manual? 


VK.colnames <- c("VK_hlr_thorak","VK_sap_less90","VK_iss_15_ej_iva",
                 "VK_gcs_less9_ej_intubTE","VK_mer_30min_DT","VK_mer_60min_interv")

kval.colnames <- c("Antal_thorakotomier_JN","sap_less_90_JN","ISS_moore_15_not_IVA",
                   "gcs_less_9_ej_intub_TE","DT_moore_30.minuter_ejTR3","akut_intervent_moore_60_min")


for (x in 1:length(VK.colnames)){

  data4[,VK.colnames[x]] <- with(data4, ifelse(data4[,kval.colnames[x]] == 1 & is.na(data4[,VK.colnames[x]]) == TRUE, "Ja", data4[,VK.colnames[x]]))
  data4[,VK.colnames[x]] <- with(data2, ifelse(data4[,kval.colnames[x]] == 0 & is.na(data4[,VK.colnames[x]]) == TRUE, "Nej", data4[,VK.colnames[x]]))
  }
# Now all kval.colnames should be in VK_ collumns instead, hence remove

data4[,kval.colnames] <- NULL

### Convert problemområde to problemområde_.FMP
data4$Problemomrade_.FMP <- with(data4, ifelse(is.na(data4$Problemomrade_.FMP) == TRUE, `problemområde`, `Problemomrade_.FMP`))

data4[,"problemområde"] <- NULL
### Need to add something for VK_avslutad to get them through create_ofi?

data4$VK_avslutad <- with(data4, ifelse(is.na(data4$bedomn_primar_granskning) == FALSE, "Ja", `VK_avslutad`))
### Dont want to remove "bedomn_primar_granskning" since its an easy way of identifying patients from kvaldata.

### combined.datasets2 now contains 10 more collumns
# (8 with unique/new info and "pat_personnummer","pat_TempPersonnummer" since you use them for ID.
combined.datasets2 <- data4
