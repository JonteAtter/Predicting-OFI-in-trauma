# Not needed if this is done before id and arrival are added.
kval <- kvalgranskning2014.2017[,-c("id","arrival")]
swe <- swetrau[,-c("id","arrival")]

library("janitor")
jmf <- compare_df_cols(swe, kval)
k <- subset(jmf, is.na(jmf$kval)== FALSE & is.na(jmf$swe)== FALSE & jmf$swe != jmf$kval)


### Make sure collumns are same class ###

swe$dt_alarm_hosp <- as.numeric(swe$dt_alarm_hosp)
swe$dt_alarm_scene <- as.numeric(swe$dt_alarm_scene)
swe$dt_ed_emerg_proc <- as.numeric(swe$dt_ed_emerg_proc)
swe$dt_ed_first_ct <- as.numeric(swe$dt_ed_first_ct)
swe$ISS <- as.numeric(swe$ISS)
swe$NISS <- as.numeric(swe$NISS)
swe$pt_age_yrs <- as.numeric(swe$pt_age_yrs)

swe$DateTime_ArrivalAtHospital <- as.POSIXct(swe$DateTime_ArrivalAtHospital)
swe$DateTime_LeaveScene  <- as.POSIXct(swe$DateTime_LeaveScene )
swe$DateTime_of_Alarm  <- as.POSIXct(swe$DateTime_of_Alarm)
swe$DateTime_Of_Trauma <- as.POSIXct(swe$DateTime_Of_Trauma)
swe$DOB <- as.POSIXct(swe$DOB)
swe$DateTime_ArrivalAtScene <- as.POSIXct(swe$DateTime_ArrivalAtScene)



## Manually found only missing tra_id in kval via swetrau, there tra_id = 24432( No tra_id in kval put pat_id and times match)
kval[285,"tra_id"] <- 24432

### In swetraukval there is 14 "extra" patients 
swetraukval <- merge(setDT(swe), setDT(kval), all.x = TRUE, all.y = TRUE,
                     by = c("tra_id"))
## To find trau_id:s that dont match in kval and swe.
id <- swe$tra_id

#manuel screening shows no pat_id.x or any other .collumns but all .y collumns exist.
k <- swetraukval[!is.element(swetraukval$tra_id, id),]

## to se if there are doubles?

### To get x.collumns
swekval.x <- names(select(swetraukval,ends_with(".x")))
k <- as.data.frame(k)

k3 <-k[,swekval.x]
table(is.na(k3)) ### all is true so no .x values -> no values from swe only kval. 

### Check if same patients exist in swetrau?
patid <- k$pat_id.y 
k4 <- swetrau[is.element(swe$pat_id,patid), ] ###Still 9 patients have same pat id in swe suggesting incorrect trau_id?
#save remaining 5 patients (14 - 9) for later.
new.pat <- k[!is.element(k$pat_id.y,k4$pat_id),]
new.pat.id <- new.pat$pat_id.y

k5 <- k4[,c("tra_id","pat_id","DOB","DateTime_ArrivalAtHospital")] ## to manually screen the 9

swetraukval2 <- merge(setDT(k5), setDT(kval[,c("tra_id","pat_id","DOB","DateTime_ArrivalAtHospital")]), all.x = TRUE,
                     by = c("pat_id"))

### Manuell screening of swetrau2 gives: same pat_id, DOB and same DateTime_ArrivalAtHospital (patient with pat_it 33857 differs 1 min)
### suggesting wrong trau_id.
## fix wrong time
swetraukval2$DateTime_ArrivalAtHospital.x <- swetraukval2[swetraukval2$pat_id =='33857', "DateTime_ArrivalAtHospital.y"]
#fix wrong tra_id

## setting up vectors for identification and replacement.
kval2 <- kval
tra_id.g.kval <- swetraukval2$tra_id.y
tra_id.g.swe <- swetraukval2$tra_id.x

## Loop to change values 
for (x in 1:length(tra_id.g.kval)){
  kval2[kval2$tra_id == tra_id.g.kval[x],"tra_id"] <-  tra_id.g.swe[x]
}

# New merge, now with 5 unknow patients if all.y == TRUE
# I checked for pat_personnummer and pat_id, they dont exist in swetrau = New patients.

swetraukval3 <- merge(setDT(swe), setDT(kval2), all.x = TRUE, all.y = TRUE,
                     by = c("tra_id"))

#### STILL NEED TO CHECK WHERE pat_id:s/personnummer Dont match, se two patients:
#View(swetraukval3[swetraukval3$pat_id.x != swetraukval3$pat_id.y])

swekval <- as.data.frame(swetraukval3)
##################### Clean new file
#####################

#### Get collumns that overlap, .x = from swe and .y is from kval
col.names.x <- names(select(swekval,ends_with(".x")))
### Needed matching orders for .x and .y so i manually did it. 
col.names.y <- c("pat_id.y","DOB.y","pt_age_yrs.y","Gender.y"                    
                 ,"pt_Gender.y","pre_card_arrest.y","ed_gcs_sum.y","hosp_dischg_dest.y"          
                 ,"res_gos_dischg.y","ISS.y","NISS.y","DateTime_Of_Trauma.y"        
                 ,"DateTime_of_Alarm.y","DateTime_ArrivalAtScene.y"   
                 ,"DateTime_LeaveScene.y","DateTime_ArrivalAtHospital.y"
                 ,"DateTime_FirstTraumaDT.y","DateTime_StartofTreatment.y" 
                 ,"dt_alarm_hosp.y","pre_intubated.y","pre_intub_type.y","ed_intubated.y"              
                 ,"ed_intub_type.y","ed_emerg_proc.y","ed_emerg_proc_other.y","host_care_level.y"           
                 ,"dt_alarm_scene.y","dt_ed_first_ct.y","dt_ed_emerg_proc.y")

######
# Loop keeps values from .x (swe), 
# but if they are empty insert values from .y (kval) insted.
######
for (x in 1:length(col.names.y)){
  swekval[,col.names.x[x]] <- with(swekval, ifelse(is.na(swekval[,col.names.x[x]]) == TRUE & is.na(swekval[,col.names.y[x]]) == FALSE,
                                               swekval[,col.names.y[x]], swekval[,col.names.x[x]]))
}

### Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
swekval[,col.names.y] <- NULL

###########
## Copied format from ROFI to add ID and match problem/fmp
###########
## Format datetime variable
swekval$arrival <- as.POSIXct(strptime(swekval$DateTime_ArrivalAtHospital.x, format = "%Y-%m-%d %H:%M"))
fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d %H:%M"))
problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d %H:%M"))

## Create id variable by pasting arrival time and hashed identify
swekval$id <- with(swekval, paste(arrival, PersonIdentity, TempIdentity))
fmp$id <- with(fmp, paste(arrival, Personnummer, Reservnummer))
problem$id <- with(problem, paste(arrival, Personnummer, Reservnummer))

## Combine datasets
combined.datasets <- merge(fmp, problem, by = "id", all.x = TRUE, all.y = TRUE)
##########
## Bigger problem here, with all.y = TRUE you get 23k+ patients?
#######
combined.datasets <- merge(swekval, combined.datasets , by = "id", all.x = TRUE, all.y = TRUE)

swekval <- combined.datasets
######################
######## Add after addition of problem and fmp file!
######################
### Columns that should be translated into another column, cant find another way but manual? 

VK.colnames <- c("VK_hlr_thorak","VK_sap_less90","VK_iss_15_ej_iva",
                 "VK_gcs_less9_ej_intubTE","VK_mer_30min_DT","VK_mer_60min_interv")

kval.colnames <- c("Antal_thorakotomier_JN","sap_less_90_JN","ISS_moore_15_not_IVA",
                   "gcs_less_9_ej_intub_TE","DT_moore_30.minuter_ejTR3","akut_intervent_moore_60_min")


for (x in 1:length(VK.colnames)){
  
  swekval[,VK.colnames[x]] <- with(swekval, ifelse(swekval[,kval.colnames[x]] == 1 & is.na(swekval[,VK.colnames[x]]) == TRUE, "Ja", swekval[,VK.colnames[x]]))
  swekval[,VK.colnames[x]] <- with(swekval, ifelse(swekval[,kval.colnames[x]] == 0 & is.na(swekval[,VK.colnames[x]]) == TRUE, "Nej", swekval[,VK.colnames[x]]))
}
# Now all kval.colnames should be in VK_ collumns instead, hence remove

swekval[,kval.colnames] <- NULL

### Convert problemområde to problemområde_.FMP
swekval$Problemomrade_.FMP <- with(swekval, ifelse(is.na(swekval$Problemomrade_.FMP) == TRUE, `problemområde`, `Problemomrade_.FMP`))

swekval[,"problemområde"] <- NULL

### Need to fill in VK_avslutad to get them through create_ofi?
## Need to get some data for mortality?

swekval$VK_avslutad <- with(swekval, ifelse(is.na(swekval$bedomn_primar_granskning) == FALSE, "Ja", `VK_avslutad`))
### Dont want to remove "bedomn_primar_granskning" since its an easy way of identifying patients from kvaldata.
