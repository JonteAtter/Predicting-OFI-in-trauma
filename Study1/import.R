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

swe$DateTime_ArrivalAtHospital <- as.POSIXct(strptime(swe$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
swe$DateTime_LeaveScene  <- as.POSIXct(strptime(swe$DateTime_LeaveScene, format = "%Y-%m-%d %H:%M"))
swe$DateTime_of_Alarm  <- as.POSIXct(strptime(swe$DateTime_of_Alarm, format = "%Y-%m-%d %H:%M"))
swe$DateTime_Of_Trauma <- as.POSIXct(strptime(swe$DateTime_Of_Trauma, format = "%Y-%m-%d %H:%M"))
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
#swetraukval2$DateTime_ArrivalAtHospital.x <- swetraukval2[swetraukval2$pat_id =='33857', "DateTime_ArrivalAtHospital.y"]
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


###### I WANT TO HAVE all.y = TRUE to add the last 5 patients
jmf <- compare_df_cols(swe, kval2)
k <- subset(jmf, is.na(jmf$kval2)== FALSE & is.na(jmf$swe)== FALSE & jmf$swe != jmf$kval)

swetraukval3 <- merge(setDT(swe), setDT(kval2), all.x = TRUE, all.y = TRUE,
                     by = c("tra_id"))

#### STILL NEED TO CHECK WHERE pat_id:s x and y/personnummer Dont match despite having same tra_id, se two patients:
# swetraukval3[swetraukval3$pat_id.x != swetraukval3$pat_id.y]. i Think patients in kvalgransk with pat_id: 33973 and 33922 should have tra_id 36078 and 
kval3 <- kval2

kval3[is.element(kval3$pat_id,c(33973)),"tra_id"] <- c(36078)
kval3[is.element(kval3$pat_id,c(33922)),"tra_id"] <- c(36024)

swetraukval4 <- merge(setDT(swe), setDT(kval3), all.x = TRUE, all.y = TRUE,
                      by = c("tra_id"))

#### Need to clean swetraukval

##################### Clean new file
#####################

#### Get collumns that overlap, .x = from swe and .y is from kval
col.names.x <- names(select(swetraukval4,ends_with(".x")))
### Needed matching orders for .x and .y so i manually did it. 

col.names.y <- c("pat_id.y","DOB.y","pt_age_yrs.y","Gender.y","pt_Gender.y","pre_card_arrest.y",
"ed_gcs_sum.y","hosp_dischg_dest.y","res_gos_dischg.y","ISS.y","NISS.y","DateTime_Of_Trauma.y",        
"DateTime_of_Alarm.y","DateTime_ArrivalAtScene.y","DateTime_LeaveScene.y","DateTime_ArrivalAtHospital.y",
"DateTime_FirstTraumaDT.y","DateTime_StartofTreatment.y","dt_alarm_hosp.y",
"pre_intubated.y","pre_intub_type.y","ed_intubated.y","ed_intub_type.y",
"ed_emerg_proc.y","ed_emerg_proc_other.y","host_care_level.y",
"dt_alarm_scene.y","dt_ed_first_ct.y","dt_ed_emerg_proc.y")

sk <- as.data.frame(swetraukval4)

######
# Loop keeps values from .x (swe), 
# but if they are empty insert values from .y (kval) insted.
# Manually checked, extremely rare for both .x and .y to exist and when it does, .y is usually an obvious error.
######
for (x in 1:length(col.names.x)){
  sk[is.na(sk[,col.names.x[x]]) == TRUE, col.names.x[x]] <- 
    sk[is.na(sk[,col.names.x[x]]) == TRUE,col.names.y[x]]
}

### Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
sk[,col.names.y] <- NULL

########### combine personnummer collumns
sk[is.na(sk[,"PersonIdentity"]) == TRUE, "PersonIdentity"] <- 
  sk[is.na(sk[,"PersonIdentity"]) == TRUE,"pat_personnummer"]

sk[is.na(sk[,"TempIdentity"]) == TRUE, "TempIdentity"] <- 
  sk[is.na(sk[,"TempIdentity"]) == TRUE,"pat_TempPersonnummer"]

###########
swekval <- as.data.frame(sk)

###########
## Copied format from ROFI to add ID and match problem/fmp
###########
## Format datetime variable
swekval$arrival <- as.POSIXct(strptime(swekval$DateTime_ArrivalAtHospital.x, format = "%Y-%m-%d"))
fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d"))
problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d"))

#swekval$arrival <- as.POSIXct(strptime(swekval$DateTime_ArrivalAtHospital.x, format = "%Y-%m-%d %H:%M"))
#fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d %H:%M"))
#problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d %H:%M"))

## Create id variable by pasting arrival time and hashed identify
swekval$id <- with(swekval, paste(arrival, PersonIdentity))
fmp$id <- with(fmp, paste(arrival, Personnummer))
problem$id <- with(problem, paste(arrival, Personnummer))
#### To the use od "id" even when you only use date + personnummer still results in +700 defect "extra" patients that should not exist.
#### How many should match? - atleast 11859 - all patients from fmp/problem exist in swe! Se below.

ids <- unique(fmp$Personnummer)
ids2 <- unique(problem$Personnummer)

match <- swe[is.element(swe$PersonIdentity,ids),]
match2 <- swe[is.element(swe$PersonIdentity,ids2),]

## Combine datasets
## i think you can merge with ("id","Ankomst_te","arrival","Personnummer","Reservnummer")? If you merge with just ID, the created .x and .y columns match.
combined.datasets <- merge(fmp, problem, by = c("id","Ankomst_te","arrival","Personnummer","Reservnummer"), all.x = TRUE, all.y = TRUE)

### Merge only with personnummer when possible when combining with swetrau and kval? 
## How many Pats exist more than once? - 306
double <- setDT(swekval)[, .N, by=PersonIdentity][N > 1L]$PersonIdentity

combined.datasets1 <- merge(swekval[!is.element(swekval$PersonIdentity,double),], combined.datasets[!is.element(combined.datasets$Personnummer,double),]
                            , by.x = "PersonIdentity", by.y = "Personnummer", all.x = TRUE, all.y = TRUE)

combined.datasets2 <- merge(swekval[is.element(swekval$PersonIdentity,double),], combined.datasets[is.element(combined.datasets$Personnummer,double),]
                            , by = "id", all.x = TRUE, all.y = TRUE)

### THe above still results in 12724 patients in total = to many.
##########
## Bigger problem here, with all.y = TRUE you get 12730 patients, 700+ dont match?
#######
combined.datasets <- merge(swekval, combined.datasets , by = "id", all.x = TRUE, all.y = TRUE)


######## THE BELOW IS FOR when combined.datasets are done

#swekval <- combined.datasets
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
