## Import data
datasets <- rofi::import_data()

merge_data <- function(datasets) {

kval <- datasets$kvalgranskning2014.2017
swe <- datasets$swetrau

## Create variable origin in kval, to be able to check how many from
## kval are later present in the merged data
kval$origin <- "kval"

## Create id variable, which is equal to personnummer if present,
## otherwise temporary id (reservnummer)
kval$id <- kval$pat_personnummer
kval$id[is.na(kval$pat_personnummer)] <- kval$pat_TempPersonnummer[is.na(kval$pat_personnummer)]
swe$id <- swe$PersonIdentity
swe$id[is.na(swe$id)] <- swe$TempIdentity[is.na(swe$PersonIdentity)]
## Manually found missing tra_id
kval[285,"tra_id"] <- 24432

## Combine id variable with date (year-month-day) of arrival to hospital
kval$did <- paste(kval$id, as.Date(strptime(kval$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M")))
swe$did <- paste(swe$id, as.Date(strptime(swe$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M")))

## Create new date of arrival variable
kval$arrival <- as.Date(strptime(kval$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
swe$arrival <- as.Date(strptime(swe$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))

## Check number of duplicate did in kval and SweTrau
sum(duplicated(kval$did)) ## 0 duplicates
sum(duplicated(swe$did)) ## 2 duplicates

## Check number of patients from kval that are not present in SweTrau
not.in.swetrau <- !(kval$did %in% swe$did)
sum(not.in.swetrau) ## 9 patients from kval are not present in SweTrau

## Check number of matched patients if we merge on did
sum(merge(swe, kval, by = "did", all.x = TRUE)$origin == "kval", na.rm = TRUE) ## 596 patients are matched, i.e. 605 - 9, which makes sense

## The result of the above is the same as if merging on each of the
## variables included in did "separately", i.e:
sum(merge(swe, kval,
          by.x = c("PersonIdentity", "TempIdentity", "arrival"),
          by.y = c("pat_personnummer", "pat_TempPersonnummer", "arrival"),
          all.x = TRUE)$origin == "kval", na.rm = TRUE) ## Also results in 9 missng

## Check if tra_id or pat_id of those in kval whose did is missing from SweTrau
## are present in SweTrau
sum(kval$tra_id[not.in.swetrau] %in% swe$tra_id) ## The tra_id of 4/9 patients are in SweTrau
sum(kval$pat_id[not.in.swetrau] %in% swe$pat_id) ## The pat_id of 4/9 patients are in SweTrau
identical(sum(kval$tra_id[not.in.swetrau] %in% swe$tra_id), sum(kval$pat_id[not.in.swetrau] %in% swe$pat_id)) ## It's the same 4 patients whose tra_id and pat_id are in SweTrau, so it doesn't matter if we try matching on tra_id or pat_id

#################
# NEW CODE - need to change pat_id, screened and know these two are wrong and should be:
kval[is.element(kval$pat_id,c(33973)),"tra_id"] <- c(36078)
kval[is.element(kval$pat_id,c(33922)),"tra_id"] <- c(36024)
#####################


## Try matching some extra patients based on tra_id 
merged <- merge(swe, kval, by = "did", all.x = TRUE)
merged$tra_id.y <- NULL
merged$tra_id <- merged$tra_id.x
merged$tra_id.x <- NULL
kval.not.matched <- kval[not.in.swetrau, ]

##########
# NEW CODE
##########

########
# To keep it clean and not duplicate all columns again i want to clean all .x columns between merges. 
#######
col.names.x <- names(select(merged,ends_with(".x")))
col.names <- gsub('.x', '', col.names.x)
col.names.y <- paste(col.names, ".y", sep="")

######
# Loop keeps values from .x (swe), 
# but if they are empty insert values from .y (kval) insted.
# Manually checked, extremely rare for both .x and .y to exist and when it does, .y is usually an obvious error.
######
merged2 <- merged
for (x in 1:length(col.names.x)){
  merged2[is.na(merged2[,col.names.x[x]]) == TRUE, col.names.x[x]] <- 
    merged2[is.na(merged2[,col.names.x[x]]) == TRUE,col.names.y[x]]
}

### Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
merged2[,col.names.y] <- NULL
#######

## change .x to original names
colnames(merged2)<-gsub(".x","",colnames(merged2))
##

merged <- merged2
#####
# END new code
#####

merged <- merge(merged, kval.not.matched, by = "tra_id", all.x = TRUE)

merged.y <- merged[]

## Compare did on these newly matched cases
with(merged, cbind(did.x[!is.na(did.y)], did.y[!is.na(did.y)])) ## Seems like they differ on the date, which are maybe one day off between the two datasets. I suggest that we keep the swetrau did

## Keep SweTrau did
merged$did <- merged$did.x
merged$did.x <- NULL
## Check how many additional matches we got
sum(merged$origin.x == "kval" | merged$origin.y == "kval", na.rm = TRUE ) ## 600, so we matched four additional patients, leaving 5 unmatched

## This is probably as good as it gets, and we'll have to live with
## having 5 unmatched patients between kval and SweTrau

#######
# OK clean again, since we merged.
#######
col.names.x <- names(select(merged,ends_with(".x")))
col.names <- gsub('.x', '', col.names.x)
col.names.y <- paste(col.names, ".y", sep="")

merged2 <- merged
for (x in 1:length(col.names.x)){
  merged2[is.na(merged2[,col.names.x[x]]) == TRUE, col.names.x[x]] <- 
    merged2[is.na(merged2[,col.names.x[x]]) == TRUE,col.names.y[x]]
}

### Removes excess collumnes (.y) since .y is inserted into .x wherever .x was empty.
merged2[,col.names.y] <- NULL
#######

## change .x to original names
colnames(merged2)<-gsub(".x","",colnames(merged2))

merged <- merged2
########

## Now try matching fmp and problem
fmp <- datasets$fmp
problem <- datasets$problem

## Combine id variables
fmp$id <- with(fmp, paste(Personnummer, Reservnummer, Ankomst_te))
problem$id <- with(problem, paste(Personnummer, Reservnummer, Ankomst_te))
identical(fmp$id, problem$id) ## These vectors are exactly the same, meaning that we can combine these datasets using cbind

## Combine fmp and problem
fmp.problem <- as.data.frame(cbind(fmp, problem$Problemomrade_.FMP))

##
# "Identical" includes same order? 
# Just change column name back to Problemomrade_.FMP
##
fmp.problem$Problemomrade_.FMP <- fmp.problem$`problem$Problemomrade_.FMP`
fmp.problem$`problem$Problemomrade_.FMP` <- NULL
####

## Create id, arrival and did variables
fmp.problem$id <- fmp$Personnummer
fmp.problem$id[is.na(fmp.problem$id)] <- fmp.problem$Reservnummer[is.na(fmp.problem$Personnummer)]
sum(is.na(fmp.problem$id)) ## No missing id values
fmp.problem$arrival <- as.Date(strptime(fmp.problem$Ankomst_te, format = "%Y%m%d %H:%M"))
fmp.problem$did <- paste(fmp.problem$id, fmp.problem$arrival)

## Check for duplicates
sum(duplicated(fmp.problem$did)) ## 22 duplicates

## Check each of the 22 duplicates
## duplicated.dids <- fmp$did[duplicated(fmp.problem$did)]
## remove <- as.numeric(unlist(lapply(duplicated.dids, function(did) {
##     rows <- fmp[fmp$did == did, ]
##     print(rows)
##     message("Remove? (press y and then Enter. Otherwise press just Enter)")
##     if (readLines(n = 1) == "y") {
##         return(row.names(rows)[2:nrow(rows)])
##     } else {
##         return (NA)
##     }
## })))

## Not all of them are obvious duplicates, for example there are cases
## with different arrival time and even sex, where they appear to be
## different, but the personal numbers are the same. I suggest we
## remove all these duplicates, and only keep the first match, as we
## can't match them to SweTrau.

## The vector was obtained with dput(remove)
remove <- c(3491, 4234, 4354, 6124, 7062, 9182, 9683, 9946, 9792, 9793, 
9792, 9793, 9794, 9928, 9940, 9683, 9946, 10097, 10700, 11230, 
11360, 11460, 11360, 11460, 11586, 11613, 11615, 11686)  
fmp.problem <- fmp.problem[-remove, ]

## Check that all duplicates are removed
sum(duplicated(fmp.problem$did)) ## 0, all duplicates removed

## Now let's merge SweTrau with this fmp.problem using did
fmp.problem$origin <- 1:nrow(fmp.problem)
merged.swetrau.fmp.problem <- merge(merged, fmp.problem, by = "did", all.x = TRUE)

## Check how many from swetrau that were not matched in fmp.problem
sum(is.na(merged.swetrau.fmp.problem$origin.y)) ## 264 cases from SweTrau are not in fmp.problem
not.in.fmp.problem <- merged[!(merged$did %in% fmp.problem$did), c("did", "did", "id", "PersonIdentity", "TempIdentity", "Gender", "DateTime_ArrivalAtHospital", "arrival")]
nrow(not.in.fmp.problem)
## to.keep <- apply(not.in.fmp.problem, 1, function(case) {
##     matching.cases <- fmp.problem[fmp.problem$id == case["id.x"], ]
##     if (nrow(matching.cases) > 0) {
##         print(case)
##         cat("\n")
##         print(matching.cases)
##         message("Keep? (Enter index and Enter or just Enter to discard)")
##         row.to.keep <- as.numeric(readLines(n = 1))
##         if (!is.na(row.to.keep)) {
##             return(matching.cases[row.to.keep, ])
##         } else {
##             return(NULL)
##         }
##     } else {
##         return(NULL)
##     }
## })

## In most cases where there is a match on personal number or
## temporary number but not on did the date is wrong, for example the
## year is entered in fmp as 2021 instead of 2020. In the majority of
## cases there are no matches in fmp however

## Change did in fmp to same as in swetrau for matching cases
## to.keep <- to.keep[!sapply(to.keep, is.null)]
## swetrau.rows <- as.numeric(names(to.keep))
## fmp.problem.rows <- as.numeric(sapply(to.keep, function(x) row.names(x)))

## Vectors were obtained with dput
fmp.problem.rows <- c(1423, 2825, 9073, 11624, 11583, 11749, 11880, 11881, 11913, 
11967)
swetrau.rows <- c(1571, 2854, 8962, 11387, 11388, 11578, 11670, 11671, 11691, 
11764)
fmp.problem[fmp.problem.rows, "did"] <- merged[swetrau.rows, "did"]

## Redo merge
merged.swetrau.fmp.problem <- merge(merged, fmp.problem, by = "did", all.x = TRUE)
sum(is.na(merged.swetrau.fmp.problem$origin.y)) ## 262 cases are still missing from swetrau in fmp.problem

merged <- merged.swetrau.fmp.problem

#######
# Combine hashed ID:S
#######

merged[is.na(merged[,"PersonIdentity"]) == TRUE, "PersonIdentity"] <- 
  merged[is.na(merged[,"PersonIdentity"]) == TRUE,"pat_personnummer"]

merged[is.na(merged[,"TempIdentity"]) == TRUE, "TempIdentity"] <- 
  merged[is.na(merged[,"TempIdentity"]) == TRUE,"pat_TempPersonnummer"]

#merged$pat_personnummer <- NULL     Should we remove?
#merged$pat_TempPersonnummer <- NULL

### Columns that should be translated into another column, cant find another way but manual? 

VK.colnames <- c("VK_hlr_thorak","VK_sap_less90","VK_iss_15_ej_iva",
                 "VK_gcs_less9_ej_intubTE","VK_mer_30min_DT","VK_mer_60min_interv")

kval.colnames <- c("Antal_thorakotomier_JN","sap_less_90_JN","ISS_moore_15_not_IVA",
                   "gcs_less_9_ej_intub_TE","DT_moore_30.minuter_ejTR3","akut_intervent_moore_60_min")

merged2 <- merged
for (x in 1:length(VK.colnames)){
  
  merged2[,VK.colnames[x]] <- with(merged2, ifelse(merged2[,kval.colnames[x]] == 1 & is.na(merged2[,VK.colnames[x]]) == TRUE, "Ja", merged2[,VK.colnames[x]]))
  merged2[,VK.colnames[x]] <- with(merged2, ifelse(merged2[,kval.colnames[x]] == 0 & is.na(merged2[,VK.colnames[x]]) == TRUE, "Nej", merged2[,VK.colnames[x]]))
}
# Now all kval.colnames should be in VK_ collumns instead, hence remove

merged2[,kval.colnames] <- NULL

### Convert problemområde to problemområde_.FMP
#merged2$Problemomrade_.FMP <- with(merged2, ifelse(is.na(merged2$Problemomrade_.FMP) == TRUE, `problemområde`, `Problemomrade_.FMP`))
merged2$Problemomrade_.FMP[is.na(merged2$Problemomrade_.FMP)] <- merged2$problemområde[is.na(merged2$Problemomrade_.FMP)]
merged2[,"problemområde"] <- NULL

### Need to fill in VK_avslutad to get them through create_ofi?
## Need to get some data for mortality?

merged2$VK_avslutad <- with(merged2, ifelse(is.na(merged2$bedomn_primar_granskning) == FALSE, "Ja", `VK_avslutad`))
### Dont want to remove "bedomn_primar_granskning" since its an easy way of identifying patients from kvaldata.

# To convert "riktlinjer" to corresponding VK_ column

torakotomi_list <- c("Torakotomi","Torakotomi","Thoracotomi\r\nMassiv tranfusion","Nöd thoracotomi\r\nMassiv tranfusion")
spleen_list <- c("Mjältskada","Lever skada\r\nMjältskada\r\nMassiv transfusion")
transfusion_list <- c("Thoracotomi\r\nMassiv tranfusion","Nöd thoracotomi\r\nMassiv tranfusion",
                      "Massiv transfusion\r\nEj nödthoracotomi  el. bäcken - stabil cirk vid ankomst","Massiv transfusion",
                      "Lever skada\r\nMjältskada\r\nMassiv transfusion","Nöd thoracotomi\r\nMassiv tranfusion")
liver_list <- c("Leverskada - ej extravasering","Leverskada","Lever skada\r\nMjältskada\r\nMassiv transfusion")
merged3 <- merged2

merged3$VK_hlr_thorak[merged3$Riktlinje %in% torakotomi_list] <- "Ja"
merged3$VK_mjaltskada[merged3$Riktlinje %in% spleen_list] <- "Ja"
merged3$VK_mass_transf[merged3$Riktlinje %in% transfusion_list] <- "Ja"
merged3$VK_leverskada[merged3$Riktlinje %in% liver_list] <- "Ja"

#### Change column class
merged3$dt_alarm_hosp <- as.numeric(merged3$dt_alarm_hosp)
merged3$dt_alarm_scene <- as.numeric(merged3$dt_alarm_scene)
merged3$dt_ed_emerg_proc <- as.numeric(merged3$dt_ed_emerg_proc)
merged3$dt_ed_first_ct <- as.numeric(merged3$dt_ed_first_ct)
merged3$ISS <- as.numeric(merged3$ISS)
merged3$NISS <- as.numeric(merged3$NISS)
merged3$pt_age_yrs <- as.numeric(merged3$pt_age_yrs)

merged3$DateTime_ArrivalAtHospital <- as.POSIXct(strptime(merged3$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
merged3$DateTime_LeaveScene  <- as.POSIXct(strptime(merged3$DateTime_LeaveScene, format = "%Y-%m-%d %H:%M"))
merged3$DateTime_of_Alarm  <- as.POSIXct(strptime(merged3$DateTime_of_Alarm, format = "%Y-%m-%d %H:%M"))
merged3$DateTime_Of_Trauma <- as.POSIXct(strptime(merged3$DateTime_Of_Trauma, format = "%Y-%m-%d %H:%M"))
merged3$DateTime_ArrivalAtScene <- as.POSIXct(strptime(merged3$DateTime_ArrivalAtScene, format = "%Y-%m-%d %H:%M"))

combined.datasets <- merged3
combined.datasets$did.y <- NULL
return(combined.datasets)
}