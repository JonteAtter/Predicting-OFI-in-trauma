
#### Functions data
# Temporary creatre.ofi2 file
# Merge datasets
# Clean audit filters
# Clean predictor variables
# Imputation
# Preprocess data

 ## Temp create_ofi2 function

create_ofi2 <- function(data,
                       quality.review.done = "VK_avslutad",
                       problem.area = "Problemomrade_.FMP",
                       mortality.review.done = "tra_DodsfallsanalysGenomford",
                       preventable.death = "Fr1.14") {
  ## Check arguments
  assertthat::assert_that(is.data.frame(data))
  variable.names <- c(quality.review.done = quality.review.done,
                      problem.area = problem.area,
                      mortality.review.done = mortality.review.done,
                      preventable.death = preventable.death)
  for (variable.name in variable.names) assertthat::assert_that(is.character(variable.name) & length(variable.name) == 1)
  assertthat::assert_that(all(variable.names %in% names(data)))
  
  ## Check for data shift, i.e. that input data has changed since
  ## this code was written
  ofi.data <- data[, variable.names]
  names(ofi.data) <- names(variable.names)
  ##### SIC: Changed reference to check_data_shit2
  
  ofi.data <- check_data_shift2(ofi.data)
  
  ## Create ofi variable
  
  ## The starting point is that either the quality review or
  ## mortality review process is done
  ofi <- !with(ofi.data, quality.review.done == "ja" | mortality.review.done == 1)
  ## If neither of them are done, then ofi should be NA
  ofi[ofi] <- NA
  ## If the problem area is not labelled as okay then there is an
  ## opportunity for improvement
  
  ############
  # New code # Why & (and) for problem.area and | (or) for preventable death?
  ############
  ofi[with(ofi.data, problem.area != "ok" & problem.area != "föredömligt handlagd" 
           & problem.area != "inget problemområde" & problem.area != "nej")] <- TRUE
  ## If the death is preventable or potentially prevantable then
  ## there is an opportunity for improvement
  ofi[with(ofi.data, preventable.death == "2" | preventable.death == "3")] <- TRUE
  ## If the preventability is unknown then it is unknown if there is
  ## an opportunity for improvement, unless there is an opportunity
  ## for improvement according to the quality review
  ofi[ofi.data$preventable.death == 999 & ofi == FALSE] <- NA
  
  ## Make ofi character
  ofi <- ifelse(ofi, "Yes", "No")
  
  ## Return ofi vector
  return (ofi)
}

check_data_shift2 <- function(ofi.data) {
  ## Check quality review done variable
  ofi.data$quality.review.done <- tolower(as.character(ofi.data$quality.review.done))
  levels.quality.review.done <- unique(ofi.data$quality.review.done)
  original.levels.quality.review.done <- c("ja", NA, "nej")
  if (!all(levels.quality.review.done %in% original.levels.quality.review.done))
    stop ("Levels in the quality review done variable have changed.")
  ## Check problem area variable
  ofi.data$problem.area <- tolower(as.character(ofi.data$problem.area))
  levels.problem.area <- unique(ofi.data$problem.area)
  
  ############
  # New code #
  ############
  
  original.levels.problem.area  <- c(NA,"ok","triage på akutmottagningen","vårdnivå","handläggning",
       "logistik/teknik","resurs","missad skada","lång tid till op",
       "kompetens brist","inget problemområde","föredömligt handlagd",
       "kommunikation","handläggning/logistik","traumakriterier/styrning",
       "lång tid till dt","triage på akm","tertiär survey","ok","nej",
       "dokumentation","bristande rutin","ok","neurokirurg","dokumetation")
  
  if (!all(levels.problem.area %in% original.levels.problem.area))
    stop ("Levels in the problem area variable have changed.")
  ## Check mortality review done variable
  ofi.data$mortality.review.done <- tolower(as.character(ofi.data$mortality.review.done))
  levels.mortality.review.done <- unique(ofi.data$mortality.review.done)
  original.levels.mortality.review.done <- c(NA, 2, 1)
  if (!all(levels.quality.review.done %in% original.levels.quality.review.done))
    stop ("Levels in the mortality review done variable have changed.")
  ## Check preventable death variable
  levels.preventable.death <- unique(ofi.data$preventable.death)
  original.levels.preventable.death <- c(NA, "1", "3", "2", "999")
  if (!all(levels.preventable.death %in% original.levels.preventable.death))
    stop ("Levels in the preventable death variable have changed.")
  ## Return data
  return (ofi.data)
}


##############
# Merge Data #
##############

merge_data2 <- function(datasets) {
  
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
  
  ### Final cleaning
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
  #######
  # Combine ID:S
  #######
  
  merged[is.na(merged[,"PersonIdentity"]) == TRUE, "PersonIdentity"] <- 
    merged[is.na(merged[,"PersonIdentity"]) == TRUE,"pat_personnummer"]
  
  merged[is.na(merged[,"TempIdentity"]) == TRUE, "TempIdentity"] <- 
    merged[is.na(merged[,"TempIdentity"]) == TRUE,"pat_TempPersonnummer"]
  
  ## Should we remove?
  merged$pat_personnummer <- NULL
  merged$pat_TempPersonnummer <- NULL
  
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
  
  ##### Errors found later
  combined.datasets[combined.datasets$tra_id == "92386","tra_DodsfallsanalysGenomford"] <- 1
  
  
  return(combined.datasets)
}

##############################################

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
dataset <- as.data.frame(dataset.clean.af)
dataset$intub <- with(dataset, ifelse(`pre_intubated` == 1 & is.na(dataset$pre_intubated) == FALSE, 3, `ed_intubated`))
  
  ## Create vectors for variable, separated by type and use - Make sure "na.values.list" is up to date
  ## Make sure you change cat values in imputation
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value","hosp_los_days","iva_dagar_n") 
  
  ## categorical variables
  cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")
  
  ## Variables used for sorting
  time.id.var <- c("arrival", "id")                                            
  variables <- c(cont.var, cat.var, time.id.var)
  model.variables <- c(cont.var, cat.var)
  dpc <- dataset[variables]
  
  ## A list that governs values in what variables that should be converted to NA
  na.values.list <- list(ed_gcs_sum = c(99, 999),
                         ed_rr_value = c(99), ## The manual states that RR should not be over 70
                         ISS = c(0),
                         res_survival = c(999)) 
  
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
# Imputation # 
##############


imputation <- function(dataset) {
  
  ## continuous variables
  cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value","hosp_los_days","iva_dagar_n") 
  
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

###############
## Bootstrap ##  
###############
  
bootstrap <- function(data, index) {
    random.datasets <- data[index,]
    
    ##if (!is.null(index)) {
    ##  random.datasets <- data[index,]. #### Fungerade ej direkt, och tog bort i ett försök att minimera felkällor. 
    ##}
    
    ## Create OFI collumn
    #
    # CHANGE HERE WHEN ROFI IS DONE!
    #
    # random.datasets$ofi <- rofi::create_ofi(random.datasets)
    random.datasets$ofi <- create_ofi2(random.datasets)
    ## Clean previous audit filters
    random.clean.af <- clean_audit_filters(random.datasets)
    
    ## Only those who are done with VK / Dodsfallskonferans
    random.clean.af <- subset(random.clean.af, random.clean.af$tra_DodsfallsanalysGenomford == 1 | random.clean.af$VK_avslutad == "Yes" )

    ## Separate and store cases without known outcome
    missing.outcome <- is.na(random.clean.af$ofi)
    random.clean.af <- random.clean.af[!missing.outcome, ]
    
    ## Clean predictors (Correct NA classification and predictor choice)
    clean.random.dataset <- clean_predictor(random.clean.af)
    
    ## clean.random.dataset <-clean.random.dataset[order(clean.random.dataset$arrival), ]
    ## tv <- c(1:round(nrow(clean.random.dataset)*0.8, digits = 0))
    
    ## Imputation
    imputed.dataset <- imputation(clean.random.dataset)
    
    ### Remove predicts without variance (imputed without missing data)
    variance.data <- Filter(function(x)(length(unique(x))>1), imputed.dataset)
    preprocessed.data <- preprocess_data(variance.data)
    
    results.lr <- run_ml(dataset = preprocessed.data,
                         method = 'glmnet',
                         outcome_colname = "ofi",
                         perf_metric_name = "AUC",
                         kfold = 5,
                         cv_times = 5,
                         training_frac = 0.8,
                         seed = 2019)
    
  results.forest <- run_ml(dataset = preprocessed.data,
                             method = 'rf',
                             outcome_colname = "ofi",
                             perf_metric_name = "AUC",
                             kfold = 5,
                             cv_times = 5,
                             training_frac = 0.8,
                             seed = 2019)
    
    results.boost <- run_ml(dataset = preprocessed.data,
                            method = 'xgbTree',
                            outcome_colname = "ofi",
                            perf_metric_name = "AUC",
                            kfold = 5,
                            cv_times = 5,
                            training_frac = 0.8,
                            seed = 2019) 
    
    test.data <- results.lr$test_data
    labels <- test.data$ofi
    test.data$ofi <- NULL
    
    ## log reg
    final.model.lr <- results.lr$trained_model$finalModel
    prediction.lr <- predict(final.model.lr, newx = as.matrix(test.data), s = 0.1, type = "response")
    pred.lr <- ROCR::prediction(prediction.lr, labels)
    auc.lr <- ROCR::performance(pred.lr, measure = "auc")@y.values
    auc.lr <- unlist(auc.lr)
    accuracy.lr <- ROCR::performance(pred.lr, measure = "acc")@y.values
    accuracy.lr <- mean(unlist(accuracy.lr))
##  ici.lr <- ici(prediction.lr, labels) ## Fungerar ej direkt men ej hunnit felsöka 100%
    
## Randrom forest
    ## final.model.forest <- results.forest$trained_model$finalModel
    ## pred.rf <- predict(final.model.forest, newdata = as.matrix(results.forest$test_data), predict.all = TRUE)
    ## pred.forest <- pred.rf$aggregate
    ## prediction.forest3 <- ROCR::prediction(as.numeric(pred.forest), as.numeric(labels))
    ## auc.forest <- unlist(ROCR::performance(prediction.forest3, measure = "auc")@y.values)
    ## accuracy.forest <- ROCR::performance(prediction.forest3, measure = "acc")@y.values
    ## accuracy.forest <- mean(unlist(accuracy.forest))
    
### SVM
    ## final.model.vector.machine <- results.vector.machine$trained_model$finalModel
    ## prediction.vector.machine <- predict(final.model.vector.machine, newx = as.matrix(test.data), s = 0.1, type = "response")
    ## pred.vector.machine <- ROCR::prediction(prediction.vector.machine, labels)
    ## auc.vector.machine <- ROCR::performance(pred.vector.machine, measure = "auc")@y.values
    ## accuracy.vector.machine <- ROCR::performance(pred.vector.machine, measure = "acc")@y.values
    
    ## xgboost
    final.model.boost <- results.boost$trained_model$finalModel
    prediction.boost <- xgboost:::predict.xgb.Booster(final.model.boost, newdata = as.matrix(test.data), type = "response")
    pred.boost <- ROCR::prediction(prediction.boost, labels)
    auc.boost <- unlist(ROCR::performance(pred.boost, measure = "auc")@y.values)
    auc.boost <- 1-as.numeric(auc.boost)
    accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values
    accuracy.boost <- 1-mean(unlist(accuracy.boost))
    
    ## Result summary
    
    ## Delta AUC 
    #lr.rf.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.forest))
    lr.boost.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.boost))
    #rf.boost.auc <- abs(as.numeric(auc.boost) - as.numeric(auc.forest))
    
    auc <- c(lr.auc = auc.lr,
             boost.auc = auc.boost,
             delta.lr.boost.auc = lr.boost.auc)
    
##    auc <- c(lr.auc = auc.lr,
##             rf.auc = auc.forest,
##             boost.auc = auc.boost,
##             delta.lr.rf.auc = lr.rf.auc,
##             delta.lr.boost.auc = lr.boost.auc,
##             delta.rf.boost.auc = rf.boost.auc)
    
    ## Delta accuracy
##    lr.rf.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.forest))
    lr.boost.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.boost))
##    rf.boost.acc <- abs(as.numeric(accuracy.boost) - as.numeric(accuracy.forest))
    
    accuracy <- c(lr.accuracy = accuracy.lr,
                  boost.accuracy = accuracy.boost,
                  delta.lr.boost.accuracy = lr.boost.acc)
    
##    accuracy <- c(lr.accuracy = accuracy.lr,
##                  rf.accuracy = accuracy.forest,
##                  boost.accuracy = accuracy.boost,
##                  delta.lr.rf.accuracy = lr.rf.acc,
##                  delta.lr.boost.accuracy = lr.boost.acc,
##                  delta.rf.boost.accuracy = rf.boost.acc)
    
    boot.result <- c(accuracy,auc)
    
    ## if (!boot) {
    ##  boot.result <- list(boot.result,
    ##                     lr.model = results.lr,
    ##                    rf.model = results.forest,
    ##                   boost.model = results.boost)
    ## }
    
    if (!dir.exists("out"))
        dir.create("out")
    filename <- paste0("out/boot.result.", gsub(".", "", as.numeric(Sys.time()), fixed = TRUE), ".Rds")
    saveRDS(boot.result, filename)
    message("Bootstrap analysis completed and results saved")
    return(boot.result)
}

