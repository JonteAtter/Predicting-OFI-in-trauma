## setwd("/Users/jonatanattergrim/Documents/GitHub/")
## data.dir <- "./scrambled-data/"

## Install packages
install.packages('bit64')
install.packages("devtools")
## install.packages("naniar")
devtools::install_github("SchlossLab/mikropml")
install.packages('dplyr')

## Load packages
library(stringr)
library(mikropml)
## library(naniar)
library(dplyr)
library(labelled)
library(tableone)
library(DBI)
library(RMariaDB)
library(dotenv)
library(keyring)

## Setup reading from the database
dataset.names <- setNames(nm = c("swetrau", "fmp", "atgarder", "problem"))
db.name <- "opportunities_for_improvement"
scrambled <- TRUE ## Set to FALSE if you're working with the real data
if (scrambled) dataset.names <- setNames(paste0(dataset.names, "_scrambled"), nm = dataset.names)
if (scrambled) db.name <- paste0(db.name, "_scrambled")

## Setup keyring
username <- "jonatana" ## Replace scrambled with your actual username
## Only do this the first time
##keyring::key_set(service = db.name,
##                 username = username) 

## Connect to database
conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                       user = username,
                       password = keyring::key_get(db.name, username),
                       db = db.name)

## Read data
datasets <- lapply(dataset.names, function(dataset.name) dbReadTable(conn = conn, name = dataset.name))
attach(datasets)

## If working with the real data we need to create the id variable for
## matching by first reformatting the date and time of arrival (also
## necessary for scrambled data) to the trauma unit so that it is the
## same in all datasets, and then create the id variable by merging
## the date and time variable with personal identifiers
swetrau$arrival <- as.POSIXct(strptime(swetrau$DateTime_ArrivalAtHospital, format = "%Y-%m-%d %H:%M"))
fmp$arrival <- as.POSIXct(strptime(fmp$Ankomst_te, format = "%Y%m%d %H:%M"))
problem$arrival <- as.POSIXct(strptime(problem$Ankomst_te, format = "%Y%m%d %H:%M"))
if (!scrambled) {
    swetrau$id <- paste(swetrau$arrival, swetrau$PersonIdentity, swetrau$TempIdentity)
    fmp$id <- paste(fmp$arrival, fmp$Personnummer, fmp$Reservnummer)
    problem$id <- paste(problem$arrival, problem$Personnummer, problem$Reservnummer)
}

## Combine datasets
combined.datasets <- merge(fmp, problem, by = "id", all.x = TRUE)
combined.datasets <- merge(combined.datasets, swetrau, by = "id", all.x = TRUE)

## Column with yes/no and 1/0 for problems
levels.Problemomrade_.FMP <- sort(unique(combined.datasets$Problemomrade_.FMP))
original.levels.Problemomrade_.FMP <- sort(c(NA, "OK",
                                             "Triage på akutmottagningen",
                                             "Resurs", "Lång tid till op",
                                             "Lång tid till DT", "Vårdnivå",
                                             "Traumakriterier/styrning",
                                             "Missad skada", "Kommunikation",
                                             "Neurokirurg",
                                             "Föredömligt handlagd",
                                             "Logistik/teknik", "Ok",
                                             "Dokumentation", "Dokumetation",
                                             "bristande rutin", "ok",
                                             "Handläggning", "kompetens brist",
                                             "Tertiär survey"))
if (!identical(levels.Problemomrade_.FMP, original.levels.Problemomrade_.FMP))
    stop ("Levels in Problemomrade._FMP have changed.")
combined.datasets$probYN <- with(combined.datasets, ifelse(
  `Problemomrade_.FMP` == "ok" |
  `Problemomrade_.FMP` == "OK" |
  `Problemomrade_.FMP` == "Ok" |
  `Problemomrade_.FMP` == "Föredömligt handlagd",
  "Yes", "No"))
combined.datasets$prob10 <- with(combined.datasets, ifelse(`probYN` == "Yes", 1, 0))

## Clean variable indicating if the the care quality process has been completed
levels.VK_avslutad <- sort(unique(combined.datasets$VK_avslutad))
original.levels.VK_avslutad <- sort(c("Ja", NA, "ja", "Nej"))
if (!identical(levels.VK_avslutad, original.levels.VK_avslutad))
    stop ("Levels in VK_avslutad have changed.")
combined.datasets$quality.process.done <- with(combined.datasets,
                                               ifelse(VK_avslutad == "Ja", "Yes",
                                               ifelse(VK_avslutad == "ja", "Yes",
                                               ifelse(VK_avslutad == "Nej", "No", NA))))

## Create outcome, which is either Yes or No:
##
## Yes: The case has been reviewed in a meeting and the consensus was
## that there were opportunities for improvement
## 
## No: The consensus was that there were no opportunities for
## improvement, or the nurses in the initial review did not send the
## case for review because everything was okay.
combined.datasets$ofi <- with(combined.datasets,
                              ifelse(quality.process.done == "Yes" & probYN == "Yes", "Yes",
                              ifelse(quality.process.done == "Yes" & probYN == "No", "No", NA)))
combined.datasets$ofi[with(combined.datasets, quality.process.done == "Yes" & is.na(probYN))] <- "No"

## Separate and store cases without known outcome
missing.outcome <- is.na(combined.datasets$ofi)
n.missing.outcome <- sum(missing.outcome)
data.prob <- combined.datasets[!missing.outcome, ]

## Create new variable for intubation status: 1: Intubated in hospital: 2: Not intubated: 3 Intubated prehospital
data.prob$intub <- with(data.prob, ifelse(`pre_intubated` == 1 & is.na(data.prob$pre_intubated) == FALSE, 3, `ed_intubated`))

## Create vectors for variable, separated by type and use - Make sure "na.values.list" is up to date

## continuous variables
cont.var <- c("ed_gcs_sum", "ed_sbp_value", "ISS", "dt_ed_first_ct", "dt_ed_emerg_proc", "pt_age_yrs", "ed_rr_value") 

## categorical variables
cat.var <- c("ofi", "res_survival", "intub", "host_care_level", "Gender")

## Variables used for sorting
time.id.var <- c("arrival", "id")                                              
variables <- c(cont.var, cat.var, time.id.var)
model.variables <- c(cont.var, cat.var)
dpc <- data.prob[variables]

## A list that governs values in what variables that should be converted to NA
na.values.list <- list(ed_gcs_sum = c(99, 999),
                       ed_rr_value_ = c(99), ## The manual states that RR should not be over 70
                       ISS = c(0, 1, 2)) 

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

# Create new dataframe - missing.indicator.variables - Containing true/false if a value is imputet
missing.indicator.variables <- as.data.frame(lapply(dpc, function(data) is.na(data)))
missing.indicator.variables[, c("probYN", "Ankomst_te", "id")] <- NULL
names(missing.indicator.variables) <- paste0("missing_", names(missing.indicator.variables))

## Convert categorical values to factors
for (variable.name in cat.var) {
   dpc[, variable.name] <- as.factor(dpc[, variable.name])
}

## Convert continuous variables to numeric
for (variable.name in cont.var) {
    dpc[, variable.name] <- as.numeric(dpc[, variable.name])
}

## Imputation 
dpc.imputed <- as.data.frame(lapply(dpc, function(data) {
    new.data <- data
    if (is.factor(data))
        new.data <- as.character(data) ## change to character to be able to identify/separate?
    if (is.numeric(data))
        new.data[is.na(data)] <- mean(new.data, na.rm = TRUE) ## continuous  - mean
    if (is.character(new.data))                                                 
        new.data[is.na(data)] <- tail(names(sort(table(new.data))), 1) ## categorical - most common
    if (is.factor(data))
        new.data <- as.factor(new.data)
    return (new.data)
}))

## Sort data according to time and create 80% training vector for training set
## selection
dpc.imputed <-dpc.imputed[order(dpc.imputed$arrival), ]
tv <- c(1:round(nrow(dpc.imputed)*0.8, digits = 0))

## Create new dataframe - dataset - combining imputed data and
## corresponding variables if the data is imputed true/false.
dataset <- cbind(dpc.imputed[model.variables], missing.indicator.variables)

results <- run_ml(dataset = dataset,
                  method = 'glmnet',
                  outcome_colname = "ofi",
                  kfold = 5,
                  cv_times = 5,
                  training_frac = tv,
                  seed = 2019)

results.forest <- run_ml(dataset = dataset,
                  method = 'rf',
                  outcome_colname = "ofi",
                  kfold = 5,
                  cv_times = 5,
                  training_frac = 0.8,
                  seed = 2019)

performance.LR <- as.list(results$performance)
performance.RF <- as.list(results.forest$performance)

performance <- do.call(rbind, Map(data.frame, "Logistic regression"=performance.LR, "Random forest"=performance.RF))

##----------------------Table of Characteristics---------------------

## tried using factorVars = cat.var and remove the categorical values from model.variables but could not display the categorical values?
## Works as intended for me, see below

dpc$intub <- factor(
    dpc$intub,
    levels = c(1, 2, 3),
    labels = c("Inhospital", "Not intubated", "Prehospital"))
dpc$host_care_level <- factor(
    dpc$host_care_level,
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Emergency department",
               "General ward",
               "Surgical ward",
               "Specialist ward/Intermediate ward",
               "Intensive care unit"))
dpc$Gender <- factor(
    dpc$Gender,
    levels = c("K", "M"),
    labels = c("Female", "Male"))
dpc$res_survival <- factor(
    dpc$res_survival,
    levels = c(1, 2),     ########## OK, det blir fel med deceased - som blir bara NA.
    labels = c("Yes", "No"))
dpc$ofi <- factor(
    dpc$ofi,
    levels = c("Yes", "No"),
    labels = c("Opportunity for improvement", "No opportunity for improvement"))
var_label(dpc) <- list(
    ofi = "Opportunity for improvement",
    ed_gcs_sum = "GCS",
    ed_sbp_value = "Systolic Blood Pressure",
    dt_ed_first_ct = "Time to first CT",
    dt_ed_emerg_proc = "Time to definitive treatment",
    intub = "Intubated",
    host_care_level = "Highest level of care",
    pt_age_yrs = "Age",
    ed_rr_value = "Respiratory rate",
    res_survival = "Dead at 30 days") ##Requires library(labelled)

## You may want to reorder your table variables so that it's easier to read. I suggest demographics first, injury details, vital signs, outcomes, or something like that.

vars <- model.variables[-grep("ofi", model.variables)]
table1 <- list()
table1$overall <- CreateTableOne(vars = vars, data = dpc, test = FALSE)
table1$stratified <- CreateTableOne(vars = vars, strata = "ofi", data = dpc, test = FALSE)
table1 <- lapply(table1, print, showAllLevels = TRUE, printToggle = FALSE, varLabels = TRUE)
table1.combined <- do.call(cbind, table1)
table1.combined <- cbind(rownames(table1.combined), table1.combined)
rownames(table1.combined) <- NULL
table1.combined <- as.data.frame(table1.combined)
table1.combined[, grep("level", colnames(table1.combined))[2]] <- NULL # Remove duplicate level column
colnames(table1.combined)[1] <- "Characteristic"
colnames(table1.combined)[colnames(table1.combined) == "level"] <- "Level"
knitr::kable(print(table1.combined,
                   caption = "Table 1. Sample Characteristics",
                   showAllLevels = TRUE,
                   printToggle = FALSE,
                   varLabels = TRUE))
