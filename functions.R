install.packages('bit64')
install.packages("devtools")
install.packages("naniar")
devtools::install_github("SchlossLab/mikropml")
install.packages('dplyr')

library(stringr)
library(mikropml)
library(naniar)
library(dplyr)
library(labelled)

setwd("/Users/jonatanattergrim/Documents/GitHub/")
data.dir <- "./scrambled-data/"
dataset.names <- setNames(nm = c("swetrau", "fmp", "atgarder", "problem"))
datasets <- lapply(dataset.names, function(dataset.name) rio::import(paste0(data.dir, dataset.name, "-scrambled.csv")))
attach(datasets)

# Column with yes/no and 1/0 for problems
problem$probYN <- with(problem, ifelse(
  `Problemomrade_ FMP` == "ok" |
  `Problemomrade_ FMP` == "OK" |
  `Problemomrade_ FMP` == "Ok" |
  `Problemomrade_ FMP` == "Föredömligt handlagd",
  'yes', 'no'))
problem$prob10 <- with(problem, ifelse(`probYN` == "yes", 1, 0))

## Separate and store cases without known outcome
missing.outcome <- is.na(problem[, "Problemomrade_ FMP"])
n.missing.outcome <- sum(missing.outcome)
dfc <- problem[!missing.outcome, ]

## Combine swetrau and problem datasets
data.prob <- merge(dfc, swetrau, by="id")

## current audit filters: 
## injury scene time more than 20 minutes: "dt_alarm_hosp" ???? Eller: "DateTime_LeaveScene"-"DateTime_ArrivalAtScene" 
## systolic blood pressure less than 90: "ed_sbp_value"
## Glasgow coma scale less than 9 and not intubated: "ed_gcs_sum" + "ed_intubated". Måste "pre_intubated" vara med?
## injury severity score more than 15 but not admitted to the intensive care unit: "ISS" + "host_care_level"
## time to acute intervention more than 60 minutes: "dt_ed_emerg_proc"
## time to computed tomography more than 30 minutes: "dt_ed_first_ct"
## death within 30 days after trauma: "Deceased"

## Create vectors for variable, separated by type and use - Make sure "na.values.list" is up to date

## continuous variables
cont.var <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct","dt_ed_emerg_proc") 

## categorical variables
cat.var <- c("probYN","Deceased","ed_intubated","pre_intubated","host_care_level")

## Variables used for sorting
time.id.var <- c("Ankomst_te","id")                                              
variables <- c(cont.var,cat.var,time.id.var)

dpc <- data.prob[variables]

# Create combined value for intubation status

dpc$ed_intubated <- with(dpc, ifelse(`pre_intubated` == 1, 3, 0))

## A list that governs values in what variables that should be converted to NA
na.values.list <- list(ed_gcs_sum = c(99, 999),
                       ISS = c(0, 1, 2)
                       pre_intubated = c(0, 999))

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

#Imputation 
dpc.imputed <- as.data.frame(lapply(dpc, function(data) {
    new.data <- data
    if (is.factor(data))
        new.data <- as.character(data)              ### change to character to be able to identify/separate?
    if (is.numeric(data))
        new.data[is.na(data)] <- mean(new.data, na.rm = TRUE)                   ## continuous  - mean
    if (is.character(new.data))                                                 
        new.data[is.na(data)] <- tail(names(sort(table(new.data))), 1)          ## categorical - most common
    if (is.factor(data))
        new.data <- as.factor(new.data)
    return (new.data)
}))

#Sort data according to time and create 80% vector for training set selection

dpc.imputed <-dpc.imputed[order(dpc.imputed$Ankomst_te),]

tv <- c(1:round(nrow(dpc.imputed)*0.8, digits = 0))

#Create new dataframe - dataset - combining imputed data and corresponding variables if the data is imputed true/false.

dataset <- cbind(dpc.imputed[c(cont.var,cat.var)], missing.indicator.variables)

results <- run_ml(dataset = dataset,
                  method = "glmnet",
                  outcome_colname = "probYN",
                  kfold = 2,
                  cv_times = 5,
                  training_frac = tv,
                  seed = 2019)
##----------------------Table of Characteristics---------------------

myVars <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct","dt_ed_emerg_proc","probYN","Deceased","ed_intubated","pre_intubated","host_care_level")
#catVars <- c("probYN","Deceased","ed_intubated","pre_intubated","host_care_level")

## tried using factorVars = cat.var and remove the categorical values from myVars but could not display the categorical values?

dpc$ed_intubated <- factor(dpc$ed_intubated, levels = c(1,2,3), labels = c("Yes","No", "prehospital"))
dpc$host_care_level <- factor(dpc$host_care_level, levels = c(1,2,3,4,5), 
                              labels = c("Emergency department","General ward","surgical ward","specialist ward/Interimediare ward","intensive care unit"))
var_label(dpc) <- list(probYN = " Opertunity for improvement", ed_gcs_sum = "GCS", ed_sbp_value = "Systolic Blood Pressure", dt_ed_first_ct = "Time to first CT", dt_ed_emerg_proc = "Time to definitive treatment", ed_intubated = "Intubated", host_care_level = "Highest level of care") ##Requires library(labelled)


Table1 <- CreateTableOne(vars = myVars, strata = "probYN", data = dpc[, c(cat.var,cont.var)])


knitr::kable(print(Table1,
                   caption = "Table 1. Demographic, Physiological parameters, Injury Characteristics", showAllLevels = TRUE, printToggle = FALSE, varLabels = TRUE))

Table1

