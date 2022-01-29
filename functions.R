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
library(tableone)

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
  "Yes", "No"))
problem$prob10 <- with(problem, ifelse(`probYN` == "Yes", 1, 0))

## Separate and store cases without known outcome
missing.outcome <- is.na(problem[, "Problemomrade_ FMP"])
n.missing.outcome <- sum(missing.outcome)
dfc <- problem[!missing.outcome, ]

## Combine swetrau and problem datasets
data.prob <- merge(dfc, swetrau, by="id")

## Create new variable for intubation status: 1: Intubated in hospital: 2: Not intubated: 3 Intubated prehospital

data.prob$intub <- with(data.prob, ifelse(`pre_intubated` == 1 & is.na(data.prob$pre_intubated) == FALSE, 3, `ed_intubated`))

## Create vectors for variable, separated by type and use - Make sure "na.values.list" is up to date

## continuous variables
cont.var <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct","dt_ed_emerg_proc","pt_age_yrs","ed_rr_value") 

## categorical variables
cat.var <- c("probYN","Deceased","intub","host_care_level","Gender")

## Variables used for sorting
time.id.var <- c("Ankomst_te","id")                                              
variables <- c(cont.var,cat.var,time.id.var)
model.variables <- c(cont.var,cat.var)
dpc <- data.prob[variables]

## A list that governs values in what variables that should be converted to NA
na.values.list <- list(ed_gcs_sum = c(99, 999),
                       ISS = c(0, 1, 2)) ##### RR = 99, could be real?

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

dataset <- cbind(dpc.imputed[model.variables], missing.indicator.variables)

results <- run_ml(dataset = dataset,
                  method = 'glmnet',
                  outcome_colname = "probYN",
                  kfold = 2,
                  cv_times = 5,
                  training_frac = tv,
                  seed = 2019)

results.forest <- run_ml(dataset = dataset,
                  method = 'rf',
                  outcome_colname = "probYN",
                  kfold = 2,
                  cv_times = 5,
                  training_frac = tv,
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
dpc$Deceased <- factor(
    dpc$Deceased,
    levels = c(TRUE, FALSE),
    labels = c("Yes", "No"))
dpc$probYN <- factor(
    dpc$probYN,
    levels = c("Yes", "No"),
    labels = c("Opportunity for improvement", "No opportunity for improvement"))
var_label(dpc) <- list(
    probYN = "Opportunity for improvement",
    ed_gcs_sum = "GCS",
    ed_sbp_value = "Systolic Blood Pressure",
    dt_ed_first_ct = "Time to first CT",
    dt_ed_emerg_proc = "Time to definitive treatment",
    intub = "Intubated",
    host_care_level = "Highest level of care",
    pt_age_yrs = "Age",
    ed_rr_value = "Respiratory rate",
    Deceased = "Dead at 30 days") ##Requires library(labelled)

## You may want to reorder your table variables so that it's easier to read. I suggest demographics first, injury details, vital signs, outcomes, or something like that.

vars <- model.variables[-grep("probYN", model.variables)]
table1 <- list()
table1$overall <- CreateTableOne(vars = vars, data = dpc, test = FALSE)
table1$stratified <- CreateTableOne(vars = vars, strata = "probYN", data = dpc, test = FALSE)
table1 <- lapply(table1, print, showAllLevels = TRUE, printToggle = FALSE, varLabels = TRUE)
table1.combined <- do.call(cbind, table1)
table1.combined <- cbind(rownames(table1.combined), table1.combined)
rownames(table1.combined) <- NULL
table1.combined <- as.data.frame(table1.combined)
table1.combined[, grep("level", colnames(table1.combined))[2]] <- NULL # Remove duplicate level column
colnames(table1.combined)[1] <- "Characteristic"
colnames(table1.combined)[colnames(table1.combined) == "level"] <- "Level"
knitr::kable(print(Table1,
                   caption = "Table 1. Sample Characteristics",
                   showAllLevels = TRUE,
                   printToggle = FALSE,
                   varLabels = TRUE))
