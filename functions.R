install.packages('bit64')
install.packages("devtools")
install.packages("naniar")
devtools::install_github("SchlossLab/mikropml")
install.packages('dplyr')
library(stringr)
library(mikropml)
library(naniar)
library(dplyr)

data.dir <- "./scrambled-data/"
dataset.names <- setNames(nm = c("swetrau", "fmp", "atgarder", "problem"))
datasets <- lapply(dataset.names, function(dataset.name) rio::import(paste0(data.dir, dataset.name, "-scrambled.csv")))
attach(datasets)

# Skapar en column med Ja/Nej och en med 1/0 för utfall.
problem$ProbJN <- with(problem, ifelse(
  `Problemomrade_ FMP` == "ok" |
  `Problemomrade_ FMP` == "OK" |
  `Problemomrade_ FMP` == "Ok" |
  `Problemomrade_ FMP` == "Föredömligt handlagd",
  'Ja', 'Nej'))
problem$prob10 <- with(problem, ifelse(`ProbJN` == "Ja", 1, 0))

## tar bort patienter utan känt utfall
## You often need to report these numbers later, so it's a good idea to store them now.
missing.outcome <- is.na(problem[, "Problemomrade_ FMP"])
n.missing.outcome <- sum(missing.outcome)
dfc <- problem[!missing.outcome, ]

#slår ihop till en ny dataframe 
data.prob <- merge(dfc, swetrau, by="id")

#skapa vektororer med önskade variabler

#kontinuerliga variabler
v1 <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct","ed_rr_value")

#kvalitativa variabler
v2 <- c("ProbJN","pt_asa_preinjury","Deceased")
v3 <- c("Ankomst_te","id")

v4 <- c(v1,v2,v3)

dpc <- data.prob[v4]

#Byt ut 999 och 99 mot NA, OBS at id och ev ankomst_ted inehåller dessa utan att vara fel? (Notera att "naniar" kan göra detta betydligt mer avancerat)

## Note that the code below will convert all 999 and 99 to NA, regardless of variable. This means that an sbp of 99 will also be converted to NA, which is probably not what you want.
## dpc <- dpc %>% dplyr::na_if(999) 
## dpc <- dpc %>% dplyr::na_if(99)

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

## Then it's probably a good idea to create a list that governs what
## values in what variables that should be converted to NA
na.values.list <- list(ed_gcs_sum = c(99, 999),
                       pt_asa_preinjury = c(999),
                       ISS = c(0, 1)) # Check these values

## Convert
dpc[] <- lapply(names(dpc), function(variable.name) {
    data <- dpc[, variable.name]
    na.values <- na.values.list[[variable.name]]
    if (!is.null(na.values))
        data <- convert_to_na(data, na.values)
    return (data)
})

# fixa kollumn,"imput" för imputet 1/0 och lägg till imput i vektor för kategorisk variabler (v2)

## Note that this will only create one indicator variable. You need
## one missing indicator variable per variable that you impute

## dpc$imput <-0
## for(i in 1:ncol(dpc)){
##   dpc$imput <- with(dpc, ifelse(is.na(dpc[i]) == TRUE | dpc$imput == 1 , 1, 0))
## }
##
## v2 <- c(v2, "imput")

missing.indicator.variables <- as.data.frame(lapply(dpc, function(data) is.na(data)))
missing.indicator.variables[, c("ProbJN", "Ankomst_te", "id")] <- NULL
names(missing.indicator.variables) <- paste0("missing_", names(missing.indicator.variables))

 #göra om kategoriska variabler till factorer, främst för numreriska likt ASA

## I suggest you use names instead of numeric indices to make the code easier to read.

for (variable.name in v2) {
   dpc[, variable.name] <- as.factor(dpc[, variable.name])
}

#Imputation 
# Kontinuerliga - mean

## For loops works but a more R-ish way to do things is to use the apply family of functions

## for(i in 1:ncol(dpc[v1])){
##   dpc[v1][is.na(dpc[v1][,i]), i] <- mean(dpc[v1][,i], na.rm = TRUE)
## }
# Kategoriska - most common
## for(i in 1:ncol(dpc[v2])){
##  dpc[v2][is.na(dpc[v2][,i]), i] <- tail(names(sort(table(dpc[v2][,i]))), 1)
## }

dpc.imputed <- as.data.frame(lapply(dpc, function(data) {
    new.data <- data
    if (is.factor(data))
        new.data <- as.character(data)
    if (is.numeric(data))
        new.data[is.na(data)] <- mean(new.data, na.rm = TRUE)
    if (is.character(new.data))
        new.data[is.na(data)] <- tail(names(sort(table(new.data))), 1)
    if (is.factor(data))
        new.data <- as.factor(new.data)
    return (new.data)
}))

#Sortera efter tid.och skapa vektor för 80% träning

dpc.imputed <-dpc.imputed[order(dpc.imputed$Ankomst_te),]

tv <- c(1:round(nrow(dpc.imputed)*0.8, digits = 0))

#simpelt första försök med mikropml

dataset <- cbind(dpc.imputed[c(v1,v2)], missing.indicator.variables)

results <- run_ml(dataset = dataset,
                  method = "glmnet",
                  outcome_colname = "ProbJN",
                  kfold = 2,
                  cv_times = 5,
                  training_frac = tv,
                  seed = 2019)

