
## Load packages
packages <- c("gmish", "rofi", "stringr", "mikropml", "dplyr", "labelled", "DBI", "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", "tableone", "table1", "dplyr", "kableExtra", "lattice", "caret")
for (package in packages) {
    if (!require(package, quietly = TRUE)) {
        install.packages(package)
    }
    libraray(package)
}

dotenv::load_dot_env

## Source functions
source("../functions2.R")

## Setup reading from the database
dataset.names <- setNames(nm = c("swetrau", "fmp", "atgarder", "problem", "kvalgranskning2014.2017"))
db.name <- "opportunities_for_improvement"
scrambled <- FALSE ## Set to FALSE if you're working with the real data
if (scrambled) dataset.names <- setNames(paste0(dataset.names, "_scrambled"), nm = dataset.names)
if (scrambled) db.name <- paste0(db.name, "_scrambled")

### Connect to server
conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                       user = 'jonatana',
                       password = Sys.getenv("DB_PASSWORD"),
                       db = "opportunities_for_improvement")


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

## Create OFI collumn
combined.datasets[,"Fr1-14"] <- combined.datasets$Fr1.14
combined.datasets$ofi <- create_ofi(combined.datasets)

### Cleaning data

### Clean previous audit filters
dataset.clean.af <- clean_audit_filters(combined.datasets)

## Only those who are done with VK / Dodsfallskonferans

dataset.clean.af <- subset(dataset.clean.af, dataset.clean.af$tra_DodsfallsanalysGenomford == 1 | dataset.clean.af$VK_avslutad == "Yes" )

## Separate and store cases without known outcome
missing.outcome <- is.na(dataset.clean.af$ofi)
n.missing.outcome <- sum(missing.outcome)
dataset.clean.af <- dataset.clean.af[!missing.outcome, ]

## Clean predictors (Correct NA classification and predictor choice)
clean.dataset <- clean_predictor(dataset.clean.af)

## Imputation
imputed.dataset <- imputation(clean.dataset)

## Sort data according to time and create 80% training vector for training set
## selection
clean.dataset <-clean.dataset[order(clean.dataset$arrival), ]
tv <- c(1:round(nrow(clean.dataset)*0.8, digits = 0))


## Result calculations ##
if (file.exists("out/boot.results.Rds"))
    file.remove("out/boot.results.Rds")
boot.results <- boot(data = combined.datasets, statistic = Bootstrap, R = 6)
saveRDS("out/boot.results.Rds")
if (file.exists("out/boot.results.Rds"))
    message("All bootstraps completed and results saved to disk")
# test for result extraction. 
#accuricy.ci <- boot.ci(boot.results, type = "norm", index = 1)
#ci.auc <- boot.ci(boot.results, type = "norm", index = 2)
