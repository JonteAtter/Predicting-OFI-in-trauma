## Load packages
packages <- c("Gmisc", "stringr", "mikropml", "dplyr", "labelled", "DBI", "RMariaDB", "dotenv", "keyring", "remotes", "boot", "DiagrammeR", "tableone", "table1", "dplyr", "kableExtra", "lattice", "caret")
for (package in packages) library(package, character.only = TRUE)

## Source functions
source("functions2.R")

## Import data
datasets <- rofi::import_data()

## Merge data
combined.datasets <- rofi::merge_data(datasets)

## Create OFI collumn
combined.datasets$ofi <- rofi::create_ofi(combined.datasets)

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
boot.results <- boot(data = combined.datasets, statistic = bootstrap, R = 6)
saveRDS("out/boot.results.Rds")
if (file.exists("out/boot.results.Rds"))
    message("All bootstraps completed and results saved to disk")
# test for result extraction. 
#accuricy.ci <- boot.ci(boot.results, type = "norm", index = 1)
#ci.auc <- boot.ci(boot.results, type = "norm", index = 2)
