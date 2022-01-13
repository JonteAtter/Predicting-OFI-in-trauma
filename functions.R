
install.packages('bit64')
install.packages("devtools")
install.packages("naniar")
devtools::install_github("SchlossLab/mikropml")
install.packages('dplyr')
library(stringr)
library(mikropml)
library(naniar)
library(dplyr)

data <- rio::import("~/Documents/GitHub/Data/swetrau-scrambled.csv")
fmp <- rio::import("~/Documents/GitHub/Data/fmp-scrambled.csv")
atg <- rio::import("~/Documents/GitHub/Data/atgarder-scrambled.csv")
prob <- rio::import("~/Documents/GitHub/Data/problem-scrambled.csv")

# Skapar en column med Ja/Nej och en med 1/0 för utfall.

prob$ProbJN <- with(prob, ifelse(
  `Problemomrade_ FMP` == "ok" |
  `Problemomrade_ FMP` == "OK" |
  `Problemomrade_ FMP` == "Ok" |
  `Problemomrade_ FMP` == "Föredömligt handlagd",
  'Ja', 'Nej'))
prob$prob10 <- with(prob, ifelse(`ProbJN` == "Ja", 1, 0))

#tar bort patienter utan känt utfall

dfc <- na.omit(prob)

#slår ihop till en ny dataframe 

data.prob <- merge(dfc, data, by="id")

#skapa vektororer med önskade variabler

#kontinuerliga variabler
v1 <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct")

#kvalitativa variabler
v2 <- c("ProbJN","Deceased","pt_asa_preinjury")
v3 <- c("Ankomst_te","id")

v4 <- c(v1,v2,v3)

dpc <- data.prob[v4]

#Byt ut 999 och 99 mot NA, OBS at id och ev ankomst_ted inehåller dessa utan att vara fel? (Notera att "naniar" kan göra detta betydligt mer avancerat)

dpc <- dpc %>% dplyr::na_if(999)
dpc <- dpc %>% dplyr::na_if(99)

#göra om kategoriska variabler till factorer, främst för numreriska likt ASA


for(i in 1:ncol(dpc[v2])){
  dpc[v2][,i] <- as.factor(dpc[v2][,i])
}


#Imputation av kvant med mean
#    LÄGG in i looperna = funktion för ny column?


# Kontinuerliga
for(i in 1:ncol(dpc[v1])){
  dpc[v1][is.na(dpc[v1][,i]), i] <- mean(dpc[v1][,i], na.rm = TRUE)
}
# Kategoriska
for(i in 1:ncol(dpc[v2])){
  dpc[v2][is.na(dpc[v2][,i]), i] <- tail(names(sort(table(dpc[v2][,i]))), 1)
}

#Sortera efter tid.och skapa vektor för 80% träning

dpc <-dpc[order(dpc$Ankomst_te),]

#tv <- c(1:round(nrow(dpc)*0.8, digits = 0))

#simpelt första försök med mikropml

results <- run_ml(dpc,
                  'glmnet',
                  outcome_colname = 'ProbJN',
                  seed = 2019)
