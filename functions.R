
install.packages('bit64')
install.packages("devtools")
devtools::install_github("SchlossLab/mikropml")
library(stringr)
library(mikropml)

Jdata <- rio::import("~/Documents/GitHub/dynamic-identification-ofi/Data/swetrau-scrambled.csv")
fmp <- rio::import("~/Documents/GitHub/dynamic-identification-ofi/Data/fmp-scrambled.csv")
atg <- rio::import("~/Documents/GitHub/dynamic-identification-ofi/Data/atgarder-scrambled.csv")
prob <- rio::import("~/Documents/GitHub/dynamic-identification-ofi/Data/problem-scrambled.csv")

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

#data.prob.c <-data.prob

#skapa vektororer med önskade variabler

#kontinuerliga variabler
v1 <- c("ed_gcs_sum","ed_sbp_value","ISS","dt_ed_first_ct","Ankomst_te")

#kvalitativa variabler
v2 <- c("ProbJN","Deceased","pt_asa_preinjury")
v3 <- c(v1,v2)

data.prob.c <- data.prob[v3]
data.prob.c$ProbJN <- as.factor(data.prob.c$ProbJN)
data.prob.c$Deceased <- as.factor(data.prob.c$Deceased)
data.prob.c$pt_asa_preinjury <- as.factor(data.prob.c$pt_asa_preinjury)

#Imputation av kvant med mean, BEHÖVER FIXA VARIABLER, EXV GCS  HAR 999 osv vilket gör den för hög!
#Behöver också fixa funktioner som göra detta automatiskt vid byte av variabler

data.prob.c$ed_gcs_sum[which(is.na(data.prob.c$ed_gcs_sum))] <- mean(data.prob.c$ed_gcs_sum,na.rm = TRUE)
data.prob.c$ed_sbp_value[which(is.na(data.prob.c$ed_sbp_value))] <- mean(data.prob.c$ed_sbp_value,na.rm = TRUE)
data.prob.c$ISS[which(is.na(data.prob.c$ISS))] <- mean(data.prob.c$ISS,na.rm = TRUE)
data.prob.c$dt_ed_first_ct[which(is.na(data.prob.c$dt_ed_first_ct))] <- mean(data.prob.c$dt_ed_first_ct,na.rm = TRUE)

#imputation av kval med vanligaste

data.prob.c$pt_asa_preinjury[which(is.na(data.prob.c$pt_asa_preinjury))] <- tail(names(sort(table(data.prob.c$pt_asa_preinjury))), 1)

#Sortera efter tid.och skapa vektor för 80% träning

dpc <- data.prob.c

dpc <-dpc[order(dpc$Ankomst_te),]

tv <- c(1:round(nrow(dpc)*0.8, digits = 0))

#simpelt första försök med mikropml

results <- run_ml(dpc,
                  'glmnet',
                  outcome_colname = 'ProbJN',
                  seed = 2019)
                  sukfold = 2,
                  cv_times = 5,
                  training_frac = tv
                  seed = 2019
                  )
# Nedan det jag gjorde innan mikropml för att leta ev prediktorer.

glm.fit=glm(prob10~
              pt_age_yrs+
              ed_sbp_value+
              ISS+
              ed_gcs_sum+
              ed_rr_value+
              pt_asa_preinjury+
              hosp_los_days+
              dt_ed_first_ct+
              dt_ed_emerg_proc
      ,data=data.prob,family = binomial)
summary(glm.fit)

View(glm.fit)

