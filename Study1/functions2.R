
#### Functions data
# Clean audit filters
# Clean predictor variables
# Imputation
# Preprocess data

##############################################

clean_audit_filters <- function(data) {
  
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
  
  data[,audit.filter][data[,audit.filter] == "Ja"| 
                                     data[,audit.filter] == "ja"] <- "Yes"
  
  data[,audit.filter][data[,audit.filter] == "Nej"| 
                                     data[,audit.filter] == "nej" | 
                                     data[,audit.filter] == "nj\r\nNej" | 
                                     data[,audit.filter] == "nj"] <- "No"
  ##### Is nn = NA or No???
  
  data[,audit.filter][data[,audit.filter] == "nn"] <- NA
  
  ### Create reference vector to check for false inputs in the audit filters.  
  Levels.audit.filters <- unique(as.vector(as.matrix(data[,audit.filter])))
  Levels.audit.filters <- Levels.audit.filters[!is.na(Levels.audit.filters)]
  
  ##Chose to remove NA above becouse sort(c("Yes", NA, "No")) only returns "No"  "Yes"?
  
  ##### 
  ##.   SIC (!!!) Oklart men nedan crashar bootstrapen?
  ######
  #original.levels.audit.filters <- sort(c("Yes", NA, "No"))
  #if (!identical(Levels.audit.filters, original.levels.audit.filters))
  #  stop ("Levels in Audit filters have changed")
  
  #########
  #  Convert NA:s in VK rows to No if VK_avslutad = Yes (To be able to calc false neg)
  #########
  
  #  for (i in 1:nrow(data)) {
  #    if (is.na(data[i,"VK_avslutad"]) == FALSE && data[i,"VK_avslutad"] == "Yes" && is.na(data[i,audit.filter2]) == TRUE ) {
  #      data[i,audit.filter2] <- "No"
  #    }
  #  }
  
  data[, audit.filter2] <- lapply(data[, audit.filter2], function(column) {
    column[is.na(column) & data$VK_avslutad == "Yes"] <- "No"
    return (column)
  })    
  
  return(data)
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
dataset <- as.data.frame(dataset)
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
    random.dataset <- data[index,]
    
    ##if (!is.null(index)) {
    ##  random.datasets <- data[index,]. #### Fungerade ej direkt, och tog bort i ett försök att minimera felkällor. 
    ##}
    
    ## Create OFI collumn
    random.dataset$ofi <- rofi::create_ofi(random.dataset)
    
    ## Clean previous audit filters
    random.clean.af <- clean_audit_filters(random.dataset)
    
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
    random.imputed.dataset <- imputation(clean.random.dataset)
    
    ### Remove predicts without variance (imputed without missing data)
    random.variance.data <- Filter(function(x)(length(unique(x))>1), random.imputed.dataset)
    random.preprocessed.data <- preprocess_data(random.variance.data)
    
    results.lr <- run_ml(dataset = random.preprocessed.data,
                         method = 'glmnet',
                         outcome_colname = "ofi",
                         perf_metric_name = "AUC",
                         kfold = 5,
                         cv_times = 5,
                         training_frac = 0.8,
                         seed = 2019)
    
#  results.forest <- run_ml(dataset = preprocessed.data,
#                             method = 'rf',
#                             outcome_colname = "ofi",
#                             perf_metric_name = "AUC",
#                             kfold = 5,
#                             cv_times = 5,
#                             training_frac = 0.8,
#                             seed = 2019)
#    
#    results.boost <- run_ml(dataset = preprocessed.data,
#                            method = 'xgbTree',
#                            outcome_colname = "ofi",
#                            perf_metric_name = "AUC",
#                            kfold = 5,
#                            cv_times = 5,
#                            training_frac = 0.8,
#                            seed = 2019) 
#    
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
    labels.ici <- as.character(labels)
    labels.ici[labels.ici == "Yes"] <- 1
    labels.ici[labels.ici == "No"] <- 0 
    labels.ici <- as.numeric(labels.ici)
    prediction.lr.vector <- as.vector(prediction.lr)
    ici.lr <- gmish::ici(prediction.lr.vector, labels.ici)
    
    auc <- auc.lr
    accuracy <- accuracy.lr
    ici <- ici.lr
    
    boot.result <- c(accuracy,auc,ici)    
        
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
#    final.model.boost <- results.boost$trained_model$finalModel
#    prediction.boost <- xgboost:::predict.xgb.Booster(final.model.boost, newdata = as.matrix(test.data), type = "response")
#    pred.boost <- ROCR::prediction(prediction.boost, labels)
#    auc.boost <- unlist(ROCR::performance(pred.boost, measure = "auc")@y.values)
#    auc.boost <- 1-as.numeric(auc.boost)
#    accuracy.boost <- ROCR::performance(pred.boost, measure = "acc")@y.values
#    accuracy.boost <- 1-mean(unlist(accuracy.boost))
    
    ## Result summary
    
    ## Delta AUC 
    #lr.rf.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.forest))
    #lr.boost.auc <- abs(as.numeric(auc.lr) - as.numeric(auc.boost))
    #rf.boost.auc <- abs(as.numeric(auc.boost) - as.numeric(auc.forest))
    
    #auc <- c(lr.auc = auc.lr,
    #         boost.auc = auc.boost,
    #         delta.lr.boost.auc = lr.boost.auc)
    
##    auc <- c(lr.auc = auc.lr,
##             rf.auc = auc.forest,
##             boost.auc = auc.boost,
##             delta.lr.rf.auc = lr.rf.auc,
##             delta.lr.boost.auc = lr.boost.auc,
##             delta.rf.boost.auc = rf.boost.auc)
    
    ## Delta accuracy
##    lr.rf.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.forest))
##    lr.boost.acc <- abs(as.numeric(accuracy.lr) - as.numeric(accuracy.boost))
##    rf.boost.acc <- abs(as.numeric(accuracy.boost) - as.numeric(accuracy.forest))
    
##    accuracy <- c(lr.accuracy = accuracy.lr,
##                  boost.accuracy = accuracy.boost,
##                  delta.lr.boost.accuracy = lr.boost.acc)
    
##    accuracy <- c(lr.accuracy = accuracy.lr,
##                  rf.accuracy = accuracy.forest,
##                  boost.accuracy = accuracy.boost,
##                  delta.lr.rf.accuracy = lr.rf.acc,
##                  delta.lr.boost.accuracy = lr.boost.acc,
##                  delta.rf.boost.accuracy = rf.boost.acc)
    

    
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

