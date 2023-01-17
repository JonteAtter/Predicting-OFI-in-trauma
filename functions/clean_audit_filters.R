
#################################
# Clean audit-filters to Yes/No #
#################################

clean_audit_filters <- function(data) {

  
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
  
  ##### 
  ##.   SIC (!!!) The safety check is not compatible with a bootsrap, replications probably lack one of the original values.
  ######
  #original.levels.audit.filters <- sort(c("Yes", NA, "No"))
  #if (!identical(Levels.audit.filters, original.levels.audit.filters))
  #  stop ("Levels in Audit filters have changed")
  
  #########
  #  Convert NA:s in VK rows to No if VK_avslutad = Yes (To be able to calc false neg)
  #########
  
  data[, audit.filter2] <- lapply(data[, audit.filter2], function(column) {
    column[is.na(column) & data$VK_avslutad == "Yes"] <- "No"
    return (column)
  })    
  
  return(data)
}
