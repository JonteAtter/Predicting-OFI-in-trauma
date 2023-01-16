combine_rts <- function(data) {
  
  #################
  # Combine RTS #
  #################
  ########
  ## SBP #
  ########
  ## ed.sbp
  ed.sbp.4 <- round(mean(data[data$ed_sbp_value > 89,"ed_sbp_value"], na.rm = TRUE))
  ed.sbp.3 <- round(mean(data[data$ed_sbp_value <= 89 & data$ed_sbp_value > 75 ,"ed_sbp_value"], na.rm = TRUE))
  ed.sbp.2 <- round(mean(data[data$ed_sbp_value <= 75 & data$ed_sbp_value > 49 ,"ed_sbp_value"], na.rm = TRUE))
  ed.sbp.1 <- round(mean(data[data$ed_sbp_value <= 49 & data$ed_sbp_value > 1 ,"ed_sbp_value"], na.rm = TRUE))
  ed.sbp.0 <- 0
  
  data[is.na(data$ed_sbp_value) == TRUE & is.na(data$ed_sbp_rtscat) == FALSE & data$ed_sbp_rtscat == 4,"ed_sbp_value"] <- ed.sbp.4
  data[is.na(data$ed_sbp_value) == TRUE & is.na(data$ed_sbp_rtscat) == FALSE & data$ed_sbp_rtscat == 3,"ed_sbp_value"] <- ed.sbp.3
  data[is.na(data$ed_sbp_value) == TRUE & is.na(data$ed_sbp_rtscat) == FALSE & data$ed_sbp_rtscat == 2,"ed_sbp_value"] <- ed.sbp.2
  data[is.na(data$ed_sbp_value) == TRUE & is.na(data$ed_sbp_rtscat) == FALSE & data$ed_sbp_rtscat == 1,"ed_sbp_value"] <- ed.sbp.1
  data[is.na(data$ed_sbp_value) == TRUE & is.na(data$ed_sbp_rtscat) == FALSE & data$ed_sbp_rtscat == 0,"ed_sbp_value"] <- ed.sbp.0

  ## pre.sbp
  pre.sbp.4 <- round(mean(data[data$pre_sbp_value > 89,"pre_sbp_value"], na.rm = TRUE))
  pre.sbp.3 <- round(mean(data[data$pre_sbp_value <= 89 & data$pre_sbp_value > 75 ,"pre_sbp_value"], na.rm = TRUE))
  pre.sbp.2 <- round(mean(data[data$pre_sbp_value <= 75 & data$pre_sbp_value > 49 ,"pre_sbp_value"], na.rm = TRUE))
  pre.sbp.1 <- round(mean(data[data$pre_sbp_value <= 49 & data$pre_sbp_value > 1 ,"pre_sbp_value"], na.rm = TRUE))
  pre.sbp.0 <- 0
  
  data[is.na(data$pre_sbp_value) == TRUE & is.na(data$pre_sbp_rtscat) == FALSE & data$pre_sbp_rtscat == 4,"pre_sbp_value"] <- pre.sbp.4
  data[is.na(data$pre_sbp_value) == TRUE & is.na(data$pre_sbp_rtscat) == FALSE & data$pre_sbp_rtscat == 3,"pre_sbp_value"] <- pre.sbp.3
  data[is.na(data$pre_sbp_value) == TRUE & is.na(data$pre_sbp_rtscat) == FALSE & data$pre_sbp_rtscat == 2,"pre_sbp_value"] <- pre.sbp.2
  data[is.na(data$pre_sbp_value) == TRUE & is.na(data$pre_sbp_rtscat) == FALSE & data$pre_sbp_rtscat == 1,"pre_sbp_value"] <- pre.sbp.1
  data[is.na(data$pre_sbp_value) == TRUE & is.na(data$pre_sbp_rtscat) == FALSE & data$pre_sbp_rtscat == 0,"pre_sbp_value"] <- pre.sbp.0
  
  ########
  ## RR ##
  ########
  ## ed.rr
  ed.rr.4 <- round(mean(data[data$ed_rr_value > 9 & data$ed_rr_value <30,"ed_rr_value"], na.rm = TRUE))
  ed.rr.3 <- round(mean(data[data$ed_rr_value >= 30  ,"ed_rr_value"], na.rm = TRUE))
  ed.rr.2 <- round(mean(data[data$ed_rr_value <= 9 & data$ed_rr_value > 5 ,"ed_rr_value"], na.rm = TRUE))
  ed.rr.1 <- round(mean(data[data$ed_rr_value <= 5 & data$ed_rr_value > 0 ,"ed_rr_value"], na.rm = TRUE))
  ed.rr.0 <- 0
  
  data[is.na(data$ed_rr_value) == TRUE & is.na(data$ed_rr_rtscat) == FALSE & data$ed_rr_rtscat == 4,"ed_rr_value"] <- ed.rr.4
  data[is.na(data$ed_rr_value) == TRUE & is.na(data$ed_rr_rtscat) == FALSE & data$ed_rr_rtscat == 3,"ed_rr_value"] <- ed.rr.3
  data[is.na(data$ed_rr_value) == TRUE & is.na(data$ed_rr_rtscat) == FALSE & data$ed_rr_rtscat == 2,"ed_rr_value"] <- ed.rr.2
  data[is.na(data$ed_rr_value) == TRUE & is.na(data$ed_rr_rtscat) == FALSE & data$ed_rr_rtscat == 1,"ed_rr_value"] <- ed.rr.1
  data[is.na(data$ed_rr_value) == TRUE & is.na(data$ed_rr_rtscat) == FALSE & data$ed_rr_rtscat == 0,"ed_rr_value"] <- ed.rr.0

  ## pre.rr
  pre.rr.4 <- round(mean(data[data$pre_rr_value > 9 & data$pre_rr_value <30,"pre_rr_value"], na.rm = TRUE))
  pre.rr.3 <- round(mean(data[data$pre_rr_value >= 30  ,"pre_rr_value"], na.rm = TRUE))
  pre.rr.2 <- round(mean(data[data$pre_rr_value <= 9 & data$pre_rr_value > 5 ,"pre_rr_value"], na.rm = TRUE))
  pre.rr.1 <- round(mean(data[data$pre_rr_value <= 5 & data$pre_rr_value > 0 ,"pre_rr_value"], na.rm = TRUE))
  pre.rr.0 <- 0
  
  data[is.na(data$pre_rr_value) == TRUE & is.na(data$pre_rr_rtscat) == FALSE & data$pre_rr_rtscat == 4,"pre_rr_value"] <- pre.rr.4
  data[is.na(data$pre_rr_value) == TRUE & is.na(data$pre_rr_rtscat) == FALSE & data$pre_rr_rtscat == 3,"pre_rr_value"] <- pre.rr.3
  data[is.na(data$pre_rr_value) == TRUE & is.na(data$pre_rr_rtscat) == FALSE & data$pre_rr_rtscat == 2,"pre_rr_value"] <- pre.rr.2
  data[is.na(data$pre_rr_value) == TRUE & is.na(data$pre_rr_rtscat) == FALSE & data$pre_rr_rtscat == 1,"pre_rr_value"] <- pre.rr.1
  data[is.na(data$pre_rr_value) == TRUE & is.na(data$pre_rr_rtscat) == FALSE & data$pre_rr_rtscat == 0,"pre_rr_value"] <- pre.rr.0
  
   return(data)
}