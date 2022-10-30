#### Need to add collumns here???

remove_columns <- function(data) {

data <- data[,-grep("VK_", colnames(data))]
data <- data[,-grep("AIS", colnames(data))]
data <- data[,-grep("ICD_", colnames(data))]
data <- data[,-grep("pac_", colnames(data))]
data <- data[,-grep("Fr", colnames(data))]
data <- data[,-grep("Date", colnames(data))]
data <- data[,-grep("rts", colnames(data))]
# data <- data[,-grep("dt", colnames(data))] need these

#### Columns not needed for prediction
data <- subset(data,
               select = -c(did
                           ,Ankomst_te
                           ,tra_id                         
                           ,pat_id
                           ,Sjukhuskod                     
                           ,PersonIdentity
                           ,TempIdentity                   
                           ,DOB
                           ,Deceased
                           ,tra_DodsfallsanalysGenomford
                           ,id                             
                           ,arrival
                           ,ISS_moore_15_trauma_3          
                           ,bedomn_primar_granskning
                           ,pat_personnummer               
                           ,ISS_less_15_trauma_1_2
                           ,pat_TempPersonnummer           
                           ,tid_skadeplats_moore_20min
                           ,tid_skadeplats                 
                           ,GCS_less13_DT_moore_120
                           ,Riktlinje                      
                           ,origin                         
                           ,Personnummer                  
                           ,Reservnummer                                              
                           ,tillavdelning_Direkt_eft_TE_AKM
                           ,IVA_efter_TE
                           ,IVA_eft_avd                    
                           ,iva_dagar_n          
                           ,Död.datum
                           ,uppfolj_30_dgr 
                           ,iva_vardtillfallen_n
                           ,Klar
                           ,SweTr                          
                           ,Flyttad_till_avd_eft_iva
                           ,inlagd_swetrau                 
                           ,waran_beh_vid_ank
                           ,noak_vid_ankomst               
                           ,Problemomrade_.FMP
                           ,Gender
                           ,Kön
                           ,Tr_Nivå   ### härifrån bantat pga stor tabell. 
                           ,korrekt_triage
                           ,ed_tta
                           ,TraumaAlarmCriteria
                           ,ed_be_art_NotDone
                           ,ed_intubated
                           ,pre_intubated))
return(data)
}
