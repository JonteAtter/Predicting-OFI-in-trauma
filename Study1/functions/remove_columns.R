#### Need to add collumns here???

remove_columns <- function(data) {
data<-dataset.clean.af
data <- data[,-grep("VK_", colnames(data))]
data <- data[,-grep("AIS", colnames(data))]
data <- data[,-grep("ICD_", colnames(data))]
data <- data[,-grep("pac_", colnames(data))]
data <- data[,-grep("Fr", colnames(data))]

#### Columns not needed for prediction
data <- subset(data,
               select = -c(did
                           ,tra_id                         
                           ,pat_id
                           ,Sjukhuskod                     
                           ,PersonIdentity
                           ,TempIdentity                   
                           ,DOB
                           ,Deceased                       
                           ,DeceasedDate
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
                           ,iva_vardtillfallen_n           
                           ,Död.datum
                           ,uppfolj_30_dgr                 
                           ,Klar
                           ,SweTr                          
                           ,Flyttad_till_avd_eft_iva
                           ,inlagd_swetrau                 
                           ,waran_beh_vid_ank
                           ,noak_vid_ankomst               
                           ,Problemomrade_.FMP))
return(data)
}
