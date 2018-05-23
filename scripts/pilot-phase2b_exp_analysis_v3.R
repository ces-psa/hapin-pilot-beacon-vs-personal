####Analysis for Guatemala pilot phase 2b
#Part 1-3 stored in the separate file 12/18/2017, thus not update here
#add option for analysis BL1, BL2, FU1, FU2
#version 3: change the location after organizing files and folder
#data collection finished, but not filter weighting (May 2018)

library(data.table)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(purrr)
library(readxl)
library(readr)

#####Part 0: Set Up Working Directory #####

rm(list =ls())
getwd()
#BL1
setwd("C:/Users/jliao8/Box Sync/Box Sync/Exposure Data--GUATEMALA (Shirin Jabbarzadeh)/PILOT IIB/DATA/BL1/ECM_processed")
setwd("/Users/Jiawen/Box Sync/Exposure Data--GUATEMALA/PILOT IIB/DATA/BL1/ECM_processed")
#BL2
setwd("C:/Users/jliao8/Box Sync/Box Sync/Exposure Data--GUATEMALA (Shirin Jabbarzadeh)/PILOT IIB/DATA/BL2/ECM_processed")
setwd("/Users/Jiawen/Box Sync/Exposure Data--GUATEMALA/PILOT IIB/DATA/BL2/ECM_processed")
#FU1
setwd("C:/Users/jliao8/Box Sync/Box Sync/Exposure Data--GUATEMALA (Shirin Jabbarzadeh)/PILOT IIB/DATA/FU1/ECM_processed")
setwd("/Users/Jiawen/Box Sync/Exposure Data--GUATEMALA/PILOT IIB/DATA/FU1/ECM_processed")
#FU2
setwd("C:/Users/jliao8/Box Sync/Box Sync/Exposure Data--GUATEMALA (Shirin Jabbarzadeh)/PILOT IIB/DATA/FU2/ECM_processed")
setwd("/Users/Jiawen/Box Sync/Exposure Data--GUATEMALA/PILOT IIB/DATA/FU2/ECM_processed")



#jump out from Part1-3
#windows
source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/pilot-phase2b_beacon_readin_v2.R")
#mac
source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/pilot-phase2b_beacon_readin_v2.R")


#save R processed data for BL1
save(list = ls(all=TRUE), file = "../../../../../temp_data/R_data/BL1.RData")

#save R processed data for BL2
save(list = ls(all=TRUE), file = "../../../../../temp_data/R_data/BL2.RData")

#save R processed data for FU1
save(list = ls(all=TRUE), file = "../../../../../temp_data/R_data/BL3.RData")

#save R processed data for FU2
save(list = ls(all=TRUE), file = "../../../../../temp_data/R_data/BL4.RData")



###Not run above
#===
#===
#===
#===
#===
#===
#===
#===
#####Part 1: Start from here ####
rm(list = ls())
#windows
setwd("C:/Users/jliao8/Box Sync/Box Sync/Exposure Data--GUATEMALA (Shirin Jabbarzadeh)/PILOT IIB/DATA/FU2/ECM_processed")
#mac
setwd("/Users/Jiawen/Box Sync/Exposure Data--GUATEMALA/PILOT IIB/DATA/FU2/ECM_processed")

#load BL1 data
load("../../../../../temp_data/R_data/BL1.RData")
#load BL2 data
load("../../../../../temp_data/R_data/BL2.RData")
#load BL3 data
load("../../../../../temp_data/R_data/BL3.RData")
#load BL4 data
load("../../../../../temp_data/R_data/BL4.RData")



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

####Part 4: Check Beacon logger for non-compliance issue####

#input (from previous read in section)
hhid_list = unique(Beacon_data2$hhid)
replicate_list = unique(Beacon_data2$replicate)

#Check whether Beacon logger is logging in data
#start time and end time need to incoporated
for(k in 1:length(replicate_list)){
      check_data = Beacon_data2[replicate==replicate_list[k]  ,]
      for(i in 1 : length(hhid_list)){
            check_data = Beacon_data2[hhid==hhid_list[i]  ,]
            Logger_list = unique(check_data$Beacon_logger_ID)
            for(j in 1 : length(Logger_list)){
                  check_data2 = check_data[Beacon_logger_ID ==Logger_list[j], ]
                  lag_minute = sum(diff(unique(check_data2$datetime3))>1)
                  message(paste("Household",hhid_list[i], "Beacon logger",Logger_list[j], "has",lag_minute, "minutes data not logged"))
                  rm(lag_minute);rm(check_data2)
            }
            rm(check_data)
            
      }
      
}

#visual check for location proximity
plot_list = list()
m = 1
for(k in 1 :length(replicate_list)){
      plot_data = Beacon_data2[replicate==replicate_list[k]  ,]
      for(i in 1 :length(hhid_list)){
            plot_data1 = plot_data[hhid==hhid_list[i]  ,]
            beacon_list = unique(plot_data1$MAC)
            for(j in 1 : length(beacon_list)){
                  plot_data2 = plot_data1[MAC ==beacon_list[j], ]
                  if(nrow(plot_data2) > 120){  #only process for over 2 hour data
                        plot_data2[,RSSI_max := max(RSSI_minute), by = c("datetime3","MAC")]
                        plot_data2[RSSI_max != RSSI_minute,RSSI_max := NA]
                        plot_data2[!is.na(RSSI_max) ,closest_logger_ID := Beacon_logger_ID]
                        plot_data2 = plot_data2[!is.na(RSSI_max),]
                        
                        p = ggplot(data = plot_data2,aes(x = as.POSIXct(datetime3), y = RSSI_max, color = monitor_env))+
                              geom_point(alpha = 0.75, size = 0.5)+
                              geom_segment(aes(x = as.POSIXct(datetime3),
                                               xend = as.POSIXct(datetime3),
                                               y = -20, yend = RSSI_max
                              ))+
                              labs(title = paste("Beacon MAC",beacon_list[j],"from household",hhid_list[i],
                                                 "from phase",replicate_list[k]),
                                   x = "Date time", y = "RSSI")
                        plot_list[[m]] = p
                        m = m+1
                  }     
            }
      }
}
plot_list[[1]]
plot_list[[3]]

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 4.1 Beacon walk though check####
#change phase, hhid and duplicate beacon accordingingly

Beacon_walk_through = read_excel("../../../../Excel CRFs/Beacon Deployment Log_GT.xlsx", 
                                       col_types = c("date", "date", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "date", "date", "text", "text", "date", 
                                                     "date", "text", "text", "date", "date", 
                                                     "text", "text", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "text", "text", 
                                                     "text", "text", "text", "text", "numeric"))
Beacon_walk_through = as.data.table(Beacon_walk_through)

#creat a empty table
correction_tabel = NULL
#this can change
#All hhid:"32140" "32141" "32142" "32143" "32144" "32145" "32146" "32147" "32148" "32149"
phase_selected = "04"
#windows
for(j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon1_v2.R")
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon2_v2.R")
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon3_v2.R")
}
#mac
for(j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon1_v2.R")
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon2_v2.R")
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/walk_through_c_Beacon3_v2.R")
}
#date
correction_tabel[,date := date(start_time)]

#data save in correction_tabel
#failure list: BL1: 32143, 32144, 32145, 32146,32147
#BL2: 32140, 32141, 32142, 32143, 32144, 32145, 32148
#FU2: 32141; 32144
#BL1
correction_tabel = correction_tabel[hhid != "32143"&hhid != "32144"&
                                    hhid != "32145"&hhid != "32146"&hhid != "32147",]
#BL2
correction_tabel = correction_tabel[hhid != "32140"&hhid != "32141"&
                                          hhid != "32142"&hhid != "32143"&hhid != "32144"
                                    &hhid != "32145"&hhid != "32148",]
#FU1
correction_tabel = correction_tabel[hhid != "32150",]

#FU2
correction_tabel = correction_tabel[hhid != "32141"&hhid != "32144",]

unique(correction_tabel$hhid)
mean(as.numeric(correction_tabel$true_rate))

#save
write.csv(correction_tabel, file = "../../../../../temp_data/Beacon_children/walk_through_correct.csv")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 4.2: Mother Beacon location analysis####
phase_selected = "02"
duplicate_Beacon = 1  #integer, the number of duplicate Beacon
location_selected = "PEO" #character, indicate location of Beacon PEO or PEC
activity_data2 = NULL

#test
#hhid_selected = "32149" #hhid selected

#windows
for(j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/Beacon_time_act.R")
      
}
#mac
for(j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/Beacon_time_act.R")
      
}
activity_data2 = as.data.table(activity_data2)
activity_data2 = activity_data2[!is.na(start_time),]
#BL1
activity_data2 = activity_data2[hhid != "32143"&hhid != "32144"&
                                          hhid != "32145"&hhid != "32146"&hhid != "32147",]
#BL2
activity_data2 = activity_data2[hhid != "32140"&hhid != "32141"&
                                          hhid != "32142"&hhid != "32143"&hhid != "32144"
                                    &hhid != "32145"&hhid != "32148",]
#FU1
#activity_data2 = activity_data2[hhid != "32150",]

#FU2
activity_data2 = activity_data2[hhid != "32141"&hhid != "32144",]


#information stored in activity_data2
unique(activity_data2$hhid)
mean(activity_data2$Total_hour);sd(activity_data2$Total_hour)
mean(activity_data2$KAP_hours);sd(activity_data2$KAP_hours)
mean(activity_data2$SAP_hour);sd(activity_data2$SAP_hour)
mean(activity_data2$HOP_hour);sd(activity_data2$HOP_hour)



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 5.1: ECM Mother data discription (excluding PEM Beacon logger)####
ECM_all[,datetime3 := floor_date(datetime2, unit = "hour")]
ECM_all = ECM_all[ECM_PM>0,]
ECM_all[,hour_PM := mean(ECM_PM, na.rm = T), by = c("hhid","datetime3","ECM_serialNumber")]
ECM_all[,hh_location_PM := mean(ECM_PM, na.rm =T), by = c("hhid","ECM_location")]
HH_location_ECM = unique(ECM_all[,c("hhid","phase","ECM_location","hh_location_PM")])
unique(HH_location_ECM$hhid)

mean(HH_location_ECM[ECM_location =="HOP",hh_location_PM]);mean(HH_location_ECM[ECM_location =="KAP1",hh_location_PM]);mean(HH_location_ECM[ECM_location =="SAP",hh_location_PM]);mean(HH_location_ECM[ECM_location =="PEO",hh_location_PM])
sd(HH_location_ECM[ECM_location =="HOP",hh_location_PM]);sd(HH_location_ECM[ECM_location =="KAP1",hh_location_PM]);sd(HH_location_ECM[ECM_location =="SAP",hh_location_PM]);sd(HH_location_ECM[ECM_location =="PEO",hh_location_PM])
median(HH_location_ECM[ECM_location =="PEO",hh_location_PM])

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 5.2: ECM Mother direct/indirect/kitchen correlation analysis####

#input for this section
#select phase and hhid. This will be replaced by loop or customized input
#test
phase_selected = "03"  #repliace
#hhid_selected = "32149" #hhid selected

duplicate_Beacon = 1  #integer, the number of duplicate Beacon
mother_duplicate_ECM = 1 #integer, the number of duplicated Mother ECM
KAP_duplicate_ECM = 1 #integer, the number of duplicated KAP ECM
hour_data = NULL
hour_data2 = NULL

#windows
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEO_ECM_comparison_v2.R")
}

#mac
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEO_ECM_comparison_v2.R")
}

#BL1
hour_data2 = hour_data2[hhid != "32143"&hhid != "32144"&
                                          hhid != "32145"&hhid != "32146"&hhid != "32147",]
#BL2
hour_data2 = hour_data2[hhid != "32140"&hhid != "32141"&
                                          hhid != "32142"&hhid != "32143"&hhid != "32144"
                                    &hhid != "32145"&hhid != "32148"&hhid!= "32147",]
#FU1
hour_data2 = hour_data2[hhid != "32150",]


#FU2
hour_data2 = hour_data2[hhid != "32141"&hhid != "32144",]


hour_data2 = hour_data2[!is.na(correlation_d_i),]
hour_data2 = hour_data2[h_direct >0]

unique(hour_data2$hhid)
#indirect
mean(hour_data2$h_indirect, na.rm = T );sd(hour_data2$h_indirect, na.rm = T )
mean(hour_data2$h_direct, na.rm = T );sd(hour_data2$h_direct, na.rm = T )

mean(unique(hour_data2$correlation_d_i));sd(unique(hour_data2$correlation_d_i))
mean(unique(hour_data2$correlation_d_k), na.rm = T);sd(unique(hour_data2$correlation_d_k), na.rm = T)
##another way
cor(hour_data2$h_direct, hour_data2$h_indirect, use = "pairwise.complete.obs", method = "spearman")
cor(hour_data2$h_direct, hour_data2$h_KAP, use = "pairwise.complete.obs",method = "spearman")

plot(hour_data2$h_direct, hour_data2$h_indirect)

write.csv(hour_data2, file = "../../../../../temp_data/Beacon_children/PEO_correction.csv")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

####Part 5.3 Lascar Mother description####
Lascar_data2[,datetime_h := floor_date(datetime, unit = "hour")]
Lascar_data2[,hour_CO := mean(CO_ppm, na.rm = T), by = c("hhid","datetime_h","Lascar_id")]
Lascar_data2[,hh_location_CO := mean(CO_ppm, na.rm =T), by = c("hhid","Lascar_location")]
HH_location_CO = unique(Lascar_data2[,c("hhid","phase","Lascar_location","hh_location_CO")])
unique(HH_location_CO$hhid)

mean(HH_location_CO[Lascar_location =="HOP",hh_location_CO]);mean(HH_location_CO[Lascar_location =="KAP1",hh_location_CO]);mean(HH_location_CO[Lascar_location =="SAP",hh_location_CO]);mean(HH_location_CO[Lascar_location =="PEO",hh_location_CO]);mean(HH_location_CO[Lascar_location =="PEC",hh_location_CO])
sd(HH_location_CO[Lascar_location =="HOP",hh_location_CO]);sd(HH_location_CO[Lascar_location =="KAP1",hh_location_CO]);sd(HH_location_CO[Lascar_location =="SAP",hh_location_CO]);sd(HH_location_CO[Lascar_location =="PEO",hh_location_CO]);sd(HH_location_CO[Lascar_location =="PEC",hh_location_CO])


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 5.4 Lascar Mother direct/indirect/kitchen correlation analysis####
#input for this section
#select phase and hhid. This will be replaced by loop or customized input
#test
phase_selected = "04"  #repliace
#hhid_selected = "32149" #hhid selected

duplicate_Beacon = 1  #integer, the number of duplicate Beacon
mother_duplicate_Lascar = 1 #integer, the number of duplicated Mother Lascar
KAP_duplicate_Lascar = 1 #integer, the number of duplicated KAP Lascar
m_hour_data_CO = NULL
m_hour_data_CO_2 = NULL

#mac
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEO_CO_comparison.R")
}
#windows
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEO_CO_comparison.R")
}

m_hour_data_CO_2 = m_hour_data_CO_2[!is.na(m_correlation_d_i),]
unique(m_hour_data_CO_2$hhid)

#BL1
m_hour_data_CO_2 = m_hour_data_CO_2[hhid != "32143"&hhid != "32144"&
                              hhid != "32145"&hhid != "32146"&hhid != "32147",]
#BL2
m_hour_data_CO_2 = m_hour_data_CO_2[hhid != "32140"&hhid != "32141"&
                              hhid != "32142"&hhid != "32143"&hhid != "32144"
                        &hhid != "32145"&hhid != "32148",]
#FU2
m_hour_data_CO_2 = m_hour_data_CO_2[hhid != "32141"&hhid != "32144",]

unique(m_hour_data_CO_2$hhid)

#direct PEO
mean(m_hour_data_CO_2$h_direct_CO, na.rm = T );sd(m_hour_data_CO_2$h_direct_CO, na.rm = T )
#indirect PEO
mean(m_hour_data_CO_2$h_indirect_CO, na.rm = T );sd(m_hour_data_CO_2$h_indirect_CO, na.rm = T )

#correlations
mean(unique(m_hour_data_CO_2$m_correlation_d_i), na.rm = T);sd(unique(m_hour_data_CO_2$m_correlation_d_i), na.rm = T)
mean(unique(m_hour_data_CO_2$m_correlation_d_k), na.rm = T);sd(unique(m_hour_data_CO_2$m_correlation_d_k), na.rm = T)

##another way
#d-i
cor(m_hour_data_CO_2$h_indirect_CO,m_hour_data_CO_2$h_direct_CO, use = "pairwise.complete.obs", method = "spearman")
#d-k
cor(m_hour_data_CO_2$h_KAP_CO,m_hour_data_CO_2$h_direct_CO, use = "pairwise.complete.obs", method = "spearman")

plot(m_hour_data_CO_2$h_indirect_CO,m_hour_data_CO_2$h_direct_CO)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 5.5 Lascar Children direct/indirect/kitchen/mother correlation analysis####
phase_selected = "04"  #repliace
#hhid_selected = "32149" #hhid selected

duplicate_Beacon = 1  #integer, the number of duplicate Beacon
mother_duplicate_Lascar = 1 #integer, the number of duplicated Mother Lascar
KAP_duplicate_Lascar = 1 #integer, the number of duplicated KAP Lascar
c_duplicate_Lascar = 1 #integer, the number of duplicated PEC Lascar
c_hour_data_CO = NULL
c_hour_data_CO_2 = NULL

#mac
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEC_CO_comparison.R")
}

#windwos
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEC_CO_comparison.R")
}
#BL1
c_hour_data_CO_2 = c_hour_data_CO_2[hhid != "32143"&hhid != "32144"&
                              hhid != "32145"&hhid != "32146"&hhid != "32147",]
#BL2
c_hour_data_CO_2 = c_hour_data_CO_2[hhid != "32140"&hhid != "32141"&
                              hhid != "32142"&hhid != "32143"&hhid != "32144"
                        &hhid != "32145"&hhid != "32148",]
#FU2
c_hour_data_CO_2 = c_hour_data_CO_2[hhid != "32141"&hhid != "32144",]

c_hour_data_CO_2 = c_hour_data_CO_2[!is.na(c_correlation_d_i),]
unique(c_hour_data_CO_2$hhid)
#children direct and indirect CO levels
mean(c_hour_data_CO_2$c_h_indirect_CO, na.rm = T );sd(c_hour_data_CO_2$c_h_indirect_CO, na.rm = T )
mean(c_hour_data_CO_2$c_h_direct_CO, na.rm = T );sd(c_hour_data_CO_2$c_h_direct_CO, na.rm = T )

#correlations with direct between indirect, kitchen and mother
mean(unique(c_hour_data_CO_2$c_correlation_d_i),na.rm = T);sd(unique(c_hour_data_CO_2$c_correlation_d_i),na.rm = T)
mean(unique(c_hour_data_CO_2$c_correlation_d_k), na.rm = T);sd(unique(c_hour_data_CO_2$c_correlation_d_k), na.rm = T)
mean(unique(c_hour_data_CO_2$c_correlation_d_m), na.rm = T);sd(unique(c_hour_data_CO_2$c_correlation_d_m), na.rm = T)

#another way
cor(c_hour_data_CO_2$c_h_direct_CO, c_hour_data_CO_2$c_h_indirect_CO, use = "pairwise.complete.obs",method = "spearman")
cor(c_hour_data_CO_2$c_h_direct_CO, c_hour_data_CO_2$h_KAP_CO, use = "pairwise.complete.obs",method = "spearman")
cor(c_hour_data_CO_2$c_h_direct_CO, c_hour_data_CO_2$h_PEO_CO, use = "pairwise.complete.obs",method = "spearman")

plot(c_hour_data_CO_2$c_h_direct_CO, c_hour_data_CO_2$c_h_indirect_CO)


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 6: Analyze PEC data####
#input for this part
phase_selected = "04"  #repliace
duplicate_Beacon = 1  #integer, the number of duplicate Beacon
Children_PM = NULL
Children_PM2 = NULL

#hhid_selected = "32143" #hhid selected
#windows
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("C:/Users/jliao8/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEC_PM_analysis.R")
}

#mac
for( j in 1 : length(unique(Beacon_data$hhid))){
      hhid_selected = unique(Beacon_data$hhid)[j]
      source("/Users/Jiawen/Google Drive/Emory research/HAPIN/HAPIN_pilot/Children exposure/beacon/R_code(with_data)/function/PEC_PM_analysis.R")
}
#removinf hhid with Beacon failure
Children_PM2 = as.data.table(Children_PM2)
Children_PM2[,hhid_selected := as.character(hhid_selected)]
#BL1
Children_PM2 = Children_PM2[hhid_selected != "32143"&hhid_selected != "32144"&
                                  hhid_selected != "32145"&hhid_selected != "32146"&hhid_selected != "32147",]
#BL2
Children_PM2 = Children_PM2[hhid_selected != "32140"&hhid_selected != "32141"&
                                  hhid_selected != "32142"&hhid_selected != "32143"&hhid_selected != "32144"
                            &hhid_selected != "32145"&hhid_selected != "32148",]
#FU2
Children_PM2 = Children_PM2[hhid_selected != "32141"&hhid_selected != "32144",]

unique(Children_PM2$hhid_selected)

#location of children
mean(Children_PM2$sampling_time);sd(Children_PM2$sampling_time)
mean(Children_PM2$HOP_time);sd(Children_PM2$HOP_time)
mean(Children_PM2$PEO_time);sd(Children_PM2$PEO_time)
mean(Children_PM2$KAP_time);sd(Children_PM2$KAP_time)
mean(Children_PM2$SAP_time);sd(Children_PM2$SAP_time)



#Beacom method PM exposure for children
mean(Children_PM2$mean_PM, na.rm = T);sd(Children_PM2$mean_PM, na.rm = T)



##Below are under-development
#####Part 7: ####






#######Archieved Code Section Below#################

###Some runs after Beacon_test3 file



####Compare PEM direct and in-direct
#compare and merge
PEM_exposure = merge(data2_1[,c("datetime3","Logger_location","Location","ECM_PM")], 
                     ECM_all[ECM_location == "PEM",c("datetime2","ECM_PM","ECM_location")],
                     by.x = "datetime3", by.y = "datetime2", all.x = T, all.y = F)
cor(PEM_exposure$ECM_PM.x, PEM_exposure$ECM_PM.y, use = "complete.obs")   #correlation between indirect minute and direct minute
fit1 = lm(ECM_PM.y ~ ECM_PM.x, data = PEM_exposure)
summary(fit1)
#hourly mean
PEM_exposure[,hourtime := floor_date(datetime3, unit = "hours") ]
PEM_exposure[,h_indirect := mean(ECM_PM.x, na.rm = T), by = hourtime]
PEM_exposure[,h_direct := mean(ECM_PM.y, na.rm = T), by = hourtime]
h_indirect = unique(PEM_exposure$h_indirect)
h_direct = unique(PEM_exposure$h_direct)
cor(h_indirect,h_direct, use = "complete.obs")

#plot

plot(h_indirect,h_direct)

ggplot(data2_1, aes(x =as.POSIXct(datetime3) , y = ECM_PM, color = Logger_location))+
      geom_segment(aes(x = as.POSIXct(datetime3),
                       xend = as.POSIXct(datetime3),
                       y = 0, yend = ECM_PM))
ggplot(data4_1, aes(x =as.POSIXct(datetime3) , y = ECM_PM, color = Logger_location))+
      geom_segment(aes(x = as.POSIXct(datetime3),
                       xend = as.POSIXct(datetime3),
                       y = 0, yend = ECM_PM))


#direct
ggplot(ECM_all[ECM_location == "PEM",], aes(x =as.POSIXct(datetime2),  y = ECM_PM  ))+
      geom_line()

#combine analysis

ggplot(PEM_exposure, aes(x = ECM_PM.x, y = ECM_PM.y))+
      geom_point(size = 0.3, alpha = 0.3)
PEM_exposure_long = melt(PEM_exposure, id.vars = c("datetime3", "Logger_location","Location","ECM_location") )
ggplot(PEM_exposure_long, aes(x = as.POSIXct(datetime3), y = value, color = variable))+
      geom_line()




