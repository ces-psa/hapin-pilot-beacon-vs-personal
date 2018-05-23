##Analysis of PEO data and compare direct and indirect ECm concentration
#Feb 11, 2018
#Feb.21, change to 30s interval

message("Processing household ",hhid_selected," in phase ",phase_selected)
#select ECM by hhid and phase (BL1/BL2FU1FU2)
ECM_selected = ECM_all[phase == phase_selected&hhid == hhid_selected,]

#select Beacon by hhid and phase (BL1/BL2FU1FU2)
Beacon_data_selected = Beacon_data2[replicate == phase_selected & hhid == hhid_selected,]
#change letter size of Beacon data
Beacon_data_selected[, MAC := toupper(MAC)]
#merge with Beacon location and Beacon ID
Beacon_data_selected = merge(Beacon_data_selected,r_Beacon_log[record_id ==hhid_selected&h41_visit==as.integer(phase_selected),
                                                               c("Beacon_MAC","Beacon_ID","Beacon_location","Notes")],
                             by.x = "MAC", by.y = "Beacon_MAC", all.x = T, all.y = F )

#firstly, focus on beacon worn by mother
Mother_Beacon = Beacon_data_selected[Beacon_location == "PEO",]
#check whether matched
if(nrow(Mother_Beacon)==0){
      message("No Beacon information")
      return()
}


#remove all PEO Beacon logger 
Mother_Beacon = Mother_Beacon[monitor_env != "PEO",]
#closest Beacon logger
Mother_Beacon[,RSSI_max := max(RSSI_30s), by = c("datetime3","MAC")]
Mother_Beacon[RSSI_max != RSSI_30s,RSSI_max := NA]
Mother_Beacon[!is.na(RSSI_max) ,closest_logger_ID := Beacon_logger_ID]
#merge with ECM for indirect ECM (there will be some duplicate ECM)
Mother_Beacon = merge(Mother_Beacon, ECM_selected[,c("datetime2","ECM_serialNumber","filter_id","ECM_location","ECM_PM")], 
                      by.x = c("datetime3","monitor_env"),
                      by.y = c("datetime2","ECM_location"), all.x = T, all.y = F)
#creat new variable for display
Mother_Beacon[,BeaconID_location := paste(Beacon_ID,Beacon_location)] #direct one
Mother_Beacon[,Logger_ID_location := paste(Beacon_logger_ID,monitor_env)] #indirect one
#direct ECM (also may be duplicate ECM)
PEO_ECM = ECM_selected[ECM_location == "PEO",c("datetime2","ECM_serialNumber","filter_id","ECM_location","ECM_PM")]
if(length(unique(PEO_ECM$ECM_serialNumber))>1){
      PEO_ECM2 = PEO_ECM[ECM_serialNumber == unique(PEO_ECM$ECM_serialNumber)[mother_duplicate_ECM],]
      
}else {
      PEO_ECM2 = PEO_ECM
}
#merge indirect with direct
Mother_exposure = merge(Mother_Beacon,PEO_ECM2,by.x = "datetime3", by.y = "datetime2", all.x = T, all.y = F)  #x is indirect, y is direct
Mother_exposure = Mother_exposure[!is.na(closest_logger_ID),]

#KAP ECM
KAP_ECM = ECM_selected[ECM_location == "KAP1",c("datetime2","ECM_serialNumber","filter_id","ECM_PM")]
colnames(KAP_ECM) = c("datetime2","ECM_serialNumber.KAP","filter_id.KAP","ECM_PM.KAP")
#merge indirect, direct ECM results with KAP1 results
if(length(unique(KAP_ECM$ECM_serialNumber.KAP))>1){
      KAP_ECM2 = KAP_ECM[ECM_serialNumber.KAP == unique(KAP_ECM$ECM_serialNumber.KAP)[KAP_duplicate_ECM],]
      
}else {
      KAP_ECM2 = KAP_ECM
}

Mother_exposure2 = merge(Mother_exposure, KAP_ECM2, by.x = "datetime3",by.y = "datetime2", all.x = T, all.y = F)

#there need one step to select duplicate ECM for analysis
#CODE NEEDED#
#select which beacon to analyze (there are two duplicate in the person)
PEO_Beacon_list = unique(Mother_Beacon$MAC)
Mother_exposure2 = Mother_exposure2[MAC == PEO_Beacon_list[duplicate_Beacon],]

Mother_exposure2[,hourtime := floor_date(datetime3, unit = "hours") ]
Mother_exposure2[,h_indirect := mean(ECM_PM.x, na.rm = T), by = hourtime]
Mother_exposure2[,h_direct := mean(ECM_PM.y, na.rm = T), by = hourtime]
Mother_exposure2[,h_KAP := mean(ECM_PM.KAP,na.rm = T), by = hourtime]
hour_data = unique(Mother_exposure2[,c("hhid","replicate","hourtime","h_indirect","h_direct", "h_KAP")])
#hour_data = subset(hour_data, !is.nan(h_direct)&!is.nan(h_indirect)&!is.nan(h_KAP))
correlation_d_i = cor(hour_data$h_direct,hour_data$h_indirect, use = "na.or.complete", method = "spearman")
correlation_d_k = cor(hour_data$h_direct,hour_data$h_KAP, use = "na.or.complete",method = "spearman")
hour_data[!is.nan(h_direct)&!is.nan(h_indirect)&!is.nan(h_KAP),correlation_d_i :=correlation_d_i]
hour_data[!is.nan(h_direct)&!is.nan(h_indirect)&!is.nan(h_KAP),correlation_d_k :=correlation_d_k]
hour_data[,phase := phase_selected];hour_data[,hhid := hhid_selected]

hour_data2 = rbind(hour_data2, hour_data)

#plot the time-series
# plot_file = melt.data.table(Mother_exposure[!is.na(ECM_PM.x)&!is.na(ECM_PM.y),
#                                             c("datetime3","monitor_env","ECM_PM.x","ECM_PM.y")],
#                             id.vars = c("datetime3", "monitor_env"))
# plot_file[variable == "ECM_PM.x",variable := "ECM_PM_indirect"];plot_file[variable == "ECM_PM.y",variable := "ECM_PM_direct"]
# p = ggplot(plot_file, aes(x = as.POSIXct(datetime3), y = value, color = variable))+
#       geom_line()
# p