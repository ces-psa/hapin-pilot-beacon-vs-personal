##Walk through correction function Beacon logger 1
#revise Feb 5, removing PEO for the analysis
#revise Feb 11, change to 30s

Walk_through_selected = Beacon_walk_through[`House ID` == hhid_selected & Round == as.numeric(phase_selected),]
#here using 10 second data
Beacon_data_selected = Beacon_data[replicate == phase_selected & hhid == hhid_selected,]
Beacon_data_selected[,MAC := toupper(MAC)]
Beacon_data_selected = merge(Beacon_data_selected,r_Beacon_log[record_id ==hhid_selected&h41_visit==as.integer(phase_selected),
                                                               c("Beacon_MAC","Beacon_ID","Beacon_location","Notes")],
                             by.x = "MAC", by.y = "Beacon_MAC", all.x = T, all.y = F )

#walk through test at Beacon logger 1
# test_logger = Walk_through_selected$BeaconLogger1Name
# test_logger_location = Walk_through_selected$BeaconLogger1Location
# start_time = as.character(Walk_through_selected$BeaconLogger1TestStart)
# end_time = as.character(Walk_through_selected$BeaconLogger1TestEnd)

#walk through test at Beacon logger 2
test_logger = Walk_through_selected$BeaconLogger2Name
test_logger_location = Walk_through_selected$BeaconLogger2Location
start_time = as.character(Walk_through_selected$BeaconLogger2TestStart)
end_time = as.character(Walk_through_selected$BeaconLogger2TestEnd)

#walk through test at Beacon logger 3
# test_logger = Walk_through_selected$BeaconLogger3Name
# test_logger_location = Walk_through_selected$BeaconLogger3Location
# start_time = as.character(Walk_through_selected$BeaconLogger3TestStart)
# end_time = as.character(Walk_through_selected$BeaconLogger3TestEnd)

if(!is.na(start_time) & !is.na(end_time)){
      
      date= date(sort(Beacon_data_selected$datetime2)[30])
      start_time = strsplit(start_time," ")[[1]][2]
      end_time = strsplit(end_time," ")[[1]][2]
      start_time = ymd_hms(paste(date,start_time), tz = "America/Guatemala")
      end_time = ymd_hms(paste(date,end_time), tz = "America/Guatemala")
      
      Beacon_data_selected_test = Beacon_data_selected[datetime2>=start_time & datetime2<= end_time,]
      Beacon_data_selected_test[,datetime2 := round_date(datetime2,unit = "30seconds")]
      Beacon_data_selected_test[,datetime:=NULL]
      Beacon_data_selected_test[,datetime3:= NULL]
      #remove PEO for Beacon logger location
      Beacon_data_selected_test = subset(Beacon_data_selected_test, monitor_env != "PEO")
      #check the closet Beacon logger for each Beacon
      Beacon_data_selected_test[,RSSI_max := max(RSSI), by = c("datetime2","MAC")]
      Beacon_data_selected_test[RSSI_max != RSSI,RSSI_max := NA]
      Beacon_data_selected_test[!is.na(RSSI_max) ,closest_logger_location := monitor_env]
      Beacon_data_selected_test2 = Beacon_data_selected_test[!is.na(RSSI_max),]
      Beacon_data_selected_test2[,true_logger_location := test_logger_location]
      Beacon_data_selected_test2[true_logger_location == "KAP",true_logger_location := "KAP1"]
      
      
      for(i in 1: length(unique(Beacon_data_selected_test2$Beacon_ID))){
            total_minute = 
                  nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i],])
            if(total_minute ==0){
                  message("Zero minute on Beacon walk through")
            }
            if(total_minute >0){
                  ture_minute = nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i]&
                                                                      closest_logger_location==true_logger_location,])
                  Beacon_location = unique(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i],Beacon_location])
                  true_location = Beacon_data_selected_test2$true_logger_location[1]
                  true_rate = ture_minute/total_minute
                  KAP1_minute = nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i]&
                                                                      closest_logger_location=="KAP1",])
                  # PEO_minute = nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i]&
                  #                                                    closest_logger_location=="PEO",])
                  SAP_minute = nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i]&
                                                                     closest_logger_location=="SAP",])
                  HOP_minute = nrow(Beacon_data_selected_test2[Beacon_ID == unique(Beacon_data_selected_test2$Beacon_ID)[i]&
                                                                     closest_logger_location=="HOP",])
                  correction_data = cbind(hhid_selected,phase_selected,as.character(start_time),unique(Beacon_data_selected_test2$Beacon_ID)[i],Beacon_location,
                                          true_location,total_minute, ture_minute,true_rate,KAP1_minute,
                                          SAP_minute,HOP_minute)
                  colnames(correction_data) = c("hhid","phase","Start_time","Beacon_ID","Beacon_location","true_location","total_30s","ture_30s",
                                                "true_rate","KAP1_30s","SAP_30s","HOP_30s")
                  correction_data = as.data.table(correction_data)
                  correction_tabel = rbind(correction_tabel,correction_data)
            }
            
            
      }
}