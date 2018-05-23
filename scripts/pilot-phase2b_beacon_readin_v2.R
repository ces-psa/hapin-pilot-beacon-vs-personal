###Phase 2b Beacon analysis Data Read In
##Include: ECM processed file
#Beacon files
#version 2, using Log sheet provded by field team for Log, Inventory etc.
#add lascar read in
#change ECM, Beacon data as 30seconds interval

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 1: Read in ECM data ####
###ECM analysis
#import_file = file_list[1]
ECM_all = NULL
file_list = list.files(pattern = ".CSV")
ECM_analysis = function(import_file){
      message(paste("processing",import_file) )
      ECM1 = fread(import_file, sep = ",",fill = TRUE,skip = 30, select = c(1:3,5:14), col.names = c("date","time",
                                                                               "RH_corrected_Nep", "Temp", "RH", "Battery","Inlet_p", "Orifice_p","flow",
                                                                               "X_axis","Y_axis","Z_axis","Vector_sum_composite")  )
      ECM1 = ECM1[-nrow(ECM1),]
      #read_in the header
      header = read.csv(import_file, nrows = 24, header = F, stringsAsFactors = F)
      header = as.data.table(header)
      #serial number, two ways to get
      serial_number1 = strsplit(import_file, "_")[[1]][4]
      serial_number2 = as.character(header[V1 == "Device Serial #:",2])
      if(serial_number1 != serial_number2){
            message("Warning on incorrected ECM serial numbers")
            message(import_file)
      }
      
      #hhid, location, phase, filter_id
      hhid = strsplit(import_file, "_")[[1]][1]
      location = strsplit(import_file, "_")[[1]][2]
      phase = strsplit(import_file, "_")[[1]][3]
      filter_id = strsplit(import_file, "_")[[1]][5]
      
      ##Calculate PM mean, Temp mean, RH and Battery minute mean
      ##some different format of date
      if((phase == "02"& hhid == "32155")|(phase == "03" & hhid == "32155")|
         (phase == "03" & hhid == "32141")|(phase == "03" & hhid == "32159")){
            ECM1[,datetime:= dmy_hms(paste(date, time), tz = "America/Guatemala" ) ]      
      }else{
            #datetime
            ECM1[,datetime:= mdy_hms(paste(date, time), tz = "America/Guatemala" ) ]
            
      }
            #take minute of each time
      ECM1[,datetime2 := floor_date(datetime, unit = "30seconds")]
      
      #calculate minute mean
      ECM1[,ECM_PM := mean(as.numeric(RH_corrected_Nep), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_PM),ECM_PM := NA]
      
      ECM1[,ECM_Temp := mean(as.numeric(Temp), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_Temp),ECM_Temp := NA]
      
      ECM1[,ECM_RH := mean(as.numeric(RH), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_RH),ECM_RH := NA]
      
      ECM1[,ECM_Battery := mean(as.numeric(Battery), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_Battery),ECM_Battery := NA]
      
      ECM1[,ECM_inlet_p := mean(as.numeric(Inlet_p), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_inlet_p),ECM_inlet_p := NA]
      
      ECM1[,ECM_orifice_p := mean(as.numeric(Orifice_p), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_orifice_p),ECM_orifice_p := NA]
      
      ECM1[,ECM_flow := mean(as.numeric(flow), na.rm  = T), by= datetime2]
      ECM1[is.nan(ECM_flow),ECM_flow := NA]
      #select the mean average
      ECM2 = unique(ECM1[,c("datetime2","ECM_PM", "ECM_Temp", "ECM_RH", "ECM_Battery",
                            "ECM_inlet_p","ECM_orifice_p","ECM_flow")])
      
      #add the serialnumber, hhid, location, phase, filter_id
      ECM2[,ECM_serialNumber:= serial_number2]
      ECM2[,ECM_location := location]
      ECM2[,hhid := hhid];ECM2[,phase := phase]; ECM2[,filter_id := filter_id]
      #merge all ECM processed file together
      ECM_all <<- rbind(ECM_all, ECM2)
      
}

for(i in 1: length(file_list)){
      ECM_analysis(file_list[i])
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
####Part 2: Read in Beacon data ####

#if the directory is in the folder of this file
getwd()

#read-in all the raw data
#only for cellphone data read-in (applicable for Guatemala)
setwd("../Beacons/")
Beacon_list = list.files(pattern = ".txt")
Beacon_list2 = list.files(pattern=".csv")

#Beacon_file = Beacon_list[1]

Beacon_data = NULL

Beacon_readin = function(Beacon_file){
      message(paste("Processing",Beacon_file))
      if(!is.na(Beacon_file)){
            data = stream_in(file(Beacon_file))
            if(is.null(unlist(data$b)))
                  return(NULL)
            data = tidyr::unnest(data, b)
            data = dplyr::select(data,"t","bid","r") %>%
                  dplyr::rename(datetime=t,MAC=bid,RSSI=r) %>%
                  dplyr::mutate(datetime=datetime/1000)
            data$datetime = as.POSIXct(data$datetime, origin="1970-01-01",tz="UTC")
            data$MAC = as.factor(data$MAC)
            keep = names(tail(sort(table(data$MAC)),4)) #only four Beacons 
            data = subset(data, MAC %in% keep)
            #get hhid, monitoring env, Beacon logger ID, replicate, filterID(day number), startdate
            data$hhid = strsplit(Beacon_file, "_")[[1]][1]
            data$monitor_env = strsplit(Beacon_file, "_")[[1]][2]
            data$replicate = strsplit(Beacon_file, "_")[[1]][3]
            data$Beacon_logger_ID = strsplit(Beacon_file, "_")[[1]][4]
            data$filterID_day_N = strsplit(Beacon_file, "_")[[1]][5]
            data$start_date = strsplit(Beacon_file, "_")[[1]][6]
            Beacon_data <<- rbind(Beacon_data, data)
      }
}
Beacon_readin2 = function(Beacon_file){
      if(!is.na(Beacon_file)){
            message("phone")
            message(paste("Processing",Beacon_file))
            data = as.data.table(read_csv(Beacon_file,
                                          skip=1,col_names =c('datetime','MAC','RSSI'),col_types = "ccd"))
            data$datetime <- as.POSIXct(data$datetime, "%Y-%m-%dT%H:%M:%S", tz="UTC")
            data$MAC <- as.factor(data$MAC)
            keep <-names(tail(sort(table(data$MAC)),4)) #also change to 4
            data <- subset(data, MAC %in% keep)
            
            #get hhid, monitoring env, Beacon logger ID, replicate, filterID(day number), startdate
            data$hhid = strsplit(Beacon_file, "_")[[1]][1]
            data$monitor_env = strsplit(Beacon_file, "_")[[1]][2]
            data$replicate = strsplit(Beacon_file, "_")[[1]][3]
            data$Beacon_logger_ID = strsplit(Beacon_file, "_")[[1]][4]
            data$filterID_day_N = strsplit(Beacon_file, "_")[[1]][5]
            data$start_date = strsplit(Beacon_file, "_")[[1]][6]
            Beacon_data <<- rbind(Beacon_data,data)
      }
      
}

if(length(Beacon_list)!=0){
      for(i in 1:length(Beacon_list)){
            Beacon_readin(Beacon_list[i])
      }  
}

if(length(Beacon_list2) != 0){
      for(i in 1:length(Beacon_list2)){
            Beacon_readin2(Beacon_list2[i])
      } 
}

#change data type
Beacon_data$datetime = as.POSIXct(Beacon_data$datetime, tz = 'UTC')
Beacon_data$MAC = as.character(Beacon_data$MAC)
Beacon_data = as.data.table(Beacon_data)

Beacon_data[,datetime2 := with_tz(datetime, tz = "America/Guatemala")]
Beacon_data = Beacon_data[!is.na(MAC),]
#Change to minute level
Beacon_data2 = Beacon_data
Beacon_data2[,datetime3 := round_date(datetime2, unit = "30seconds")]
Beacon_data2[,RSSI_30s := round(mean(RSSI)), by = c("MAC","Beacon_logger_ID","datetime3")]
Beacon_data2= unique(Beacon_data[,c("datetime3","MAC","hhid","monitor_env","replicate","Beacon_logger_ID","filterID_day_N","start_date","RSSI_30s")])
Beacon_data2 = subset(Beacon_data2, !is.na(Beacon_data2$MAC))

####part 3: Read in Lascar data####
setwd("../LASCAR CO/")
Lascar_list = list.files(pattern = ".txt")
Lascar_data2 = NULL

#Lascar_file = Lascar_list[1]
Lascar_readin = function(Lascar_file){
      message("Processing lascar file ", Lascar_file)
      Lascar_data = fread(Lascar_file, sep=",", skip = 2, select = c("V2","V3"))
      colnames(Lascar_data) = c("datetime","CO_ppm")
      Lascar_data[,datetime := ymd_hms(datetime,tz ="America/Guatemala") ]
      
      header = read.table(Lascar_file, sep = ",", nrows = 2)
      serialnumber = as.character(header[2,4])
      #get the hhid
      hhid = strsplit(Lascar_file, "_")[[1]][1]
      #get the sample type
      Lascar_location= strsplit(Lascar_file, "_")[[1]][2]
      #phase
      phase = strsplit(Lascar_file, "_")[[1]][3]
      #get the lascar CO device ID
      Lascar_id=strsplit(Lascar_file, "_") [[1]][4]
      
      Lascar_data[,hhid := hhid];Lascar_data[,phase:=phase];Lascar_data[,Lascar_id:=Lascar_id]
      Lascar_data[,Lascar_location:= Lascar_location];Lascar_data[,Lascar_SI := serialnumber]
      
      Lascar_data2 <<- rbind(Lascar_data2,Lascar_data)
}

for(i in 1:length(Lascar_list)){
      Lascar_readin(Lascar_list[i])
}




####Part 4: Log, Inventory Read in (Redcap or Excel)####
##Read in redCap in lieu of log sheet
message("Read In RedCap and Excel Spreadsheet data")

redcap_log = fread("../../redCap_analysis/HAPINGuatemala_DATA.csv")

redcap_log2 = redcap_log[,c("record_id","redcap_event_name","h41_visit",
                            "h41_o_phone_id1","h41_o_phone_id2","h41_c_phone_id1","h41_c_phone_id2",
                            "h41_hop_logger_id", "h41_sap_logger_id", "h41_kap1_logger_id", "h41_o_logger_id")]
rm(redcap_log)
redcap_log2 = redcap_log2[!is.na(h41_visit),]
#red cap version of Beacon log
r_Beacon_log = redcap_log2[,c("record_id","redcap_event_name","h41_visit",
                              "h41_o_phone_id1","h41_o_phone_id2",
                              "h41_c_phone_id1","h41_c_phone_id2")]
r_Beacon_log = melt.data.table(r_Beacon_log, value.name = "Beacon_ID",
                               id.vars = c("record_id","redcap_event_name","h41_visit"))
r_Beacon_log[,Beacon_ID := as.character(Beacon_ID)]
r_Beacon_log[grepl("_o_", variable),Beacon_location := "PEO"]
r_Beacon_log[grepl("_c_", variable),Beacon_location := "PEC"]
#red cap version of Beacon logger log
r_Beacon_logger_log = redcap_log2[,c("record_id","redcap_event_name","h41_visit",
                                     "h41_hop_logger_id","h41_sap_logger_id",
                                     "h41_kap1_logger_id","h41_o_logger_id")]
r_Beacon_logger_log = melt.data.table(r_Beacon_logger_log, value.name = "Beacon_logger_ID",
                                      id.vars = c("record_id","redcap_event_name","h41_visit") )
r_Beacon_logger_log[grepl("hop", variable),Beacon_logger_location := "HOP"]
r_Beacon_logger_log[grepl("sap", variable),Beacon_logger_location := "SAP"]
r_Beacon_logger_log[grepl("kap1", variable),Beacon_logger_location := "KAP1"]
r_Beacon_logger_log[grepl("_o_", variable),Beacon_logger_location := "PEO"]
r_Beacon_log[,Beacon_ID := as.numeric(Beacon_ID)]
##read in Beacon inventory
Beacon_inventory =  as.data.table(read_excel("../../../../Inventory/Beacon Inventory_GT.xlsx",
                                             col_types = c("text", "text", "skip",
                                                           "skip", "skip", "text")))
colnames(Beacon_inventory) = c("Beacon_ID","Beacon_MAC","Notes")
Beacon_inventory[,Beacon_ID := as.numeric(Beacon_ID)]
#merge
r_Beacon_log = merge(r_Beacon_log, Beacon_inventory,by = "Beacon_ID", all.x = T, all.y = F )
r_Beacon_log = r_Beacon_log[order(record_id),]

#revise the h41_visit
r_Beacon_log[redcap_event_name =="baseline_1_arm_2",h41_visit := 1]
r_Beacon_log[redcap_event_name =="baseline_2_arm_2",h41_visit := 2]
r_Beacon_log[redcap_event_name =="fu1_arm_2",h41_visit := 3]
r_Beacon_log[redcap_event_name =="fu2_arm_2",h41_visit := 4]

r_Beacon_log[Beacon_ID <10000,Beacon_ID2 := paste("0",as.character(Beacon_ID),sep = "")]
r_Beacon_log[Beacon_ID >10000,Beacon_ID2 := as.character(Beacon_ID)]

r_Beacon_log[,Beacon_ID := Beacon_ID2]
r_Beacon_log[,Beacon_ID2:= NULL]
##New methods
#read-in Beacon deployment from Box file.
# log_data = as.data.table(read_excel("../../../../Log Sheets/Beacon Deployment Log_GT.xlsx"))
# log_data2 = log_data[,c(4,5,6,8:32), with = F]
# log_data2 = log_data2[!is.na(`House ID`),]
# 
# #r_Beacon
# r_Beacon_log = log_data2[,c("House ID","Round","Beacon1ID (Mother)","Beacon2ID (Mother)","Beacon3ID (Child)","Beacon4ID (Child)")]
# r_Beacon_log = melt.data.table(r_Beacon_log, value.name = "Beacon_ID",id.vars = c("House ID","Round"))
# #change name
# r_Beacon_log[grepl("Child", variable),Beacon_location := "PEC"]
# r_Beacon_log[grepl("Mother", variable),Beacon_location := "PEO"]
# colnames(r_Beacon_log) = c("record_id","h41_visit","variable","Beacon_ID","Beacon_location")
# r_Beacon_log[,h41_visit := as.numeric(h41_visit)]
# r_Beacon_log[,variable := NULL]
# ##read in Beacon inventory
# Beacon_inventory =  as.data.table(read_excel("../../../../Inventory/Beacon Inventory_GT.xlsx", 
#                                              col_types = c("text", "text", "skip", 
#                                                            "skip", "skip", "text")))
# colnames(Beacon_inventory) = c("Beacon_ID","Beacon_MAC","Notes")
# 
# #merge
# r_Beacon_log = merge(r_Beacon_log, Beacon_inventory,by = "Beacon_ID", all.x = T, all.y = F )
# r_Beacon_log = r_Beacon_log[order(record_id),]
# 
# #r_Beacon_logger
# r_Beacon_logger_log = log_data2[,c("House ID","Round","BeaconLogger1Name","BeaconLogger2Name",
#                                "BeaconLogger3Name","BeaconLogger4Name")]
# r_Beacon_logger_log = melt.data.table(r_Beacon_logger_log,value.name = "Beacon_logger_ID",
#                                       id.vars = c("House ID","Round"))
# r_Beacon_logger_log[grepl("1Name", variable),Beacon_logger_location:= "KAP1"]
# r_Beacon_logger_log[grepl("2Name", variable),Beacon_logger_location:= "SAP"]
# r_Beacon_logger_log[grepl("3Name", variable),Beacon_logger_location:= "HOP"]
# r_Beacon_logger_log[grepl("4Name", variable),Beacon_logger_location:= "PEO"]
# 
# colnames(r_Beacon_logger_log) = c("record_id","h41_visit","variable","Beacon_logger_ID","Beacon_logger_location")
# r_Beacon_logger_log[,h41_visit := as.numeric(h41_visit)]
# rm(log_data);rm(log_data2)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#




