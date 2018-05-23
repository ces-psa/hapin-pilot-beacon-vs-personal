###Beacon walk through test
#Jiawen Mar 21
#revised a little bit May 21

library(data.table)
library(ggplot2)
library(lubridate)
rm(list = ls())

####Beacon walk through test#####
#windows
setwd("C:/Users/jliao8/Box Sync/Box Sync/temp_data/Beacon_children/30_second_May21")
#MAC
setwd("/Users/Jiawen/Box Sync/temp_data/Beacon_children/30_second_May21")

filelist = list.files(pattern = ".csv")
#import_file = filelist[1]
data2 = NULL
data_import = function(import_file){
      data1 = fread(import_file, drop = "V1")
      data2 <<- rbind(data2,data1)
}

for(i in 1:length(filelist)){
      data_import(filelist[i])
}

#analysis
#mean
data2$true_rate = as.numeric(data2$true_rate)
data2$total_30s = as.numeric(data2$total_30s)
data2 = data2[total_30s > 4,]
mean(data2$true_rate)

#by phase
mean(data2[phase=="01",true_rate])
mean(data2[phase=="02",true_rate])
mean(data2[phase=="03",true_rate])
mean(data2[phase=="04",true_rate])

length(unique(data2[phase=="01",hhid]))
length(unique(data2[phase=="02",hhid]))
length(unique(data2[phase=="03",hhid]))
length(unique(data2[phase=="04",hhid]))

#daily analysis
data2[,date:= date(Start_time)]
data2[,daily_true_rate := mean(true_rate), by = date]
data2[,daily_device := length(hhid), by = date]

daily_data = unique(data2[,c("date","daily_device","daily_true_rate")])


ggplot(daily_data,aes(x = as.POSIXct(date), y = daily_true_rate))+
      geom_point(aes(size = daily_device))+
      geom_smooth(method = 'loess')+
      labs(y = "Daily Percentage of Correct Classification", x = "Date" )+
      scale_y_continuous(limits = c(0,1))+
      theme_bw()

#duplicate analysis, also a way to transform the long to wide
data2[,hhid_phase_Beacon_loc := paste(hhid,phase,Beacon_location,true_location,sep = "_")]
data2[,dup := as.numeric(duplicated(hhid_phase_Beacon_loc))]

data3 = data2[,c("true_rate","hhid_phase_Beacon_loc","dup")]

a = dcast(data3, hhid_phase_Beacon_loc~dup ,value.var = "true_rate")
colnames(a) = c("variable","obs_1","obs_2")
ggplot(a, aes(x =obs_1, y = obs_2 ))+
      geom_point()

cor(a$obs_1,a$obs_2,use = "complete.obs",method = "pearson")

summary(lm(a$obs_1~a$obs_2))

#by location
mean(data2[true_location =="KAP1",true_rate])
mean(data2[true_location =="SAP",true_rate])
mean(data2[true_location =="HOP",true_rate])
unique(data2$true_location)


####Indirect and Direct analysis####
rm(list = ls())
#windows
setwd("C:/Users/jliao8/Box Sync/Box Sync/temp_data/Beacon_children/hourly_data_PEO_May21")
#MAC
setwd("/Users/Jiawen/Box Sync/temp_data/Beacon_children/hourly_data_PEO_May21")

filelist = list.files(pattern = ".csv")
#import_file = filelist[1]
data2 = NULL
data_import = function(import_file){
      data1 = fread(import_file, drop = "V1")
      data2 <<- rbind(data2,data1)
}

for(i in 1:length(filelist)){
      data_import(filelist[i])
}

data2 = data2[h_direct >=0,]
#number of households
unique(data2[replicate == "01",hhid])
unique(data2[replicate == "02",hhid])
unique(data2[replicate == "03",hhid])
unique(data2[replicate == "04",hhid])

cor(data2[,h_direct],data2[,h_indirect], use = "complete.obs", method = "spearman")
cor(data2[,h_direct],data2[,h_KAP], use = "complete.obs", method = "spearman")

##using linear regression, i.e. same to pearson coef
summary(lm(data2[,h_direct]~data2[,h_indirect]))
summary(lm(data2[,h_direct]~data2[,h_KAP]))


#baselin line
cor(data2[phase == "01"|phase == "02",h_direct],data2[phase == "01"|phase == "02",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "01"|phase == "02",h_direct],data2[phase == "01"|phase == "02",h_KAP], use = "complete.obs", method = "spearman")

#FU
cor(data2[phase == "03"|phase == "04",h_direct],data2[phase == "03"|phase == "04",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "03"|phase == "04",h_direct],data2[phase == "03"|phase == "04",h_KAP], use = "complete.obs", method = "spearman")

mean(data2[phase == "01"|phase == "02",h_direct])
mean(data2[phase == "03"|phase == "04",h_direct])

#BL1
cor(data2[phase == "01",h_direct],data2[phase == "01",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "01",h_direct],data2[phase == "01",h_KAP], use = "complete.obs", method = "spearman")

#BL2
cor(data2[phase == "02",h_direct],data2[phase == "02",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "02",h_direct],data2[phase == "02",h_KAP], use = "complete.obs", method = "spearman")

#FU1
cor(data2[phase == "03",h_direct],data2[phase == "03",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "03",h_direct],data2[phase == "03",h_KAP], use = "complete.obs", method = "spearman")

#FU2
cor(data2[phase == "04",h_direct],data2[phase == "04",h_indirect], use = "complete.obs", method = "spearman")
cor(data2[phase == "04",h_direct],data2[phase == "04",h_KAP], use = "complete.obs", method = "spearman")



##Bland Altman
#direct vs indirect
data2[,ave_d_in := (h_indirect + h_direct)/2]
data2[,diff_d_in :=h_direct - h_indirect]
hist(data2$diff_d_in)

ggplot(data2, aes(x = ave_d_in, y = diff_d_in,color = phase)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(data2$diff_d_in), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_in) - (1.96 * sd(data2$diff_d_in)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_in) + (1.96 * sd(data2$diff_d_in)), colour = "red", size = 0.5) +
      ylab("Diff. Between Measures") +
      xlab("Average Measure")+
      theme_bw() 
#save data2
write.csv(data2, file = "../hourly_merged.csv")


#direct vs kitchen
data2[,ave_d_k := (h_KAP + h_direct)/2]
data2[,diff_d_k :=h_direct - h_KAP]

ggplot(data2, aes(x = ave_d_k, y = diff_d_k, color = phase)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(data2$diff_d_k), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_k) - (1.96 * sd(data2$diff_d_k)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_k) + (1.96 * sd(data2$diff_d_k)), colour = "red", size = 0.5) +
      ylab("Diff. Between Measures") +
      xlab("Average Measure")+
      theme_bw()


###daily
data2[,daily_id := paste(hhid,replicate,sep = "_")]
data2[,d_indirect := mean(h_indirect), by = daily_id]
data2[,d_direct := mean(h_direct), by = daily_id]
data2[,d_KAP := mean(h_KAP), by = daily_id]
data3 = unique(data2[,c("daily_id","replicate","d_indirect","d_direct","d_KAP")])

cor(data3[,d_direct],data3[,d_indirect])
cor(data3[,d_direct],data3[,d_KAP])

#regression
summary(lm(data3[,d_direct]~data3[,d_indirect]))
summary(lm(data3[,d_direct]~data3[,d_KAP]))


##Bland Altan

data3[,ave_d_in := (d_indirect + d_direct)/2]
data3[,diff_d_in :=d_direct - d_indirect]
hist(data3$diff_d_in)

ggplot(data3, aes(x = ave_d_in, y = diff_d_in, color = replicate)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(data3$diff_d_in), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(data3$diff_d_in) - (1.96 * sd(data3$diff_d_in)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(data3$diff_d_in) + (1.96 * sd(data3$diff_d_in)), colour = "red", size = 0.5) +
      ylab("Diff. Between Measures") +
      xlab("Average Measure")+
      theme_bw()

#log transform
data2[,ave_d_in2 := (log(h_indirect) + log(h_direct))/2]
data2[,diff_d_in2 :=log(h_direct) - log(h_indirect)]

ggplot(data2, aes(x = ave_d_in2, y = diff_d_in2,color = phase)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(data2$diff_d_in2,na.rm = T), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_in2,na.rm = T) - (1.96 * sd(data2$diff_d_in2,na.rm = T)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(data2$diff_d_in2,na.rm = T) + (1.96 * sd(data2$diff_d_in2,na.rm = T)), colour = "red", size = 0.5) +
      ylab("Diff. Between Measures") +
      xlab("Average Measure")+
      theme_bw() 



