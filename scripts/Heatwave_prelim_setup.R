library(lubridate)
#stats on average daily range for Cultured_Ab loggers

#import the dataset that you want to use, this one has Date+Time, Source & Temp
datalog<-read_csv("Data/Cultured_Abalone_Hobo_Data.csv")

# start date and time of data logging 
startLog<-parse_datetime("2020-07-15 00:00:00", format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# end date and time of data logging
endLog<-parse_datetime("2020-07-31 14:00:00",format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# filter out "test time" data
datalog<-datalog%>%
  filter((Date>=startLog) & (Date<=endLog))

##find max and min temperature per day##
#separate data and time into two separate columns by a space " "
#make two columns, one for min temp and max temp 
datalog<-datalog%>%
  separate(Date,into = c("Date", "Time"), sep = " ", remove = TRUE)%>%
  group_by(Date, Source)%>%
  mutate(Min.t=min(TempC), Max.t=max(TempC))

#remove the column for time, and add a new column for range (max-min) 
datalog<-datalog%>%
  select(-c("Time"))%>%
  mutate(Range=Max.t-Min.t)

#Remove the individual temperature column, and then remove the repeating rows by using the distinct function
datalog<-datalog%>%
  select(-c("TempC"))%>%
  distinct()

#get rid of the AverageTemps, so only have the Hobo Tmp 1 OR Hobo Tmp 2
datalog<-datalog%>%
  filter(Source=="Hobo-Tmp-1"|Source=="Hobo-Tmp-2")

#creae an average range per day column
datalog<-datalog%>%
  group_by(Date)%>%
  mutate(AverageRange=mean(Range))

write.csv(datalog, "datalog.csv")

#stats on average range, average min and average max 
#note that this is
mean(datalog$Range)
#3.550588
mean(datalog$Min.t)
#13.35824
mean(datalog$Max.t)
#19.90882

# 4.0 variability 
# mean ambient temperature was set at 17.5 
# 
# daily fluctuation of 2 degrees C

##plots## 

datalog<-read_csv("Data/Cultured_Abalone_Hobo_Data.csv")

# start date and time of data logging 
startLog<-parse_datetime("2020-07-15 00:00:00", format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# end date and time of data logging
endLog<-parse_datetime("2020-07-31 14:00:00",format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# filter out "test time" data
datalog<-datalog%>%
  filter((Date>=startLog) & (Date<=endLog))

# Plot each logger separately 

plot1<-ggplot(data=datalog, aes(x=Date, y=TempC))+
  geom_line()+
  theme_bw()+
  facet_wrap(ncol=1,~Source, scales="fixed")+ # 'fixed' instead of 'free y'
  labs(x="Date",y="TempC",title="Temperatures from Cultured Abalone Hobo Loggers",subtitle="July 15 through July 31")#+
#ggsave(paste0("Output/",folder.date,"/CulteredAb_Hobo_2week_facet_plot.png"),width=11,height=7)
plot1
plot2<-ggplot(data=datalog, aes(x=Date, y=TempC, colour = Source))+
  geom_line()+
  theme_bw()+
  labs(x="Date",y="TempC",title="Temperatures from Cultured Abalone Hobo Loggers",subtitle="July 15 through July 31")#+
#ggsave(paste0("Output/",folder.date,"/CulteredAb_Hobo_2week_plot.png"),width=11,height=7)
plot2




######## More temperature analysis 
#Script for mean, min & max daily temperatures in mesocosm tanks 
#last edited 08/28/2020

foldername<-'Data/Hobo_CulturedAb/'
folder.date<-'20200903'

#import the dataset that you want to use, this one has Date+Time, Source & Temp
datalog<-read.csv("Data/Hobo_CulturedAb/20200903/Cult_data_03.csv")

datalog
# start date and time of data logging 
startLog<-parse_datetime("2020-08-12 00:00:00",format = "%F %T", na=character(),locale = locale(tz = ""), trim_ws = TRUE)
# end date and time of data logging
endLog<-parse_datetime("2020-09-02 00:00:00",format = "%F %T", na=character(),locale = locale(tz = ""), trim_ws = TRUE)

# filter out "test time" data
datalog<-datalog%>%
  filter((Date>=startLog) & (Date<=endLog))

##find max and min temperature per day##
#separate data and time into two separate columns by a space " "
#make two columns, one for min temp and max temp 
datalog<-datalog%>%
  separate(Date,into = c("Date", "Time"), sep = " ", remove = TRUE)%>%
  group_by(Date, Logger)%>%
  mutate(Min.t=min(TempC), Max.t=max(TempC), Mean.t=mean(TempC))

datalog$Date<-datalog$Date%>%
  parse_date(format = "%Y-%m-%d", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

#remove the column for time, and add a new column for range (max-min) 
datalog<-datalog%>%
  select(-c("Time"))%>%
  mutate(Range=Max.t-Min.t)

#Remove the individual temperature column, and then remove the repeating rows by using the distinct function
datalog<-datalog%>%
  select(-c("TempC"))%>%
  distinct()

#get rid of the AverageTemps, so only have the Hobo Tmp 1 OR Hobo Tmp 2
datalog<-datalog%>%
  filter(Logger=="A1"|Logger=="A2")

#creae an average range per day column
datalog<-datalog%>%
  group_by(Date)%>%
  mutate(AverageRange=mean(Range))

write_csv(datalog, "Data/Hobo_CulturedAb/20200903/Cult_data_03_stats_3weeks.csv")

#stats on average range, average min and average max 

mean(datalog$Mean.t)
#17.14 (past two weeks)
#16.62 (past three weeks)
#15.9 (past month)
#note that this is
mean(datalog$Range)
#3.478 (past two weeks)
#3.444 (past three weeks)
#3.34 (past month)
mean(datalog$Min.t)
#15.57833 (past two weeks)
#15.06 (past three weeks)
#14.5 (past month)
mean(datalog$Max.t)
#19.05633 (past two weeks)
#1.851 (past three weeks)
#17.81 (past month)

#diurnal ambient average min 15.5, 18.5 degrees at peak 

##plots## 

startLog<-parse_datetime("2020-08-19 00:00:00", format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# end date and time of data logging
endLog<-parse_datetime("2020-09-02 00:00:00",format = "%Y-%m-%d %H:%M:%S", na=character(), locale = locale(tz = ""), trim_ws = TRUE)

# filter out "test time" data
datalog<-datalog%>%
  filter((Date>=startLog) & (Date<=endLog))

# Plot each logger separately 
plot1<-ggplot(data = datalog, aes(x=Date, y=Mean.t))+
  geom_line()+
  theme_bw()


plot1<-ggplot(data=datalog, aes(x=Date, y=Max.t))+
  geom_line()+
  theme_bw()+
  facet_wrap(ncol=1,~Logger, scales="fixed")+ # 'fixed' instead of 'free y'
  labs(x="Date",y="TempC",title="Temperatures from Cultured Abalone Hobo Loggers",subtitle="July 15 through July 31")#+
#ggsave(paste0("Output/",folder.date,"/CulteredAb_Hobo_2week_facet_plot.png"),width=11,height=7)
plot1
plot2<-ggplot(data=datalog, aes(x=Date, y=TempC, colour = Source))+
  geom_line()+
  theme_bw()+
  labs(x="Date",y="TempC",title="Temperatures from Cultured Abalone Hobo Loggers",subtitle="July 15 through July 31")#+
#ggsave(paste0("Output/",folder.date,"/CulteredAb_Hobo_2week_plot.png"),width=11,height=7)
plot2

