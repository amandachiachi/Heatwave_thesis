## Average Tank Temperature Per-Treatment Plots ##

# Load libraries#
library('here')
library('tidyverse')
library('lubridate')
library('ggplot2')

# Load Data
here()
view(tank3_amb)
tank3_amb <- read_csv(here("data","TNK-3-SN20565253 2020-12-18 15_04_48 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "3")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank5_amb <- read_csv(here("data","TNK-5-SN20555838 2020-12-18 15_08_13 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "5")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank7_amb <- read_csv(here("data","TNK-7-SN20565255 2020-12-18 15_10_32 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "7")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank9_amb <- read_csv(here("data","TNK-9-SN20565257 2020-12-18 15_12_42 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "9")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank10_amb <- read_csv(here("data","TNK-10-SN20565258 2020-12-18 15_14_11 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "10")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank11_amb <- read_csv(here("data","TNK-11-SN20565259 2020-12-18 15_15_12 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "11")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank14_amb <- read_csv(here("data","TNK-14-SN20565262 2020-12-18 15_19_08 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "14")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank18_amb <- read_csv(here("data","TNK-18-SN20569983 2020-12-18 15_25_54 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "18")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank20_amb <- read_csv(here("data","TNK-20-SN20714143 2020-12-18 15_28_05 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "20")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "amb")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank4_hw <- read_csv(here("data","TNK-4-SN20565252 2020-12-18 15_05_48 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "4")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank6_hw <- read_csv(here("data","TNK-6-SN20565254 2020-12-18 15_09_09 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "6")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank8_hw <- read_csv(here("data","TNK-8-SN20714139 2020-12-18 15_11_30 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "8")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank12_hw <- read_csv(here("data","TNK-12-SN20714140 2020-12-18 15_16_07 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "12")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank13_hw <- read_csv(here("data","TNK-13-SN20565250 2020-12-18 15_17_15 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "13")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank15_hw <- read_csv(here("data","TNK-15-SN20565263 2020-12-18 15_22_06 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "15")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank16_hw <- read_csv(here("data","TNK-16-SN20714141 2020-12-18 15_23_57 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "16")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank17_hw <- read_csv(here("data","TNK-17-SN20565265 2020-12-18 15_24_57 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "17")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

tank19_hw <- read_csv(here("data","TNK-19-SN20714142 2020-12-18 15_27_02 -0800.csv"),skip = 1)%>%
  mutate(tank_number = "19")%>%
  select(-c("Button Down", "Button Up", "Host Connect", "EOF"))%>%
  mutate(treatment = "hw")%>%
  rename(date=contains("Date"),
         TempInSitu=contains("Temp"))

hobo_temps<-rbind(tank3_amb, tank4_hw, tank5_amb, tank6_hw, tank7_amb, tank8_hw, tank9_amb, tank10_amb, tank11_amb, tank12_hw, tank13_hw, tank14_amb, tank15_hw, tank16_hw, tank17_hw, tank18_amb, tank19_hw, tank20_amb)
head(hobo_temps)
start<-as.POSIXct("2020-10-22 00:00:00")
end<-as.POSIXct("2020-12-12 21:00:00") # this is not end of diet trials, only heatwave 
hobo_temps_mean <- hobo_temps %>% group_by(treatment, date) %>%
  summarise(TempInSitu_mean = mean(TempInSitu, na.rm = TRUE), 
            TempInSitu_SE = sd(TempInSitu)/sqrt(n()), na.rm = TRUE)%>%
  filter(between(date, start, end))
view(hobo_temps_mean)

hobo_temps_mean<-hobo_temps_mean%>%
  na.omit()

write_csv(hobo_temps_mean, "data/hobo_temps_mean.csv")

dates_vline3 <- as.POSIXct("2020-10-29 00:00:00")
dates_vline4 <- as.POSIXct("2020-10-31 00:00:00")
dates_vline5 <- as.POSIXct("2020-11-24 00:00:00")
dates_vline6 <- as.POSIXct("2020-11-26 00:00:00")
dates_vline7 <- as.POSIXct("2020-12-11 00:00:00")
dates_vline8 <- as.POSIXct("2020-12-13 00:00:00")

view(hobo_temps_mean)
hobo_temps_mean%>%
  ggplot(aes(x = date, y = TempInSitu_mean, group = treatment, color = treatment))+
  geom_line()+
  xlab(bquote('Date'))+
  ylab(bquote('Temperature (\u00B0C)'))+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14), 
        plot.title=element_text(hjust = 0.5))+
  geom_vline(xintercept=dates_vline3, linetype = 'solid', color = 'black', size = .5)+ 
  geom_vline(xintercept=dates_vline4, linetype = 'solid', color = 'black', size = .5)+
  geom_vline(xintercept=dates_vline5, linetype = 'solid', color = 'black', size = .5)+ 
  geom_vline(xintercept=dates_vline6, linetype = 'solid', color = 'black', size = .5)+
  geom_vline(xintercept=dates_vline7, linetype = 'solid', color = 'black', size = .5)+ 
  geom_vline(xintercept=dates_vline8, linetype = 'solid', color = 'black', size = .5)+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=17))
ggsave(filename = "output/Avg_Treatment_plot.png", width = 10, height = 5, dpi = 300)


## Average Temperatures Based on Treatment and Time Point ####

hobo_temps_clean<-hobo_temps_mean%>%
  select(-c(TempInSitu_SE, na.rm))
view(hobo_temps_clean)
# Pre HW
start_prehw<-as.POSIXct("2020-10-22 00:00:00")
end_prehw<-as.POSIXct("2020-10-30 24:00:00")
hobo_temps_means_pre<- hobo_temps_clean%>%
  group_by(treatment)%>%
  filter(between(date, start_prehw, end_prehw))%>%
  summarise(TempInSitu_mean2 = mean(TempInSitu_mean, na.rm = TRUE), 
            SE = sd(TempInSitu_mean, na.rm=TRUE)/sqrt(n()))
hobo_temps_means_pre

# HW
start_hw<-as.POSIXct("2020-11-06 00:00:00")
end_hw<-as.POSIXct("2020-11-26 00:00:00")
hobo_temps_means_hw<- hobo_temps_mean%>%
  group_by(treatment)%>%
  filter(between(date, start_hw, end_hw))%>%
  summarise(TempInSitu_mean2 = mean(TempInSitu_mean, na.rm = TRUE), 
            SE = sd(TempInSitu_mean, na.rm = TRUE)/sqrt(n()))
hobo_temps_means_hw

# Post Hw (a bit off)
start_posthw<-as.POSIXct("2020-12-04 00:00:00")
end_posthw<-as.POSIXct("2020-12-12 21:00:00")
hobo_temps_means_post<- hobo_temps_mean%>%
  group_by(treatment)%>%
  filter(between(date, start_posthw, end_posthw))%>%
  summarise(TempInSitu_mean2 = mean(TempInSitu_mean, na.rm = TRUE), 
            SE = sd(TempInSitu_mean, na.rm = TRUE)/sqrt(n()))
hobo_temps_means_post


## Averaging pH data by timepoint and treatment ####
pH_salinity_data <- read_csv(here("data/Chiachi_Meso_ph.csv"))
pH_salinity_data$date <- ymd(pH_salinity_data$date)
pH_salinity_data$date <-as.POSIXct(pH_salinity_data$date)

view(pH_salinity_data)
start_prehw_pH<-as.POSIXct("2020-10-22")
end_prehw_pH<-as.POSIXct("2020-10-30")
pH_means_pre<-pH_salinity_data%>%
  group_by(treatment)%>%
filter(between(date, start_prehw_pH, end_prehw_pH))%>%
  summarise(pH_mean = mean(pH, na.rm = TRUE), 
            pH_SE = sd(pH)/sqrt(n()))
pH_means_pre

# HW
start_hw_pH<-as.POSIXct("2020-11-06")
end_hw_pH<-as.POSIXct("2020-11-26")
pH_means_hw<- pH_salinity_data%>%
  group_by(treatment)%>%
  filter(between(date, start_hw_pH, end_hw_pH))%>%
  summarise(pH_mean = mean(pH, na.rm = TRUE), 
            pH_SE = sd(pH)/sqrt(n()))
pH_means_hw

# Post HW
start_posthw_pH<-as.POSIXct("2020-12-04")
end_posthw_pH<-as.POSIXct("2020-12-12")
pH_means_post<- pH_salinity_data%>%
  group_by(treatment)%>%
  filter(between(date, start_posthw_pH, end_posthw_pH))%>%
  summarise(pH_mean = mean(pH, na.rm = TRUE), 
            pH_SE = sd(pH)/sqrt(n()))
pH_means_post


## Averaging Salinity data by timepoint and treatment ####
start_prehw_salinity<-as.POSIXct("2020-10-22")
end_prehw_salinity<-as.POSIXct("2020-10-30")
salinity_means_pre<-pH_salinity_data%>%
  group_by(treatment)%>%
  filter(between(date, start_prehw_salinity, end_prehw_salinity))%>%
  summarise(salinity_mean = mean(Salinity_lab, na.rm = TRUE), 
            Sal_SE = sd(Salinity_lab)/sqrt(n()))
salinity_means_pre

# HW
start_hw_salinity<-as.POSIXct("2020-11-06")
end_hw_salinity<-as.POSIXct("2020-11-26")
salinity_means_hw<- pH_salinity_data%>%
  group_by(treatment)%>%
  filter(between(date, start_hw_salinity, end_hw_salinity))%>%
  summarise(salinity_mean = mean(Salinity_lab, na.rm = TRUE), 
            Sal_SE = sd(Salinity_lab)/sqrt(n()))
salinity_means_hw

# Post HW
start_posthw_salinity<-as.POSIXct("2020-12-04")
end_posthw_salinity<-as.POSIXct("2020-12-12")
salinity_means_post<- pH_salinity_data%>%
  group_by(treatment)%>%
  filter(between(date, start_posthw_salinity, end_posthw_salinity))%>%
  summarise(salinity_mean = mean(Salinity_lab, na.rm = TRUE), 
            Sal_SE = sd(Salinity_lab)/sqrt(n()))
salinity_means_post


## Averaging TA by Timepoint and Treatment ####
TA_data <- read_csv(here("data/TA_all.csv"))
TA_data$date <- ymd(TA_data$date)
TA_data$date <-as.POSIXct(TA_data$date)

# extended timepoint to before peak heatwave (did not have anything previously)
start_prehw_TA<-as.POSIXct("2020-10-22")
end_prehw_TA<-as.POSIXct("2020-11-05")
TA_means_pre<-TA_data%>%
  group_by(treatment)%>%
  filter(between(date, start_prehw_TA, end_prehw_TA))%>%
  summarise(TA_mean = mean(TA, na.rm = TRUE), 
            TA_SE = sd(TA)/sqrt(n()))
TA_means_pre

# HW
start_hw_TA<-as.POSIXct("2020-11-06")
end_hw_TA<-as.POSIXct("2020-11-26")
TA_means_hw<- TA_data%>%
  group_by(treatment)%>%
  filter(between(date, start_hw_TA, end_hw_TA))%>%
  summarise(TA_mean = mean(TA, na.rm = TRUE), 
            TA_SE = sd(TA)/sqrt(n()))
TA_means_hw

# Post HW
start_posthw_TA<-as.POSIXct("2020-12-04")
end_posthw_TA<-as.POSIXct("2020-12-12")
TA_means_post<- TA_data%>%
  group_by(treatment)%>%
  filter(between(date, start_posthw_TA, end_posthw_TA))%>%
  summarise(TA_mean = mean(TA, na.rm = TRUE), 
            TA_SE = sd(TA)/sqrt(n()))
TA_means_post

#### Summary Stats Table ####

library(xtable)

SummaryStats <- matrix(nrow = 8, ncol = 6)
colnames(SummaryStats) <- c("Pre HW","Pre HW SE", "HW", "HW SE", "Post-HW", "Post HW SE")
rownames(SummaryStats) <- c("pH (Amb)", "pH (HW)", 
                            "Total Alkalinity (Amb)", "Total Alkalinity (HW)", 
                            "Salinity (Amb)", "Salinity (HW)", 
                            "Temperature (Amb)", "Temp (HW)")


SummaryStats[1,1] <- pH_means_pre$pH_mean[1]
SummaryStats[2,1] <- pH_means_pre$pH_mean[2]
SummaryStats[1,2] <- pH_means_pre$pH_SE[1]
SummaryStats[2,2] <- pH_means_pre$pH_SE[2]

SummaryStats[1,3] <- pH_means_hw$pH_mean[1]
SummaryStats[2,3] <- pH_means_hw$pH_mean[2]
SummaryStats[1,4] <- pH_means_hw$pH_SE[1]
SummaryStats[2,4] <- pH_means_hw$pH_SE[2]

SummaryStats[1,5] <- pH_means_post$pH_mean[1]
SummaryStats[2,5] <- pH_means_post$pH_mean[2]
SummaryStats[1,6] <- pH_means_post$pH_SE[1]
SummaryStats[2,6] <- pH_means_post$pH_SE[2]

SummaryStats[3,1] <- TA_means_pre$TA_mean[1]
SummaryStats[4,1] <- TA_means_pre$TA_mean[2]
SummaryStats[3,2] <- TA_means_pre$TA_SE[1]
SummaryStats[4,2] <- TA_means_pre$TA_SE[2]

SummaryStats[3,3] <- TA_means_hw$TA_mean[1]
SummaryStats[4,3] <- TA_means_hw$TA_mean[2]
SummaryStats[3,4] <- TA_means_hw$TA_SE[1]
SummaryStats[4,4] <- TA_means_hw$TA_SE[2]

SummaryStats[3,5] <- TA_means_post$TA_mean[1]
SummaryStats[4,5] <- TA_means_post$TA_mean[2]
SummaryStats[3,6] <- TA_means_post$TA_SE[1]
SummaryStats[4,6] <- TA_means_post$TA_SE[2]

SummaryStats[5,1] <- salinity_means_pre$salinity_mean[1]
SummaryStats[6,1] <- salinity_means_pre$salinity_mean[2]
SummaryStats[5,2] <- salinity_means_pre$Sal_SE[1]
SummaryStats[6,2] <- salinity_means_pre$Sal_SE[2]

SummaryStats[5,3] <- salinity_means_hw$salinity_mean[1]
SummaryStats[6,3] <- salinity_means_hw$salinity_mean[2]
SummaryStats[5,4] <- salinity_means_hw$Sal_SE[1]
SummaryStats[6,4] <- salinity_means_hw$Sal_SE[2]


SummaryStats[5,5] <- salinity_means_post$salinity_mean[1]
SummaryStats[6,5] <- salinity_means_post$salinity_mean[2]
SummaryStats[5,6] <- salinity_means_post$Sal_SE[1]
SummaryStats[6,6] <- salinity_means_post$Sal_SE[2]


SummaryStats[7,1] <- hobo_temps_means_pre$TempInSitu_mean[1]
SummaryStats[8,1] <- hobo_temps_means_pre$TempInSitu_mean[2]


SummaryStats[7,3] <- hobo_temps_means_hw$TempInSitu_mean[1]
SummaryStats[8,3] <- hobo_temps_means_hw$TempInSitu_mean[2]


SummaryStats[7,5] <- hobo_temps_means_post$TempInSitu_mean[1]
SummaryStats[8,5] <- hobo_temps_means_post$TempInSitu_mean[2]

# Save as CSV (both data and output)
write.csv(SummaryStats, "output/SummaryStats.csv", row.names = TRUE)



