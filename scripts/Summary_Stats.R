## Summary Stats Table ## 

library(xtable)

SummaryStats <- matrix(nrow = 8, ncol = 3)
colnames(SummaryStats) <- c("Pre HW", "During", "Post-HW")
rownames(SummaryStats) <- c("pH (Amb)", "pH (HW)", 
                          "Total Alkalinity (Amb)", "Total Alkalinity (HW)", 
                          "Salinity (Amb)", "Salinity (HW)", 
                          "Temperature (Amb)", "Temp (HW)")

SummaryStats[1,1] <- pH_means_pre$pH_mean[1]
SummaryStats[2,1] <- pH_means_pre$pH_mean[2]
SummaryStats[1,2] <- pH_means_hw$pH_mean[1]
SummaryStats[2,2] <- pH_means_hw$pH_mean[2]
SummaryStats[1,3] <- pH_means_post$pH_mean[1]
SummaryStats[2,3] <- pH_means_post$pH_mean[2]

SummaryStats[3,1] <- TA_means_pre$TA_mean[1]
SummaryStats[4,1] <- TA_means_pre$TA_mean[2]
SummaryStats[3,2] <- TA_means_hw$TA_mean[1]
SummaryStats[4,2] <- TA_means_hw$TA_mean[2]
SummaryStats[3,3] <- TA_means_post$TA_mean[1]
SummaryStats[4,3] <- TA_means_post$TA_mean[2]

SummaryStats[5,1] <- salinity_means_pre$salinity_mean[1]
SummaryStats[6,1] <- salinity_means_pre$salinity_mean[2]
SummaryStats[5,2] <- salinity_means_hw$salinity_mean[1]
SummaryStats[6,2] <- salinity_means_hw$salinity_mean[2]
SummaryStats[5,3] <- salinity_means_post$salinity_mean[1]
SummaryStats[6,3] <- salinity_means_post$salinity_mean[2]

SummaryStats[7,1] <- hobo_temps_means_pre$TempInSitu_mean[1]
SummaryStats[8,1] <- hobo_temps_means_pre$TempInSitu_mean[2]
SummaryStats[7,2] <- hobo_temps_means_hw$TempInSitu_mean[1]
SummaryStats[8,2] <- hobo_temps_means_hw$TempInSitu_mean[2]
SummaryStats[7,3] <- hobo_temps_means_post$TempInSitu_mean[1]
SummaryStats[8,3] <- hobo_temps_means_post$TempInSitu_mean[2]

# I do not know how to save this table
save(SummaryStats, file = "data/SummaryStats.csv")



