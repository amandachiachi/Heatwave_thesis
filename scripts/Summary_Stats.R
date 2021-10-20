## Summary Stats Table ## 

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




