#OISST_data
#Adapted by Amanda Chiachi
#11/13/2019

#### Setup ##########

library(dplyr)
library(ggplot2)
library(heatwaveR)# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(maps)



# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# This function expects the user to provide it with a start and end date
# It then downloads and prepares the data
# we are downloading only the SST data out of several variables 
# spatial extent of latitude 36.4, 37.1 & longitude 237.66, 238.23
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(34.3, 34.4),
                       longitude = c(-119.7, -119.9),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(34.2, 34.4),
                       longitude = c(-119.7, -119.9),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

#server doesnt like more than 9 years of consecutive data 
#creates a data frame to download multiple batches of the data 
# allows us to automate the entire download 
# Date download range by start and end dates per year


santabarbara_years <- data.frame(date_index = 1,
                                 start = as.Date(c("2013-01-01")),
                                 end = as.Date(c("2019-08-29")))
                    

system.time(
  OISST_data <- santabarbara_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) # 38 seconds, ~8 seconds per b


# Visualise data 
OISST_data %>% 
  filter(t == "2019-8-29") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  #borders() +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")


OISST<-OISST_data


# Detect the events in a time series
ts <- ts2clm(OISST, climatologyPeriod = c("2013-01-01", "2019-08-29"))
mhw <- detect_event(ts)

# View just a few metrics
mhw$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_max) %>% 
  head(5)

event_line(mhw, spread = 180, metric = "intensity_max", 
           start_date = "2013-01-01", end_date = "2019-08-29")

## can look at average temperature by day
OISST_average<- OISST %>%
  group_by(t) %>%
  summarise(temp = mean(temp))

# Detect the events in a time series
# "...must be at least three years to calculate thresholds"
# create your time series data 
# ts <- ts2clm(Monterey_data1, climatologyPeriod = mydates)
ts <- ts2clm(OISST_average, climatologyPeriod = c("2013-01-01", "2019-08-29"))
mhw <- detect_event(ts)
# View just a few metrics
mhw$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_max) %>% 
  head(5)

# this is really cool! it shows event duration, date start and peak, intensity max and cumulative intensity

#####Visualizing Marine Heatwaves######
heatwave_plot1<-event_line(mhw, spread = 550, metric = "intensity_max", 
           start_date = "2013-01-01", end_date = "2019-08-29")+
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 17), 
        plot.title=element_text(hjust = 0.5))
#category = TRUE for categories
heatwave_lolli<-lolli_plot(mhw, metric = "intensity_max", xaxis = "date_peak")+
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 17), 
        plot.title=element_text(hjust = 0.5))
(heatwave_plot1|heatwave_lolli)+ theme(legend.position = "bottom")
ggsave(filename = "output/heatwave_plots.png", width = 15, height = 5, dpi = 300)


?lolli_plot



#trial 1

# Detecting EVents
event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = OISST, climatologyPeriod = c("2015-01-01", "2019-08-29"))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

system.time(
  # First we start by choosing the 'OISST' dataframe
  MHW_dplyr <- OISST %>% 
    # Then we group the data by the 'lon' and 'lat' columns
    group_by(lon, lat) %>% 
    # Then we run our MHW detecting function on each group
    group_modify(~event_only(.x))
) # ~123 seconds

OISST_n <- MHW_dplyr %>% 
  mutate(year = lubridate::year(date_start)) %>% 
  group_by(lon, lat, year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(lon, lat) %>%
  tidyr::complete(year = c(2015:2019)) %>% # Note that these dates may differ
  mutate(n = ifelse(is.na(n), 0, n))
head(OISST_n)

lin_fun <- function(ev) {
  mod1 <- glm(n ~ year, family = poisson(link = "log"), data = ev)
  # extract slope coefficient and its p-value
  tr <- data.frame(slope = summary(mod1)$coefficients[2,1],
                   p = summary(mod1)$coefficients[2,4])
  return(tr)
}

OISST_nTrend <- plyr::ddply(OISST_n, c("lon", "lat"), lin_fun, .parallel = T)
OISST_nTrend$pval <- cut(OISST_nTrend$p, breaks = c(0, 0.001, 0.01, 0.05, 1))
head(OISST_nTrend)

map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long)

map_slope <- ggplot(OISST_nTrend, aes(x = lon, y = lat)) +
  geom_rect(size = 0.2, fill = NA,
            aes(xmin = lon - 0.1, xmax = lon + 0.1, ymin = lat - 0.1, ymax = lat + 0.1,
                colour = pval)) +
  geom_raster(aes(fill = slope), interpolate = FALSE, alpha = 0.9) +
  scale_fill_gradient2(name = "count/year (slope)", high = "red", mid = "white",
                       low = "darkblue", midpoint = 0,
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position = "top")) +
  scale_colour_manual(breaks = c("(0,0.001]", "(0.001,0.01]", "(0.01,0.05]", "(0.05,1]"),
                      values = c("firebrick1", "firebrick2", "firebrick3", "white"),
                      name = "p-value", guide = FALSE) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = c(34.3, 34.4), ylim = c(-119.7, -119.9), expand = TRUE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_p <- ggplot(OISST_nTrend, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = pval), interpolate = FALSE) +
  scale_fill_manual(breaks = c("(0,0.001]", "(0.001,0.01]", "(0.01,0.05]",
                               "(0.05,0.1]", "(0.1,0.5]", "(0.5,1]"),
                    values = c("black", "grey20", "grey40",
                               "grey80", "grey90", "white"),
                    name = "p-value",
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top")) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = c(34.3, 34.4), ylim = c(-119.7, -119.9), expand = TRUE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_both <- ggpubr::ggarrange(map_slope, map_p, align = "hv")
map_both
#didnt work 