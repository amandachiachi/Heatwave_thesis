
# Respiration Analysis Script 
# Last Edited 07/21/2021


# Load Libraries 

library('here')
library('stringr')
library('performance')
library('see')
library('tidyverse')
library('survival')
library('survminer')
library('ggsci')
library('lubridate')
library('patchwork')

# Bring in data 
# rates, mortality, CI, diet trial

# clean up respiration data my removing those with faulty data
# turn bad data into NA based on visual of oxygen update 
Respo.R.clean<- read_csv(here("data","Abalone_Respo_Rates_nolog.csv"))
view(Respo.R.clean)
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab19_amb_channel3_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab26_amb_channel4_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab31_amb_channel1_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab34_amb_channel2_trial8_final_12_12_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab35_amb_channel4_trial8_final_12_12_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab35_amb_channel6_Trial4_posthw_11_24_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab56_amb_channel5_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab59_amb_channel6_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab63_amb_channel5_trial8_final_12_12_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab73_amb_channel8_trial8_final_12_12_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab81_amb_channel9_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab83_amb_channel3_trial8_final_12_12_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "ab88_amb_channel2_Trial8_posthw_11_25_20_O2"
is.na(Respo.R.clean$mmol.gram.hr) <- Respo.R.clean$abalone_number == "blank_hw_channel7_Trial5_posthw_11_25_20_O2"
# c("each name")
# not sure why 23 is missing a line, check back.

Respo.R.clean<-Respo.R.clean%>%
  drop_na(mmol.gram.hr)

view(Respo.R.clean)

# Respiration rates data file 
resporates<- read_csv(here("data","Respo.R.clean.csv"))%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))

view(resporates)
#ready to plot (below)

# Body Condition Index data file
sizedata<- read_csv(here("data", "condition_index.csv"))%>%
  mutate(period = factor(period, levels = c("receiving", "prehw", "posthw", "final")))
# Mortality rates data file
mortality<- read_csv(here("data", "mortality_raw.csv"))
# Diet trial rates data file 
dietrates<- read_csv(here("data", "diet_raw.csv")) # add the factor as listed above when changed 

###### Bring datasets together ######

#join respo data sheet with the size data to make it one called allrates
allrates<- resporates %>%
  left_join(sizedata)

### Data exploration###

#plot a histogram with weights, respo, shell length 
allrates%>%
  ggplot(aes(x = shell_length))+
  geom_histogram()

#go back to group projects -- goes over how to look at 

### Calculate percent change ###
# shell length, respo, weight 

percentchange<- allrates%>%
  group_by(abalone_ID, treatment, size_class)%>%
  summarise(weightpchange=100*(weight[period=="final"]-weight[period=="prehw"])/weight[period=="prehw"],
            shellpchange=100*(shell_length[period=="final"]-shell_length[period=="prehw"])/shell_length[period=="prehw"], 
            respopchange_hw=100*(mmol.gram.hr[period=="posthw"]-mmol.gram.hr[period=="prehw"])/mmol.gram.hr[period=="prehw"],
            respopchange_final=100*(mmol.gram.hr[period=="final"]-mmol.gram.hr[period=="prehw"])/mmol.gram.hr[period=="prehw"]) 

summarypchange<- percentchange%>%
  group_by(size_class, treatment)%>%
  summarise(weight_avgpchange=mean(weightpchange),
            weight_sepchange=sd(weightpchange)/sqrt(n()), 
            shell_avgpchange=mean(shellpchange), 
            shell_sepchange=sd(shellpchange)/sqrt(n()), 
            resp_hw_avgpchange=mean(respopchange_hw),
            resp_hw_sepchange=sd(respopchange_hw)/sqrt(n()), 
            resp_final_avgpchange=mean(respopchange_final),
            resp_final_sepchange=sd(respopchange_final)/sqrt(n()))%>%
  drop_na()
view(summarypchange)
#### Analysis for change in weight and change in shell length####
# weight model
weightmodel<- lm(weightpchange ~size_class*treatment, data= percentchange)
check_model(weightmodel)
# checked the model and everything looks normal 
anova(weightmodel)
#tukey here
# length model
lengthmodel<- lm(shellpchange ~size_class*treatment, data = percentchange)
check_model(lengthmodel)
# checked the model and everything looks normal
anova(lengthmodel)
#tukey here (show table in supplement for results)

##### Plotting weight, length, and respiration at two time points ####
# weight plot 
weight_plot <- summarypchange%>%
  ggplot(aes(x = size_class, y = weight_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = weight_avgpchange - weight_sepchange, ymax = weight_avgpchange + weight_sepchange), width = 0.2)+
  labs(x = "",
       y = "Average change in weight (g)")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 15))+
  scale_color_manual(values = c("#a8a8a8", "#990000"))

weight_plot

# length plot 
length_plot <- summarypchange%>%
  ggplot(aes(x = size_class, y = shell_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = shell_avgpchange - shell_sepchange, ymax = shell_avgpchange + shell_sepchange), width = 0.2)+
  labs(x = "Size Class",
       y = "Average change in length (mm)")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 15))+
  scale_color_manual(values = c("#a8a8a8", "#990000"))
length_plot
# combine plots 
weight_plot/length_plot + plot_layout( guides = 'collect')
ggsave(filename = "output/change_weight_length.png", width = 12, height = 10, dpi = 300)

# check to see if this is correct - after this, if clean we can write it

#### survivorship curve ####
 mortality_surv <- mortality%>%
  mutate(status=case_when(
    status==1~0, 
    status==0~1))

# treatment mortality with size class 30 
mort_30<- mortality%>%
  filter(size_class == "30")
#view(mort_30)
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_30)
#ggsurv(fit1)
p1<- ggsurvplot(fit1, data = mortality,
           conf.int = TRUE,
           pval = FALSE, 
           fun = "pct", 
           risk.table = FALSE, 
           size = 1, 
           linetype = "strata", 
           palette = c("#a8a8a8", 
                       "#661100"),
           legend = "bottom", 
           legend.title = "",
           xlab = "",
           ylab = "",
           legend.labs = c("ambient", "heatwave"))
p1plot <- p1$plot + labs(title = "A                                   30mm")

# treatment mortality with size class 60 
mort_60<- mortality%>%
  filter(size_class == "60")
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_60)
#ggsurv(fit1)
p2<-ggsurvplot(fit1, data = mortality,
           conf.int = TRUE,
           pval = FALSE, 
           fun = "pct", 
           risk.table = FALSE, 
           size = 1, 
           linetype = "strata", 
           palette = c("#a8a8a8", 
                       "#661100"),
           legend = "bottom", 
           legend.title = "", 
           xlab = "", 
           legend.labs = c("ambient", "heatwave"))
p2plot <- p2$plot + labs(title = "B                                   60mm")

# treatment mortality with size class 90
mort_90<- mortality%>%
  filter(size_class == "90")
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_90)
#ggsurv(fit1)
p3<-ggsurvplot(fit1, data = mortality,
           conf.int = TRUE,
           pval = FALSE, 
           fun = "pct", 
           risk.table = FALSE, 
           size = 1, 
           linetype = "strata", 
           palette = c("#a8a8a8", 
                       "#661100"),
           legend = "bottom", 
           legend.title = "", 
           ylab = "",
           legend.labs = c("ambient", "heatwave"))
p3plot <- p3$plot + labs(title = "C                                   90mm")

# use patchwork to combine all mortality plots and make them pub quality 
(p1plot/p2plot/p3plot)+ plot_layout( guides = 'collect')
ggsave(filename = "output/survplot_all.png", width = 9, height = 10, dpi = 300)


# treatment mortality with all sizes
fit1 <- survfit(Surv(time, status) ~ treatment, data = mortality)
ggsurv(fit1)
ggsurvplot(fit1, data = mortality,
           conf.int = TRUE,
           pval = TRUE, 
           fun = "pct", 
           risk.table = FALSE, 
           size = 1, 
           linetype = "strata", 
           palette = c("#2E9FDF", 
                       "#E7B800"),
           legend = "bottom", 
           legend.title = "", 
           legend.labs = c("ambient", "heatwave"))

# size class mortality 
fit2 <- survfit(Surv(time, status) ~ size_class, data = mortality)
ggsurvplot(fit2, data = mortality,
           conf.int = TRUE,
           pval = TRUE, 
           fun = "pct", 
           risk.table = FALSE, 
           size = 1, 
           linetype = "strata", 
           palette = c("#2E9FDF", 
                       "#E7B800",
                       "#990000"),
           legend = "bottom", 
           legend.title = "", 
           legend.labs = c("30", "60", "90"))

# Survivorship data analysis 
# try the coxph()
res.cox1 <- coxph(Surv(time, status) ~ treatment, data = mortality)
res.cox1
summary(res.cox1)

res.cox2 <- coxph(Surv(time, status) ~ size_class, data = mortality)
res.cox2
summary(res.cox2)

# multivariate cox regression alanysis 
res.cox3 <- coxph(Surv(time, status) ~ treatment+size_class, data = mortality)
summary(res.cox3)

# calculate diet data normalized by weight 
# similar to my code that Jenn helped me with
weightsum<- allrates%>%
  group_by(tank_ID,period)%>% 
  summarise(Ab_weight_sum = sum(weight)) %>% # get the sum of weights in each tank
  filter(period=="final") # only want final weights
dietrates<-dietrates%>% 
  left_join(weightsum) # creates a column with summed weights by tank 

# data analysis 
#### Respo code #### 

# Nyssas code (add)

# data analysis without NA data 

# Cultured abalone pH - general plot
pH_data <- read_csv(here("data", "Cultured_pH.csv"))

pH_data$datetimes <- mdy_hm(pH_data$datetimes)

# filter(pH_data, between(datetimes, as.Date("2020-01-01 12:00:00"), as.Date("2020-05-01 12:00:00"))) 

pH_plot <- pH_data%>%
  ggplot(aes(x = datetimes, y = pH))+
  geom_line()+
  labs(x = "Date and Time",
       y = "pH")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 15))

pH_plot 

