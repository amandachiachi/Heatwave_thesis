
# Respiration Analysis Script 
# Last Edited 07/21/2021


# Load Libraries 
library('tidyverse')
library('here')
library('stringr')
library('performance')
library('see')

# Bring in data 
# rates, mortality, CI, diet trial

# Respiration rates data file 
resporates<- read_csv(here("data","Abalone_Respo_Rates_nolog.csv"))%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))

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
# outlier!!
which(allrates$shell_length>600)
allrates[101,]
# abalone 46 

#go back to group projects -- goes over how to look at 

### Calculate percent change ###
# shell length, respo, weight 

percentchange<- allrates%>%
  group_by(abalone_ID, treatment, size_class)%>%
  summarise(weightpchange=100*(weight[period=="prehw"]-weight[period=="final"])/weight[period=="prehw"],
            shellpchange=100*(shell_length[period=="prehw"]-shell_length[period=="final"])/shell_length[period=="prehw"], 
            respopchange_hw=100*(mmol.gram.hr[period=="prehw"]-mmol.gram.hr[period=="posthw"])/mmol.gram.hr[period=="prehw"],
            respopchange_final=100*(mmol.gram.hr[period=="prehw"]-mmol.gram.hr[period=="final"])/mmol.gram.hr[period=="prehw"]) 

summarypchange<- percentchange%>%
  group_by(size_class, treatment)%>%
  summarise(weight_avgpchange=mean(weightpchange),
            weight_sepchange=sd(weightpchange)/sqrt(n()), 
            shell_avgpchange=mean(shellpchange), 
            shell_sepchange=sd(shellpchange)/sqrt(n()), 
            resp_hw_avgpchange=mean(respopchange_hw),
            resp_hw_sepchange=sd(respopchange_hw)/sqrt(n()), 
            resp_final_avgpchange=mean(respopchange_final),
            resp_final_sepchange=sd(respopchange_final)/sqrt(n()))
#### Analysis for change in weight and change in shell length####
weightmodel<- lm(weightpchange ~size_class*treatment, data= percentchange)
check_model(weightmodel)
# checked the model and everything looks normal 

anova(weightmodel)
#finish the analysis 

# Now we will make a ggplot, still need to clean up 
# weight plot 
weight_plot <- summarypchange%>%
  ggplot(aes(x = size_class, y = weight_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = weight_avgpchange - weight_sepchange, ymax = weight_avgpchange + weight_sepchange), width = 0.2)

# length plot 
# analysis 

# after this, if clean we can write it

# re-do survivorship curve
# diet data --> normalized by weight 
# allrates group by tank and get the sum of weights -- 
# lefjoin by tank id so you have a column with summed weights by tank 
# (look at code I chatted with with jenn)


weightsum<- allrates%>%
  group_by(tank_ID,period)%>%
  summarise(Ab_weight_sum = sum(weight)) %>%
  filter(period=="final")
dietrates<-dietrates%>%
  left_join(weightsum)

#data analysis 
