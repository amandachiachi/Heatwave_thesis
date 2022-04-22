# Respiration Analysis Script 
# Last Edited 4/05/2022
# Load Libraries #### 
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
library('ARPobservation')
library('reshape2')
library('ggpmisc')

# Survivorship Plots ####

mortality_clean<- read_csv(here("data", "mortality_clean.csv"))
mortality<- read_csv(here("data", "mortality_raw.csv"))
# 1s alive, 2 dead 

# statistical analysis 
# to calculate the log-rank test p-value, I use survdiff()
survdiff(Surv(time, status)~treatment, data = mortality_clean)
survdiff(Surv(time, status)~size_class, data = mortality_clean)
survdiff(Surv(time, status)~treatment+size_class, data = mortality_clean)

# I need to figure out how to get my statistical analysis to have treatment * size class.

surv_fit<-coxph(Surv(time, status)~treatment*size_class, data = mortality_clean)
surv_fit
# this is still non significant

# to check the CI of each day for heatwave vs ambient 
sfit <-survfit(Surv(time,status)~treatment, data = mortality)
plot(sfit)
ggsurvplot(sfit)
summary(sfit)

# plot this in ggsurv plot, not individual for heatwave treatment 
# has risk table and p value 
ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Abmient", "Heatwave"), legend.title="Treatment",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Ambient vs. Heatwave Survivorship", 
           risk.table.height=.15)

# to look at the fit with treatment and size class separated
# this also as the log-rank p value included
sfit_all <-survfit(Surv(time,status)~treatment, data = mortality)
plot(sfit_all)
survplot_stats<-ggsurvplot(sfit_all, 
           pval = TRUE, 
           conf.int = TRUE, 
           risk.table = TRUE, 
           pval.method = TRUE,
           size = 1, 
           linetype = "strata", 
           ggtheme = theme_minimal(), 
           palette = c("#523C92", 
                       "#ff9d03"),
           legend = "bottom", 
           legend.title = "",
           xlab = "",
           ylab = "",
           legend.labs = c("Ambient", "Heatwave")) 
survplot_stats
# save plot showing the survivorship of heatwave vs. ambient along with the log-rank p value and number at risk table
ggsave(filename = "output/survplot_stats.png", width = 9, height = 4, dpi = 300)

# Individual Survivorship Plots####
# Size Class 30 
mort_30<- mortality%>%
  filter(size_class == "30")
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_30)
p1<- ggsurvplot(fit1, data = mortality,
                conf.int = TRUE,
                pval = FALSE, 
                fun = "pct", 
                risk.table = FALSE, 
                size = 1, 
                linetype = "strata", 
                palette = c("#523C92", 
                            "#ff9d03"),
                legend = "bottom", 
                legend.title = "",
                xlab = "",
                ylab = "",
                legend.labs = c("Ambient", "Heatwave"))
p1plot <- p1$plot + 
  labs(tags = "A", title = "30mm") + 
  geom_vline(xintercept=0, linetype='dashed', color='black', size=.5)+ 
  annotate("text", x = 0, y = 25, label = "Peak Heatwave", angle = 90, vjust = 1.5)
p1plot

# Size Class 60 
mort_60<- mortality%>%
  filter(size_class == "60")
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_60)
p2<-ggsurvplot(fit1, data = mortality,
               conf.int = TRUE,
               pval =FALSE, 
               fun = "pct", 
               risk.table = FALSE, 
               size = 1, 
               linetype = "strata", 
               palette = c("#523C92", 
                           "#ff9d03"),
               legend = "bottom", 
               legend.title = "", 
               ylab = "Survival probability (%)",
               xlab = "",
               legend.labs = c("Ambient", "Heatwave"),
               axis.text = element_text(size = 12))
p2plot <- p2$plot + 
  labs(tags = "B", title = "60mm")+  
  geom_vline(xintercept=0, linetype='dashed', color='black', size=.5)
p2plot
# Size Class 90
mort_90<- mortality%>%
  filter(size_class == "90")
fit1 <- survfit(Surv(time, status) ~ treatment, data = mort_90)
p3<-ggsurvplot(fit1, data = mortality,
               conf.int = TRUE,
               pval = FALSE, 
               fun = "pct", 
               risk.table = FALSE, 
               size = 1, 
               linetype = "strata", 
               palette = c("#523C92", 
                           "#ff9d03"),
               legend = "bottom", 
               legend.title = "", 
               xlab = "Time (days)",
               ylab = "",
               legend.labs = c("Ambient", "Heatwave"))
p3plot <- p3$plot + 
  labs(tags = "C", title = "90mm")+  
  geom_vline(xintercept=0, linetype='dashed', color='black', size=.5)
p3plot
# Combine three plots (30,60,90) with patchwork
(p1plot/p2plot/p3plot)+ plot_layout( guides = 'collect') & theme(legend.position = "bottom")
ggsave(filename = "output/survplot_all_vert.png", width = 4, height = 10, dpi = 300)



# Size, Mortality & Diet Data ####
# Length Rates Data File
sizedata<- read_csv(here("data", "condition_index.csv"))%>%
  mutate(period = factor(period, levels = c("receiving", "prehw", "posthw", "final")))
# Load Mortality rates data file
mortality<- read_csv(here("data", "mortality_raw.csv"))
# Load Diet trial rates data file 
dietrates<- read_csv(here("data", "diet_raw.csv")) # add the factor as listed above when changed 

# Length, Weight & CI Data Analysis and Plots####
# Data exploration (histograms with shell length, respiration and weight)
sizedata%>%
  ggplot(aes(x = shell_length_cm))+
  geom_histogram()

sizedata%>%
  ggplot(aes(x = weight))+
  geom_histogram()

sizedata%>%
  ggplot(aes(x = CI))+
  geom_histogram()

# % Change Code for Weight, Shell length and CI #
percentchange<- sizedata%>%
  group_by(abalone_ID, treatment, size_class)%>%
  summarise(weightpchange=100*(weight[period=="final"]-weight[period=="prehw"])/weight[period=="prehw"],
            shellpchange=100*(shell_length_cm[period=="final"]-shell_length_cm[period=="prehw"])/shell_length_cm[period=="prehw"], 
            CIpchange=100*CI[period=="final"]-CI[period=="prehw"]/CI[period=="prehw"])

summarypchange<- percentchange%>%
  group_by(size_class, treatment)%>%
  summarise(weight_avgpchange=mean(weightpchange),
            weight_sepchange=sd(weightpchange)/sqrt(n()), 
            shell_avgpchange=mean(shellpchange), 
            shell_sepchange=sd(shellpchange)/sqrt(n()), 
            CI_avgpchange=mean(CIpchange), 
            CI_sepchange=sd(CIpchange)/sqrt(n()))%>%
  drop_na()
view(percentchange)
# 64 individuals that have receiving data through final timepoint 
view(summarypchange)

# Statistical Analysis (ANOVA + Tukey) for change in Weight, Shell Length, and CI ####
# Make treatment and size class each a factor
percentchange$treatment<-as.factor(percentchange$treatment)
percentchange$size_class<-as.factor(percentchange$size_class)

# Weight model
weightmodel<- lm(weightpchange ~size_class*treatment, data= percentchange)
check_model(weightmodel)
shapiro.test(percentchange$weightpchange)
#this is non-normal, but that is because I have three humps?
anova(weightmodel)
weight_model_p<-aov(weightpchange ~ size_class*treatment, data = percentchange)
summary(weight_model_p)
TukeyHSD(weight_model_p, conf.level = .95)

# Length model
lengthmodel<- lm(shellpchange ~size_class*treatment, data = percentchange)
check_model(lengthmodel)
# checked the model and everything looks normal
anova(lengthmodel)
length_model_p<-aov(shellpchange ~ size_class*treatment, data = percentchange)
summary(length_model_p)
TukeyHSD(length_model_p, conf.level = .95)

# CI model
CImodel<- lm(CIpchange ~size_class*treatment, data= percentchange)
check_model(CImodel)
# checked the model and everything looks normal 
anova(CImodel)
CI_model_p<-aov(CIpchange ~ size_class*treatment, data = percentchange)
summary(CI_model_p)
TukeyHSD(CI_model_p, conf.level = .95)

# Plot % Change in Weight, Shell Length, CI ####

percentchange%>%
  ggplot(aes(x = size_class, y = weightpchange, color = treatment))+
  geom_boxplot()
percentchange%>%
  ggplot(aes(x = size_class, y = shellpchange, color = treatment))+
  geom_boxplot()
percentchange%>%
  ggplot(aes(x = size_class, y = CIpchange, color = treatment))+
  geom_boxplot()

view(percentchange)
weight_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = weight_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = weight_avgpchange - weight_sepchange, ymax = weight_avgpchange + weight_sepchange), width = 0.1)+
  geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  annotate("text", x = .8, y = .8, label = "")+
  labs(x = "",
       y = "Change in Weight (%)", 
       tags = "A")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank(), legend.position = 'bottom', 
        plot.tag.position = c(0.02, 1))
weight_plot

## note- if you want to remove size class at the bottom use ,
#axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())+
  
#Length plot 
length_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = shell_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = shell_avgpchange - shell_sepchange, ymax = shell_avgpchange + shell_sepchange), width = 0.1)+
  geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  annotate("text", x = .8, y = .8, label = "")+
  labs(x = "Size Class",
       y = "Change in Length (%)", 
       tags = "B")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank(), legend.position = 'bottom', 
        plot.tag.position = c(0.02, 1))
length_plot

weight_plot+length_plot+ plot_layout(guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/change_weight_length.png", width = 10, height = 5, dpi = 300)

#CI plot
CI_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = CI_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = CI_avgpchange - CI_sepchange, ymax = CI_avgpchange + CI_sepchange), width = 0.1)+
  #geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  #annotate("text", x = .8, y = .8, vjust = 2.5)+
  labs(x = "",
       y = "Change in Condition Index (%)", 
       tags = "C")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank(), 
        plot.tag.position = c(0.02, 1))
CI_plot
ggsave(filename = "output/change_CI.png", width = 6, height = 5, dpi = 300)

#Combine Weight, Length and CI plots 
weight_plot+length_plot+CI_plot + plot_layout(guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/change_weight_length_CI_noresp.png", width = 10, height = 5, dpi = 300)

### Respiration 
# Respiration Data Cleaning (Part 1) ####
# load dataset, this has been compiled by previous respiration code
Respo.R.clean<- read_csv(here("data","Abalone_Respo_Rates_nolog.csv"))
#Remove data with faulty data (turn into NA based on visual of oxygen) 
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
# remove NAs from dataset to consolidate
Respo.R.clean<-Respo.R.clean%>%
  drop_na(mmol.gram.hr)

# Now clean dataset to only include the information we need to anayze and plot 
resporates<- Respo.R.clean%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr, tank_ID)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))
# Respiration Analysis (Sick abalone removed) ####

resporates_healthy<-Respo.R.clean
view(resporates_healthy)
# remove abalone that were close to death, and that died by the final time-point 
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab78_hw_channel7_Trial1_posthw_11_24_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab58_hw_channel9_Trial1_posthw_11_24_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab86_hw_channel6_Trial5_posthw_11_25_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab72_hw_channel4_Trial6_posthw_11_25_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab89_hw_channel4_Trial5_posthw_11_25_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab11_hw_channel3_Trial5_posthw_11_25_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab38_hw_channel1_Trial6_posthw_11_25_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab45_hw_channel8_Trial1_posthw_11_24_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab61_hw_channel6_Trial1_posthw_11_24_20_O2"
is.na(resporates_healthy$mmol.gram.hr) <- resporates_healthy$abalone_number == "ab80_hw_channel10_Trial2_posthw_11_24_20_O2"

resporates_healthy<-resporates_healthy%>%
  drop_na(mmol.gram.hr)
# Total Respiration Analysis 
view(resporates_healthy)
resporates_healthy_final<- resporates_healthy%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr, tank_ID)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))
view(resporates_healthy_final)

# make treatment and size class factors for tukey test 
resporates_healthy_final$treatment<-as.factor(resporates_healthy_final$treatment)
resporates_healthy_final$size_class<-as.factor(resporates_healthy_final$size_class)
#simple anova 
respo_model_clean<-lm(mmol.gram.hr~size_class*treatment*period, data = resporates_healthy_final)
check_model(respo_model_clean)
anova(respo_model_clean)
#anova with tukey post-hoc test
respo_model_clean_2<-aov(mmol.gram.hr ~ size_class*treatment*period, data = resporates_healthy_final)
check_model(respo_model_clean_2)
TukeyHSD(respo_model_clean_2, conf.level = .95)

## Now to plot the new cleaned data with only healthy abalone
resporates_healthy_final$size_name<-with(resporates_healthy_final, factor(size_class, levels = c(30,60,90), 
                                              labels = c("Thirty", "Sixty", "Ninety")))

resporates_healthy_final <- resporates_healthy_final%>%
  mutate(period=case_when(
    period=="prehw"~"1", 
    period=="posthw"~"2", 
    period=="final"~"3"))

view(resporates_healthy_final)
resposummary_clean_1<- resporates_healthy_final%>%
  group_by(period, treatment)%>%
  pivot_wider(names_from = size_name, values_from = mmol.gram.hr)%>%
  summarise(resp_30=mean(Thirty, na.rm = TRUE), 
            resp_30_se=sd(Thirty, na.rm = TRUE/sqrt(n())), 
            resp_60=mean(Sixty, na.rm = TRUE), 
            resp_60_se=sd(Sixty, na.rm = TRUE)/sqrt(n()), 
            resp_90=mean(Ninety, na.rm = TRUE), 
            resp_90_se=sd(Ninety, na.rm = TRUE)/sqrt(n()))
view(resposummary_clean_1)

# Averaged Respiration Rate Plot
Respo_30_average_clean<- resposummary_clean_1%>%
  ggplot(aes(x = factor(period), y = resp_30, color = treatment))+
  geom_point()+
  ylim(20, 40)+
  geom_errorbar(aes(ymin = resp_30 - resp_30_se, ymax = resp_30 + resp_30_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Respiration Rate ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("A                            30mm")+
  theme_light()+# remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 14), 
        plot.title=element_text(hjust = 0))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_30_average_clean

Respo_60_average_clean<- resposummary_clean_1%>%
  ggplot(aes(x = factor(period), y = resp_60, color = treatment))+
  geom_point()+
  ylim(17, 28)+
  geom_errorbar(aes(ymin = resp_60 - resp_60_se, ymax = resp_60 + resp_60_se), width = 0.1)+
  xlab(bquote('Time Period'))+
  ylab(bquote(''))+
  ggtitle("B                            60mm")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 14), 
        plot.title=element_text(hjust = 0))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_60_average_clean

Respo_90_average_clean<- resposummary_clean_1%>%
  ggplot(aes(x = factor(period), y = resp_90, color = treatment))+
  geom_point()+
  ylim(5, 16)+
  geom_errorbar(aes(ymin = resp_90 - resp_90_se, ymax = resp_90 + resp_90_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote(''))+
  ggtitle("C                            90mm")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 14), 
        plot.title=element_text(hjust = 0))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_90_average_clean

Respo_30_average_clean+Respo_60_average_clean+Respo_90_average_clean + plot_layout(guides = 'collect')&theme(legend.position = "bottom")
ggsave(filename = "output/respo_plot_average_clean.png", width = 13, height = 4, dpi = 300)

# Log Respiration Code (Sick abalone removed) #### 
# Size class showing on plot, faceted by timepoint
plotlabels<-data.frame(y = c(0.25, -0.5), x = c(1,1), label = c("Heatwave > Ambient", "Heatwave < Ambient"))
# 
respo_plot_data_healthy<-resporates_healthy_final %>%
  mutate(phase = as.factor(ifelse(treatment=="amb", "base","treat")))%>% # needed for LRR function base is control
  group_split(size_class, period)%>%
  purrr::map_dfr(
    function(x){
      LRR_test<-logRespRatio(x$mmol.gram.hr, phase = x$phase, base_level = "base")
      LNRR<-LRR_test$lRR # log response ratio
      CI.LL <- LRR_test$CI[1] # lower CI
      CI.UL <- LRR_test$CI[2] # upper CI
      period<- x$period[1] # time period
      size_class<-as.factor(x$size_class[1]) # size class
      data.frame(size_class, period, LNRR,CI.LL, CI.UL) # make a dataframe of everything
    }
  )
respo_plot_data_healthy%>%
  ggplot(aes(x = period, y = LNRR))+
  facet_wrap(~size_class)+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(width = 0.1, aes(ymin = CI.LL, ymax = CI.UL),position = position_dodge(0.9)) +
  geom_hline(yintercept = 0, lty = 2)+
  theme_light()+ 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        strip.background = element_rect(color="white", fill="white", size=1.5), 
        strip.text.x = element_text(size = 12, color = "black"))+
  theme(legend.title = element_blank())+
  scale_fill_viridis_d()+
  labs(x = "Time Point",
       y = expression(paste("Respiration Log Ratio", frac(heatwave,control))))+
  geom_label(data = plotlabels, aes(x=x, y=y, label = label), show.legend = FALSE)
ggsave(filename = "output/respo_log_healthy.png", width = 14, height = 5, dpi = 300)


# Cultured abalone pH & temperature - general plot #####
pH_data <- read_csv(here("data", "Cultured_pH.csv"))
view(pH_data)
pH_data$datetimes <- mdy_hm(pH_data$datetimes)
pH_data$datetimes <- as.POSIXct(pH_data$datetimes)

tail(pH_data)
start_cultpH<-as.POSIXct("2021-01-06 12:00:00")
end_cultpH<-as.POSIXct("2021-04-30 12:00:00")
pH_data_clean<- pH_data%>%
  filter(between(datetimes, start_cultpH, end_cultpH))

y_expression_ph<- expression(pH[NBS])
pH_plot <- pH_data_clean%>%
  ggplot(aes(x = datetimes, y = pH))+
  geom_line()+
  labs(x = "2021",
       y = y_expression_ph, 
       tags = "B")+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 17), 
        plot.title=element_text(hjust = 0.5))

pH_plot 

ggsave(filename = "output/Cultured_pH.png", width = 10, height = 5, dpi = 300)

# Cultured abalone temp 
temp_data <- read_csv(here("data", "A1_Cult-SN20565264.csv"))
view(temp_data)
temp_data$datetime <- mdy_hm(temp_data$datetime)
#temp_data$datetime <- as.POSIXct(temp_data$datetime) 

start_culttemp<-as.POSIXct("2020-01-01 12:00:00")
end_culttemp<-as.POSIXct("2020-12-15 12:00:00")
temp_data_clean<- temp_data%>%
  filter(between(datetime, start_culttemp, end_culttemp))

dates_vline <- as.POSIXct("2020-10-22 00:00:00")
dates_vline2 <- as.POSIXct("2020-12-13 00:00:00")
view(temp_data_clean)
temp_plot <- temp_data_clean%>%
  ggplot(aes(x = datetime, y = temp))+
  geom_line()+
  geom_vline(xintercept=dates_vline, linetype = 'dashed', color = 'black', size = .5)+ 
  geom_vline(xintercept=dates_vline2, linetype = 'dashed', color = 'black', size = .5)+
  labs(x = "",
       y = "Temperature (°C)", 
       tags = "A")+
  #scale_x_date(date_labels = "%B")+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 17), 
        plot.title=element_text(hjust = 0.5))
  

temp_plot 

ggsave(filename = "output/Cultured_temp.png", width = 10, height = 5, dpi = 300)

temp_plot|pH_plot + plot_layout(guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/cult_temp_ph.png", width = 12, height = 3, dpi = 300)

## combine size data with respodata
allrates<- resporates_healthy%>%
  left_join(sizedata)

# Data exploration (histograms with shell length, respiration and weight)
allrates%>%
  ggplot(aes(x = shell_length_cm))+
  geom_histogram()

allrates%>%
  ggplot(aes(x = mmol.gram.hr))+
  geom_histogram()

allrates%>%
  ggplot(aes(x = weight))+
  geom_histogram()
# summarizing percent change with respiration 
view(allrates)

weightsum<- allrates%>%
  group_by(tank_ID,period)%>% 
  summarise(Ab_weight_sum = sum(weight)) %>% # get the sum of weights in each tank
  filter(period=="final") # only want final weights

dietrates<-dietrates%>% 
  left_join(weightsum) # creates a column with summed weights by tank 
view(dietrates)
# clean the data
diet_average<-dietrates%>%
  group_by(tank_ID, size_class, diet_treatment, heatwave_treatment, initial_weight, final_weight, AFDW_biomass_per_tank)%>%
  summarise(weight_change = final_weight- initial_weight)%>%
  pivot_wider(names_from = diet_treatment, values_from = weight_change)
mean(diet_average$control, na.rm = T)
# average control reduction in growth is 3.67111, use this for calculating total granms consumed and grams consumed / biomass
dietrates<-dietrates[-c(13),]
view(dietrates)
dietrates_clean<-dietrates%>%
  group_by(tank_ID, size_class, diet_treatment, heatwave_treatment, initial_weight, final_weight, AFDW_biomass_per_tank)%>%
  summarise(weight_change = final_weight - initial_weight)%>%
  pivot_wider(names_from = diet_treatment, values_from = weight_change)%>%
  group_by(tank_ID, size_class, heatwave_treatment, treatment, AFDW_biomass_per_tank)%>%
  summarise(grams_consumed_norm = -3.6711 - treatment)%>%
  drop_na()%>%
  group_by(tank_ID, size_class, heatwave_treatment, grams_consumed_norm, AFDW_biomass_per_tank)%>%
  summarise(grams_consumed_over_biomass = grams_consumed_norm/AFDW_biomass_per_tank)
view(dietrates_clean)

#code for average biomass consumed, didn't finish because one point seems unnecessary
summarychangediet_no<-dietrates_clean%>%
  select(tank_ID, size_class, heatwave_treatment, grams_consumed_over_biomass)%>%
  group_by(size_class)%>%
  pivot_wider(names_from = size_class, values_from = grams_consumed_over_biomass)
view(summarychangediet_no)

summary_diet<- dietrates_clean%>%
  group_by(size_class, heatwave_treatment)%>%
  summarise(diet_avg=mean(grams_consumed_over_biomass),
            diet_se=sd(grams_consumed_over_biomass)/sqrt(n()))
view(summary_diet)

# Analysis for consumption data
# consumption model
view(summary_diet)
consumptionmodel<- lm(diet_avg ~size_class*heatwave_treatment, data= summary_diet)
check_model(consumptionmodel)# did not work
anova(consumptionmodel)
# try with the raw data
consumptionmodel_raw<-lm(grams_consumed_over_biomass ~size_class*heatwave_treatment, data = dietrates_clean)
check_model(consumptionmodel_raw)
# checked the model and everything looks normal 
anova(consumptionmodel_raw)
#tukey here

# Plot Consumption Trial Data (Average+SE)
consumption_plot <- summary_diet%>%
  ggplot(aes(x = factor(size_class), y = diet_avg, color = heatwave_treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = diet_avg - diet_se, ymax = diet_avg + diet_se), width = 0.1)+
  #facet_wrap(~heatwave_treatment)+
  labs(x = "Size Class",
       y = "Average Grams consumed / AFDW Biomass")+ # re-label y label 
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_plot
ggsave(filename = "output/consumption_plot_avg.png", width = 5, height = 5, dpi = 300)


## Consumption vs. Respiration per tank ##

view(resporates)
## Clean the Data

Respo_final_clean<-resporates_healthy_final
Resp_final_avg_clean<-Respo_final_clean%>%
  group_by(tank_ID)%>%
  summarize(average_resp = mean(mmol.gram.hr))
view(Resp_final_avg_clean)
Resp_final_avg_clean<-Resp_final_avg_clean[-c(2,4,6,10,11),]

view(Resp_final_avg_clean)
diet_final_avg<-dietrates
diet_final_avg[diet_final_avg==0]<-NA
diet_final_avg<-diet_final_avg[-c(4),]%>%
  na.omit()
view(diet_final_avg)

Resp_diet_avg_clean<-cbind(Resp_final_avg_clean, diet_final_avg)

Resp_diet_avg_clean<-Resp_diet_avg_clean%>%
  select(-1)

view(Resp_diet_avg_clean)

## Data Anlysis ###
consumption_resp_model<- lm(AFDW_biomass_per_tank~average_resp*heatwave_treatment, data = Resp_diet_avg_clean)
summary(consumption_resp_model)
check_model(consumption_resp_model)
anova(consumption_resp_model)


#tukey here

# Plot With Treatment ##
consumption_resp_plot <- Resp_diet_avg_clean%>%
  ggplot(aes(x = average_resp, y = AFDW_biomass_per_tank, color = heatwave_treatment))+
  geom_smooth(method = lm)+ 
  stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)+
  geom_jitter()+
  #facet_wrap(~heatwave_treatment)+
  xlab(bquote('Average Respiration ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ylab(bquote('Consumption Rate '('gram'['algae ']*  ' gram'['abalone biomass ']^-1*  ' day'^-1)))+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_resp_plot
ggsave(filename = "output/consumption_resp_plot_clean_2.png", width = 8, height = 5, dpi = 300)

## change after this 

###Data Analysis Without Treatment###
consumption_resp_model_all<- lm(AFDW_biomass_per_tank~average_resp, data = Resp_diet_avg_clean)
check_model(consumption_resp_model_all)
anova(consumption_resp_model_all)

# Plot Without Treatment
consumption_resp_plot_all <- Resp_diet_avg_clean%>%
  ggplot(aes(x = average_resp, y = AFDW_biomass_per_tank))+
  geom_smooth(method = lm)+
  geom_jitter()+
  #facet_wrap(~heatwave_treatment)+
  xlab(bquote('Average Respiration ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ylab(bquote('Average Grams Consumed / AFDW Organic Biomass'))+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))
consumption_resp_plot_all
ggsave(filename = "output/consumption_resp_plot_all.png", width = 8, height = 5, dpi = 300)

#random stats 
library(stargazer)
data(mtcars)
resporates_df <- as.data.frame(resporates)
stargazer(resporates_df, type = "text")

reg1 <- lm(shell_length ~ weight, data = sizedata)
reg2 <- lm(shell_length ~ weight + log(CI), data = sizedata)

stargazer(reg1, reg2, type = "text")

###Ash Free Dry Weight Correlation Plot###
afdw_data <- read_csv(here("data", "afdw_correlation.csv"))

view(afdw_data)
afdw_plot <- afdw_data%>%
  ggplot(aes(x = wet_weight, y = dry_weight))+
  geom_smooth(method = lm)+
  geom_jitter()+
  labs(x = "Wet Weight (G)",
       y = "Dry Biomass (AFDW)")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 17), 
        plot.title=element_text(hjust = 0.5))


afdw_plot 
ggsave(filename = "output/afdw_correlation.png", width = 8, height = 5, dpi = 300)


