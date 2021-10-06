# Respiration Analysis Script 
# Last Edited 10/05/2021
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
library('ggplot2')
library('reshape2')

# Survivorship Plots ####
# Mortality rates data file
mortality<- read_csv(here("data", "mortality_raw.csv"))

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
                legend.labs = c("Ambient", "Heatwave"))
p1plot <- p1$plot + 
  labs(title = "A    30mm") + 
  geom_vline(xintercept=0, linetype='dashed', color='blue', size=1)+ 
  annotate("text", x = 0, y = 25, label = "Peak Heatwave", angle = 90, vjust = 1.5)

p1plot
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
               legend.labs = c("Ambient", "Heatwave"))
p2plot <- p2$plot + labs(title = "B    60mm")+  geom_vline(xintercept=0, linetype='dashed', color='blue', size=1)

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
               legend.labs = c("Ambient", "Heatwave"))
p3plot <- p3$plot + labs(title = "C    90mm")+  geom_vline(xintercept=0, linetype='dashed', color='blue', size=1)

# use patchwork to combine all mortality plots and make them pub quality 
(p1plot/p2plot/p3plot)+ plot_layout( guides = 'collect') & theme(legend.position = "bottom")
ggsave(filename = "output/survplot_all.png", width = 4, height = 10, dpi = 300)


# treatment mortality with all sizes, not using this ?
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
res.cox1 <- coxph(Surv(time, status) ~ treatment, data = mortality)
res.cox1
summary(res.cox1)

res.cox2 <- coxph(Surv(time, status) ~ size_class, data = mortality)
res.cox2
summary(res.cox2)

# multivariate cox regression analysis 
res.cox3 <- coxph(Surv(time, status) ~ treatment+size_class, data = mortality)
summary(res.cox3)
res.cox3

res.cox4<-coxph(formula = Surv(time, status) ~treatment+size_class, data = mortality)


# Respiration Data ####
#Remove data with faulty data (turn into NA based on visual of oxygen) 

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

Respo.R.clean<-Respo.R.clean%>%
  drop_na(mmol.gram.hr)
view(Respo.R.clean)

# Clean respiration rates data file
resporates<- Respo.R.clean%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))
view(resporates)

# Size, Mortality and Diet data ####
sizedata<- read_csv(here("data", "condition_index.csv"))%>%
  mutate(period = factor(period, levels = c("receiving", "prehw", "posthw", "final")))
# Mortality rates data file
mortality<- read_csv(here("data", "mortality_raw.csv"))
# Diet trial rates data file 
dietrates<- read_csv(here("data", "diet_raw.csv")) # add the factor as listed above when changed 

# Bring Data sets together ######

#join respo data sheet with the size data to make it one called allrates
allrates<- resporates %>%
  left_join(sizedata)


# Data exploration (length, respiration and weight) ####

# Check histogram with shell length, respiration rates and weight 
allrates%>%
  ggplot(aes(x = shell_length))+
  geom_histogram()

allrates%>%
  ggplot(aes(x = mmol.gram.hr))+
  geom_histogram()

allrates%>%
  ggplot(aes(x = weight))+
  geom_histogram()

view(allrates)

# Calculate percent change for weight, shell length, respiration, and CI ####

percentchange<- allrates%>%
  group_by(abalone_ID, treatment, size_class)%>%
  summarise(weightpchange=100*(weight[period=="final"]-weight[period=="prehw"])/weight[period=="prehw"],
            shellpchange=100*(shell_length[period=="final"]-shell_length[period=="prehw"])/shell_length[period=="prehw"], 
            respopchange_hw=100*(mmol.gram.hr[period=="posthw"]-mmol.gram.hr[period=="prehw"])/mmol.gram.hr[period=="prehw"],
            respopchange_final=100*(mmol.gram.hr[period=="final"]-mmol.gram.hr[period=="prehw"])/mmol.gram.hr[period=="prehw"], 
            CIpchange=100*CI[period=="final"]-CI[period=="prehw"]/CI[period=="prehw"])

summarypchange<- percentchange%>%
  group_by(size_class, treatment)%>%
  summarise(weight_avgpchange=mean(weightpchange),
            weight_sepchange=sd(weightpchange)/sqrt(n()), 
            shell_avgpchange=mean(shellpchange), 
            shell_sepchange=sd(shellpchange)/sqrt(n()), 
            resp_hw_avgpchange=mean(respopchange_hw),
            resp_hw_sepchange=sd(respopchange_hw)/sqrt(n()), 
            resp_final_avgpchange=mean(respopchange_final),
            resp_final_sepchange=sd(respopchange_final)/sqrt(n()), 
            CI_avgpchange=mean(CIpchange), 
            CI_sepchange=sd(CIpchange)/sqrt(n()))%>%
  drop_na()
view(summarypchange)
# Analysis for change in weight and change in shell length ####
# Weight model
weightmodel<- lm(weightpchange ~size_class*treatment, data= percentchange)
check_model(weightmodel)
# checked the model and everything looks normal 
anova(weightmodel)
#lsmeans(weightmodel, pairwise~size_class*treatment, adjust = "tukey")

# Wength model
lengthmodel<- lm(shellpchange ~size_class*treatment, data = percentchange)
check_model(lengthmodel)
# checked the model and everything looks normal
anova(lengthmodel)
#tukey 

# CI model
CImodel<- lm(CIpchange ~size_class*treatment, data= percentchange)
check_model(CImodel)
# checked the model and everything looks normal 
anova(CImodel)
#tukey 

# Plot weight, length, CI and respiration at two time points ####
#Weight plot 
weight_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = weight_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = weight_avgpchange - weight_sepchange, ymax = weight_avgpchange + weight_sepchange), width = 0.1)+
  geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  annotate("text", x = .8, y = .8, label = "No Change", vjust = 2)+
  labs(x = "",
       y = "Percent Change in Weight")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
weight_plot

#Length plot 
length_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = shell_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = shell_avgpchange - shell_sepchange, ymax = shell_avgpchange + shell_sepchange), width = 0.1)+
  geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  annotate("text", x = .8, y = .8, label = "No Change")+
  labs(x = "",
       y = "Percent Change in Length")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
length_plot

#CI plot
CI_plot <- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = CI_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = CI_avgpchange - CI_sepchange, ymax = CI_avgpchange + CI_sepchange), width = 0.1)+
  #geom_hline(yintercept=0, linetype='dashed', color='black', size=.5)+ 
  #annotate("text", x = .8, y = .8, label = "No Change", vjust = 2.5)+
  xlab(bquote('Size Class'))+
  ylab(bquote('Percent Change in CI'))+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
CI_plot

#Combine Weight, Length and CI plots 
weight_plot/length_plot/CI_plot + plot_layout( guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/change_weight_length_CI.png", width = 5, height = 10, dpi = 300)

# Respiration Plots ####
Respo_plot_hw<- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = resp_hw_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = resp_hw_avgpchange - resp_hw_sepchange, ymax = resp_hw_avgpchange + resp_hw_sepchange), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Change in Respiration ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Pre Heatwave to Heatwave Peak")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_plot_hw

Respo_plot_final<- summarypchange%>%
  ggplot(aes(x = factor(size_class), y = resp_final_avgpchange, color = treatment))+
  geom_point()+
  geom_errorbar(aes(ymin = resp_final_avgpchange - resp_final_sepchange, ymax = resp_final_avgpchange + resp_final_sepchange), width = 0.1)+
  xlab(bquote('Size Class'))+
  ylab(bquote('Average Change in Respiration ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Heatwave Peak to Post Heatwave")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5))+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_plot_final

Respo_plot_hw/Respo_plot_final+ plot_layout( guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/change_respiration.png", width = 5, height = 8, dpi = 300)

#Average respiration rate (not percent change)
resposummary<- resporates%>%
  group_by(size_class, treatment)%>%
  pivot_wider(names_from = period, values_from = mmol.gram.hr)%>%
  summarise(resp_pre=mean(prehw, na.rm = TRUE), 
            resp_pre_se=sd(prehw)/sqrt(n()), 
            resp_post=mean(posthw, na.rm = TRUE), 
            resp_post_se=sd(posthw, na.rm = TRUE)/sqrt(n()), 
            resp_final=mean(final, na.rm = TRUE), 
            resp_final_se=sd(final, na.rm = TRUE)/sqrt(n()))
view(resposummary)

# Averaged Respiration Rate Plot
Respo_pre_average<- resposummary%>%
  ggplot(aes(x = factor(size_class), y = resp_pre, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_pre - resp_pre_se, ymax = resp_pre + resp_pre_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Respiration Rate ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Pre Heatwave")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5))+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.position = "none")
Respo_pre_average

Respo_post_average<- resposummary%>%
  ggplot(aes(x = factor(size_class), y = resp_post, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_post - resp_post_se, ymax = resp_post + resp_post_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Respiration Rate ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Post Heatwave")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.position = "none")
Respo_post_average

Respo_final_average<- resposummary%>%
  ggplot(aes(x = factor(size_class), y = resp_final, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_final - resp_final_se, ymax = resp_final + resp_final_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Respiration Rate ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Final")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_final_average

Respo_pre_average|Respo_post_average|Respo_final_average + plot_layout( guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/respo_plot_average.png", width = 10, height = 5, dpi = 300)

#Raw respiration rate plot faceted by period 
view(resporates)
respo_plot_raw<- resporates%>%
  ggplot(aes(x = factor(size_class), y = mmol.gram.hr, color = treatment))+
  geom_point()+
  facet_wrap(~period)+
  xlab(bquote(''))+
  ylab(bquote('Respiration Rates('*'mmol' ~CO[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Pre Heatwave to Heatwave Peak")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
respo_plot_raw

view(summarypchange)
total_respo_dataset<-summarypchange%>%
  select(size_class, treatment, resp_hw_avgpchange, resp_final_avgpchange, resp_hw_sepchange, resp_final_sepchange)%>%
  pivot_longer(., cols = c(resp_hw_avgpchange, resp_final_avgpchange), names_to = "Timepoint_Resp", values_to = "Resp_Val")%>%
  pivot_longer(., cols = c(resp_hw_sepchange, resp_final_sepchange), names_to = "Timepoint_SE", values_to = "SE_Val")

view(total_respo_dataset)
## Combining both respiration plots together
total_respo_dataset%>%
  ggplot(aes(x = factor(size_class), y = Resp_Val, color = treatment, linetype = Timepoint_Resp))+
  geom_point()+
  geom_errorbar(aes(ymin = Resp_Val - SE_Val, ymax = Resp_Val + SE_Val), width = 0.1)+
  xlab(bquote('Size Class'))+
  ylab(bquote('Average Change in Respiration ('*'mmol' ~CO[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Heatwave Peak to Post Heatwave")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5))+
  scale_color_manual(values = c("#535353", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
view(total_respo_dataset)

# Log Respiration Code #### 

plotlabels<-data.frame(y = c(0.25, -0.5), x = c(1,1), label = c("Respiration of Heatwave > Ambient", "Respiration of Heatwave < Ambient"))
# 
respo_plot_data<-resporates %>%
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
      data.frame(period, size_class, LNRR,CI.LL, CI.UL) # make a dataframe of everything
    }
  )
respo_plot_data%>%
  ggplot(aes(x = size_class, y = LNRR))+
  geom_bar(stat = "identity", position = position_dodge(0.9))+
  geom_errorbar(width = 0.1, aes(ymin = CI.LL, ymax = CI.UL),position = position_dodge(0.9)) +
  geom_hline(yintercept = 0, lty = 2)+
  theme_bw()+
  scale_fill_viridis_d()+
  labs(x = "Size Class",
       y = "Log ratio (heatwave/control)"
  )+
  geom_label(data = plotlabels, aes(x=x, y=y, label = label), show.legend = FALSE)
ggsave(filename = "output/respo_log.png", width = 10, height = 10, dpi = 300)


# data analysis without NA Respiration Data
# Ambient to Heatwave Change in Respiration Analysis 
respo_hw_model<- lm(respopchange_hw~size_class*treatment, data= percentchange)
check_model(respo_hw_model)
# checked the model and everything looks normal 
anova(respo_hw_model)

# Heatwave to Recovery Change in Respiration Analysis 
respo_final_model<- lm(respopchange_final~size_class*treatment, data= percentchange)
check_model(respo_final_model)
# checked the model and everything looks normal 
anova(respo_final_model)

view(resporates)
respo_model<-lm(mmol.gram.hr~size_class*treatment*period, data = resporates)
check_model(respo_model)
anova(respo_model)
# Cultured abalone pH - general plot #####
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

# Consumption Rate Plot and Analysis ####

# Diet Data and Plots ####
# calculate diet data normalized by weight 
# similar to my code that Jenn helped me with
# early code (not using?)
weightsum<- allrates%>%
  group_by(tank_ID,period)%>% 
  summarise(Ab_weight_sum = sum(weight)) %>% # get the sum of weights in each tank
  filter(period=="final") # only want final weights
dietrates<-dietrates%>% 
  left_join(weightsum) # creates a column with summed weights by tank 

# clean the data
diet_average<-dietrates%>%
  group_by(tank_ID, size_class, diet_treatment, heatwave_treatment, initial_weight, final_weight, AFDW_biomass_per_tank)%>%
  summarise(weight_change = final_weight- initial_weight)%>%
  pivot_wider(names_from = diet_treatment, values_from = weight_change)
mean(diet_average$control, na.rm = T)
# average control reduction in growth is 3.67111, use this for calculating total granms consumed and grams consumed / biomass
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

summary_diet<- dietrates_clean%>%
  group_by(size_class, heatwave_treatment)%>%
  summarise(diet_avg=mean(grams_consumed_over_biomass),
            diet_se=sd(grams_consumed_over_biomass)/sqrt(n()))%>%
  drop_na()            
view(summary_diet)

# Analysis for consumption data ####
# consumption model
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
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#a8a8a8", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_plot
ggsave(filename = "output/consumption_plot_avg.png", width = 5, height = 5, dpi = 300)

# Plot Consumption Trial Data (Raw)
consumption_plot <- dietrates_clean%>%
  ggplot(aes(x = factor(size_class), y = grams_consumed_over_biomass, color = heatwave_treatment))+
  geom_point()+
  #facet_wrap(~heatwave_treatment)+
  labs(x = "Size Class",
       y = "Average Grams Consumed / AFDW Organic Biomass")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#a8a8a8", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_plot
ggsave(filename = "output/consumption_plot_raw.png", width = 5, height = 5, dpi = 300)

## Consumption vs. Respiration per tank ##

## Clean the Data
Respo_final<- read_csv(here("data","respo_raw_final_w:tank.csv"))

Resp_final_avg<-Respo_final%>%
  group_by(tank_ID)%>%
  summarize(average_resp = mean(umol.gram.hr))
view(Resp_final_avg)

diet_final_avg<-dietrates
diet_final_avg[diet_final_avg==0]<-NA
diet_final_avg<-diet_final_avg%>%
  na.omit()
view(diet_final_avg)

Resp_diet_avg<-cbind(Resp_final_avg, diet_final_avg)
Resp_diet_avg_clean<-Resp_diet_avg%>%
  select(-1)

view(Resp_diet_avg_clean)

## Data Anlysis ###
consumption_resp_model<- lm(AFDW_biomass_per_tank~average_resp*heatwave_treatment, data = Resp_diet_avg_clean)
check_model(consumption_resp_model)
anova(consumption_resp_model)

#tukey here

# Plot With Treatment ##
consumption_resp_plot <- Resp_diet_avg_clean%>%
  ggplot(aes(x = average_resp, y = AFDW_biomass_per_tank, color = heatwave_treatment))+
  geom_smooth(method = lm)+
  geom_jitter()+
  #facet_wrap(~heatwave_treatment)+
  xlab(bquote('Average Respiration ('*'mmol' ~CO[2]~ gram^-1~hr^-1*')'))+
  ylab(bquote('Average Grams Consumed / AFDW Organic Biomass'))+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#a8a8a8", "#990000"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_resp_plot
ggsave(filename = "output/consumption_resp_plot.png", width = 8, height = 5, dpi = 300)

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
  xlab(bquote('Average Respiration ('*'mmol' ~CO[2]~ gram^-1~hr^-1*')'))+
  ylab(bquote('Average Grams Consumed / AFDW Organic Biomass'))+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))+
  scale_color_manual(values = c("#a8a8a8", "#990000"), labels = c('Ambient', 'Heatwave'))
consumption_resp_plot_all
ggsave(filename = "output/consumption_resp_plot_all.png", width = 8, height = 5, dpi = 300)

