# plots that aren't being used for thesis 
#Survivorship 
# to make 1s alive and 2 dead 
mortality_surv <- mortality%>%
  mutate(status=case_when(
    status==1~2, 
    status==0~1))

# Survivorship data analysis 
# google how to use interaction terms in a cox proportional hazard analysis 
res.cox1 <- coxph(Surv(time, status) ~ treatment*size_class, data = mortality_surv)
res.cox1
summary(res.cox1)

# multivariate cox regression analysis 
covariates<- c("treatment", "size_class")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = mortality_surv)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
# .99 p value for both treatment and size_class? this seems wrong

res.cox3 <- coxph(Surv(time, status) ~ treatment+size_class, data = mortality_surv)
summary(res.cox3)
res.cox3

ggsurvplot(survfit(res.cox3), data = mortality_surv, color = "#2E9FDF",
           ggtheme = theme_minimal())



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
           palette = c("#741b47", 
                       "#e69138"),
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
                       "#e69138"),
           legend = "bottom", 
           legend.title = "", 
           legend.labs = c("30", "60", "90"))


## before this clean out the respo that you dont need 
# i moved this code down into the respo analysis section 
# Combine these datasheets together and call it allrates
allrates<- resporates %>%
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
percentchange<- allrates%>%
  group_by(abalone_ID, treatment, size_class)%>%
  summarise(weightpchange=100*(weight[period=="final"]-weight[period=="prehw"])/weight[period=="prehw"],
            shellpchange=100*(shell_length_cm[period=="final"]-shell_length_cm[period=="prehw"])/shell_length_cm[period=="prehw"], 
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
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
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
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_plot_final

Respo_plot_hw/Respo_plot_final+ plot_layout( guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/change_respiration.png", width = 5, height = 8, dpi = 300)


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


#Raw respiration rate plot faceted by period 
view(resporates)
respo_plot_raw<- resporates%>%
  ggplot(aes(x = factor(size_class), y = mmol.gram.hr, color = treatment))+
  geom_point()+
  facet_wrap(~period)+
  xlab(bquote(''))+
  ylab(bquote('Respiration Rates('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Pre Heatwave to Heatwave Peak")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
respo_plot_raw

# more respiration, raw
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
  ylab(bquote('Average Change in Respiration ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("Heatwave Peak to Post Heatwave")+
  theme_pubr()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
view(total_respo_dataset)

# consumption plot raw 
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
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
consumption_plot
ggsave(filename = "output/consumption_plot_raw.png", width = 5, height = 5, dpi = 300)


# Respiration analysis & Plots without removing sick abalone####
# make treatment and size class factors for tukey test 
resporates$treatment<-as.factor(resporates$treatment)
resporates$size_class<-as.factor(resporates$size_class)
#simple anova 
respo_model<-lm(mmol.gram.hr~size_class*treatment*period, data = resporates)
check_model(respo_model)
anova(respo_model)
#anova with tukey post-hoc test
respo_model_2<-aov(mmol.gram.hr ~ size_class*treatment*period, data = resporates)
check_model(respo_model_2)
summary(respo_model_2)
TukeyHSD(respo_model_2, conf.level = .95)
# not sure if I did this correctly- 3 way interaction ANOVA?
# remove sick abalone (about to die), from my resporates - save as separate csv file, run this analysis 

resporates<- Respo.R.clean%>%
  select(abalone_number, size_class, treatment, period, mmol.gram.hr, tank_ID)%>%
  mutate(period = factor(period, levels = c("prehw", "posthw", "final")))%>%
  separate(abalone_number, into = "abalone_ID", sep = "_")%>%
  mutate(abalone_ID = as.numeric(str_extract_all(abalone_ID, "[0-9]+")))
view(resporates)
resporates$size_name<-with(resporates, factor(size_class, levels = c(30,60,90), 
                                              labels = c("Thirty", "Sixty", "Ninety")))

resporates <- resporates%>%
  mutate(period=case_when(
    period=="prehw"~"1", 
    period=="posthw"~"2", 
    period=="final"~"3"))

view(resporates)
resposummary1<- resporates%>%
  group_by(period, treatment)%>%
  pivot_wider(names_from = size_name, values_from = mmol.gram.hr)%>%
  summarise(resp_30=mean(Thirty, na.rm = TRUE), 
            resp_30_se=sd(Thirty, na.rm = TRUE/sqrt(n())), 
            resp_60=mean(Sixty, na.rm = TRUE), 
            resp_60_se=sd(Sixty, na.rm = TRUE)/sqrt(n()), 
            resp_90=mean(Ninety, na.rm = TRUE), 
            resp_90_se=sd(Ninety, na.rm = TRUE)/sqrt(n()))
view(resposummary1)

# Averaged Respiration Rate Plot
Respo_30_average<- resposummary1%>%
  ggplot(aes(x = factor(period), y = resp_30, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_30 - resp_30_se, ymax = resp_30 + resp_30_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote(''))+
  ggtitle("30")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_30_average

Respo_60_average<- resposummary1%>%
  ggplot(aes(x = factor(period), y = resp_60, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_60 - resp_60_se, ymax = resp_60 + resp_60_se), width = 0.1)+
  xlab(bquote(''))+
  ylab(bquote('Average Respiration Rate ('*'mmol' ~O[2]~ gram^-1~hr^-1*')'))+
  ggtitle("60")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_60_average

Respo_90_average<- resposummary1%>%
  ggplot(aes(x = factor(period), y = resp_90, color = treatment))+
  geom_point()+
  ylim(5, 40)+
  geom_errorbar(aes(ymin = resp_90 - resp_90_se, ymax = resp_90 + resp_90_se), width = 0.1)+
  xlab(bquote('Time Period'))+
  ylab(bquote(''))+
  ggtitle("90")+
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 12), 
        plot.title=element_text(hjust = 0.5))+
  scale_color_manual(values = c("#523C92", 
                                "#ff9d03"), labels = c('Ambient', 'Heatwave'))+
  theme(legend.title = element_blank())
Respo_90_average

Respo_30_average/Respo_60_average/Respo_90_average + plot_layout(guides = 'collect')& theme(legend.position = "bottom")
ggsave(filename = "output/respo_plot_average.png", width = 3, height = 10, dpi = 300)

# Log Respiration Code without removing sick abalone
# Size class showing on plot, faceted by timepoint
plotlabels<-data.frame(y = c(0.25, -0.5), x = c(1,1), label = c("Heatwave > Ambient", "Heatwave < Ambient"))
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
      data.frame(size_class, period, LNRR,CI.LL, CI.UL) # make a dataframe of everything
    }
  )
respo_plot_data%>%
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
ggsave(filename = "output/respo_log.png", width = 14, height = 5, dpi = 300)


