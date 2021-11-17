# plots that aren't being used for thesis 
#Survivorship 

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
