library(tidyverse)
library(data.table)


pats_to_track <- fread("data/Item3.10_after.txt")
pats_to_track <- pats_to_track[,"SUBJID"]
SUBJID <- pats_to_track %>% select(SUBJID)


UPDRSIII_TOTAUX <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- SUBJID %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX[-1,]

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX %>% select(SUBJID, TOT_OFF_DRUG_V0, EVAL_MOT_BESTON_V0,
                                              OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX <- data.frame(UPDRSIII_TOTAUX) %>% mutate_each(as.numeric, TOT_OFF_DRUG_V0:ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- UPDRSIII_TOTAUX %>% select(SUBJID, OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- Tot_UPDRS_III_after %>% drop_na() # 503

summary_stats <- sapply(Tot_UPDRS_III_after, function(col) {
  if (is.numeric(col)) {
    m <- mean(col, na.rm = TRUE)
    sdv <- sd(col, na.rm = TRUE)
    med <- median(col, na.rm = TRUE)
    q1 <- quantile(col, 0.25, na.rm = TRUE)
    q3 <- quantile(col, 0.75, na.rm = TRUE)
    return(c(
      Mean_SD = sprintf("%.2f ± %.2f", m, sdv),
      Median = sprintf("%.1f [%.1f-%.1f]", med, q1, q3)
    ))
  } else {
    return(c(Mean_SD = NA, Median = NA))
  }
})

summary_stats <- t(summary_stats)
print(summary_stats)


# Reshape data to long format
data_long <- Tot_UPDRS_III_after %>%
  pivot_longer(cols = c(OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(Tot_UPDRS_III_after[, 2:5]))  # Assuming columns 2:5 are conditions

# Friedman rank sum test
# 
# data:  as.matrix(Tot_UPDRS_III_after[, 2:5])
# Friedman chi-squared = 1258.9, df = 3, p-value < 2.2e-16


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")


# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#                    OFF_TOTALCALC_V1 OFFON_TOTALCALC_V1 ON_TOTALCALC_V1
# OFFON_TOTALCALC_V1 < 2e-16          -                  -              
#   ON_TOTALCALC_V1    < 2e-16          < 2e-16            -              
#   ONOFF_TOTALCALC_V1 < 2e-16          7.6e-11            < 2e-16        
# 
# P value adjustment method: bonferroni 


plot1 <- data_long %>% mutate(Condition=ifelse(Condition=="OFF_TOTALCALC_V1", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_TOTALCALC_V1", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_TOTALCALC_V1", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Post-OP Condition",
       y = "MDS-UPDRS III Total Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#6b7280", "#896f7c", "#105067", "#bb3b2c")) +
  scale_fill_manual(values=c("#6b7280", "#896f7c", "#105067", "#bb3b2c")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="updrs3tot_postops.svg", plot=plot1, width=4, height=5)
 

# Deltas 4 conditions post op 

deltas_df <- Tot_UPDRS_III_after %>% mutate(`Med-OFF/DBS-ON`=100*(ONOFF_TOTALCALC_V1-OFF_TOTALCALC_V1)/OFF_TOTALCALC_V1,
                               `Med-ON/DBS-OFF`=100*(OFFON_TOTALCALC_V1 -OFF_TOTALCALC_V1)/OFF_TOTALCALC_V1,
                               `ON/ON`=100*(ON_TOTALCALC_V1-OFF_TOTALCALC_V1)/OFF_TOTALCALC_V1) %>%
  select(SUBJID, `Med-OFF/DBS-ON`,`Med-ON/DBS-OFF`, `ON/ON`)


friedman.test(as.matrix(deltas_df[, 2:4]))  

# 	Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:4])
# Friedman chi-squared = 620.98, df = 2, p-value < 2.2e-16

 deltas_df_long <- deltas_df  %>%
  pivot_longer(cols = c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"),
               names_to = "Condition",
               values_to = "Value") 
 
pairwise.wilcox.test(deltas_df_long$Value, deltas_df_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  deltas_df_long$Value and deltas_df_long$Condition 
# 
#                Med-OFF/DBS-ON Med-ON/DBS-OFF
# Med-ON/DBS-OFF 1.6e-11        -             
# ON/ON          < 2e-16        < 2e-16       
# 
# P value adjustment method: bonferroni 


plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, shape=1, stroke=1) +
  theme_minimal() + 
  ylim(-120, 50) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Post-OP Condition",
       y = "% Drop MDS-UPDRS III Total Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c( "#896f7c", "#105067", "#bb3b2c")) +
  scale_fill_manual(values=c( "#896f7c", "#105067", "#bb3b2c")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="updrs3tot_postops_dorps.svg", plot=plot1, width=3, height=5)








# Before vs After surger 


UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX


UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% select(SUBJID, TOT_OFF_DRUG_V0, OFF_TOTALCALC_V1, EVAL_MOT_BESTON_V0, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% drop_na()


# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(UPDRSIII_TOTAUX_before_vs_after$TOT_OFF_DRUG_V0, UPDRSIII_TOTAUX_before_vs_after$EVAL_MOT_BESTON_V0, paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with
# 	continuity correction
# 
# data:  UPDRSIII_TOTAUX_before_vs_after$TOT_OFF_DRUG_V0 and UPDRSIII_TOTAUX_before_vs_after$EVAL_MOT_BESTON_V0
# V = 121771, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(UPDRSIII_TOTAUX_before_vs_after$OFF_TOTALCALC_V1 , UPDRSIII_TOTAUX_before_vs_after$ON_TOTALCALC_V1    , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with
# 	continuity correction
# 
# data:  UPDRSIII_TOTAUX_before_vs_after$OFF_TOTALCALC_V1 and UPDRSIII_TOTAUX_before_vs_after$ON_TOTALCALC_V1
# V = 121770, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

before_after_off_on <- UPDRSIII_TOTAUX_before_vs_after  %>%
  pivot_longer(cols = c("TOT_OFF_DRUG_V0","OFF_TOTALCALC_V1", "EVAL_MOT_BESTON_V0", "ON_TOTALCALC_V1"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="TOT_OFF_DRUG_V0"|Condition=="EVAL_MOT_BESTON_V0", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="TOT_OFF_DRUG_V0"|Condition=="OFF_TOTALCALC_V1", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
       y = "MDS-UPDRS III Total Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c( "#B0B0B5", "#C96A5F", "#6b7280", "#bb3b2c")) +
  scale_fill_manual(values=c( "#B0B0B5", "#C96A5F", "#6b7280", "#bb3b2c")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="updrs3tot_pre_postops_offon.svg", plot=plot1, width=4, height=5)




library(afex)

anova_res <- aov_ez(
  id = "SUBJID",
  dv = "Value",
  data = before_after_off_on,
  within = c("Time", "State"),
  type = 3
)

anova_res

# Response: Value
#       Effect     df    MSE           F  ges p.value
# 1       Time 1, 492 110.58     7.19 ** .003    .008
# 2      State 1, 492 132.24 3921.24 *** .638   <.001
# 3 Time:State 1, 492  50.24   13.49 *** .002   <.001

library(emmeans)

emm <- emmeans(anova_res, ~ Time * State)

# OFF vs ON within each Time
contrast(emm, method = "pairwise", by = "Time", adjust = "bonferroni")

# Time = V0:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     31.3 0.588 492  53.150  <.0001
# 
# Time = V1:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     33.6 0.628 492  53.511  <.0001
 
# Before vs After within each State
contrast(emm, method = "pairwise", by = "State", adjust = "bonferroni")

# State = OFF:
#  contrast estimate    SE  df t.ratio p.value
#  V0 - V1   -2.4422 0.713 492  -3.424  0.0007
# 
# State = ON:
#  contrast estimate    SE  df t.ratio p.value
#  V0 - V1   -0.0974 0.379 492  -0.257  0.7974






deltas_df <- UPDRSIII_TOTAUX_before_vs_after %>% mutate(`Drop V0`=100*(EVAL_MOT_BESTON_V0-TOT_OFF_DRUG_V0)/TOT_OFF_DRUG_V0,
                               `Drop V1`=100*(ON_TOTALCALC_V1 -OFF_TOTALCALC_V1)/OFF_TOTALCALC_V1) %>%
  select(SUBJID, `Drop V0`,`Drop V1`)


friedman.test(as.matrix(deltas_df[, 2:3]))  


# 	Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:3])
# Friedman chi-squared = 0.89817, df = 1, p-value = 0.3433

deltas_df_long <- deltas_df  %>%
  pivot_longer(cols = c("Drop V0","Drop V1"),
               names_to = "Condition",
               values_to = "Value") 
 
pairwise.wilcox.test(deltas_df_long$Value, deltas_df_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  deltas_df_long$Value and deltas_df_long$Condition 
# 
#         Drop V0
# Drop V1 0.28   
# 
# P value adjustment method: bonferroni 


plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Drop V0","Drop V1"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Drop V0", "Drop V1"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, size=0.5, shape=1, stroke=1) +
  theme_minimal() + 
  #ylim(-120, 50) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Pre|Post Evaluation",
       y = "% Drop MDS-UPDRS III Total Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c(  "#C96A5F", "#bb3b2c")) +
  scale_fill_manual(values=c( "#C96A5F", "#bb3b2c")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="updrs3tot_prepos_dorps.svg", plot=plot1, width=3, height=4)


cor.test(deltas_df$`Drop V0`, deltas_df$`Drop V1`, method="spearman")

# 	Spearman's rank correlation rho
# 
# data:  deltas_df$`Drop V0` and deltas_df$`Drop V1`
# S = 13631082, p-value = 5.281e-13
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3174372 

plot1 <- deltas_df  %>% 
  ggplot(aes(x = `Drop V0`, y = `Drop V1`)) +
  geom_smooth(method="lm", colour= "#105067", fill="#105067" ) +
  geom_jitter(alpha=0.5, size=0.5, shape=1, colour= "#105067", stroke=1) +
  theme_minimal() + 
  ylim(-110, 0) + xlim(-110, 0) +
  labs(x = "\n Pre-OP % Delta",
       y = "Post-OP % Delta \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="updrs3tot_dorps_cors_pre_post.svg", plot=plot1, width=3, height=3)
