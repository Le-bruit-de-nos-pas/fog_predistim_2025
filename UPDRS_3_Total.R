
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

names(Tot_UPDRS_III_after)

mean(Tot_UPDRS_III_after$OFF_TOTALCALC_V1,na.rm = T)
mean(Tot_UPDRS_III_after$ONOFF_TOTALCALC_V1,na.rm = T)
mean(Tot_UPDRS_III_after$OFFON_TOTALCALC_V1,na.rm = T)
mean(Tot_UPDRS_III_after$ON_TOTALCALC_V1,na.rm = T)


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



data_long %>% group_by(Condition, Value) %>% count() %>%
  ungroup() %>% group_by(Condition) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)

# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )

# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_TOTALCALC_V1", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_TOTALCALC_V1", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_TOTALCALC_V1", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n Post-OP Condition",
       y = "Mean ± SEM \n") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="updrs3tot_postops.svg", plot=plot1, width=3, height=5)






# ************************************
# Before vs After surger *************
# ************************************





UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX

# Define the relevant columns
columns_to_check <- c("ON_15min_Before", "ON_30min_Before", "ON_45min_Before", 
                      "ON_60min_Before", "ON_90min_Before", "ON_120min_Before")


names(UPDRSIII_TOTAUX_before_vs_after)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% select(SUBJID, TOT_OFF_DRUG_V0, OFF_TOTALCALC_V1, EVAL_MOT_BESTON_V0, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% drop_na()

length(unique(UPDRSIII_TOTAUX_before_vs_after$SUBJID)) # 493

names(UPDRSIII_TOTAUX_before_vs_after)

summary_stats <- sapply(UPDRSIII_TOTAUX_before_vs_after, function(col) {
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


# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(UPDRSIII_TOTAUX_before_vs_after$TOT_OFF_DRUG_V0    , UPDRSIII_TOTAUX_before_vs_after$OFF_TOTALCALC_V1   , paired = TRUE, alternative = "two.sided")

# Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSIII_TOTAUX_before_vs_after$TOT_OFF_DRUG_V0 and UPDRSIII_TOTAUX_before_vs_after$OFF_TOTALCALC_V1
# V = 48519, p-value = 0.000957
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(UPDRSIII_TOTAUX_before_vs_after$EVAL_MOT_BESTON_V0 , UPDRSIII_TOTAUX_before_vs_after$ON_TOTALCALC_V1    , paired = TRUE, alternative = "two.sided")

# Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSIII_TOTAUX_before_vs_after$EVAL_MOT_BESTON_V0 and UPDRSIII_TOTAUX_before_vs_after$ON_TOTALCALC_V1
# V = 52126, p-value = 0.8794
# alternative hypothesis: true location shift is not equal to 0


# Reshape the data into long format
long_data <- UPDRSIII_TOTAUX_before_vs_after %>%
  pivot_longer(cols = c(TOT_OFF_DRUG_V0, OFF_TOTALCALC_V1, EVAL_MOT_BESTON_V0, ON_TOTALCALC_V1),
               names_to = c("Condition"),
               values_to = "Value") %>%
  mutate(Group = ifelse(grepl("V0", Condition), "Before", "After"),
         Measure = case_when(
           grepl("OFF_", Condition) ~ "OFF",
           grepl("ON", Condition) ~ "ON"
         ))


# Wilcoxon Signed-Rank Test
wilcox.test(Value ~ Group, data = filter(long_data, Measure == "OFF"), paired = TRUE)

# Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 68851, p-value = 0.000957
# alternative hypothesis: true location shift is not equal to 0
  
wilcox.test(Value ~ Group, data = filter(long_data, Measure == "ON"), paired = TRUE)


# Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 52986, p-value = 0.8794
# alternative hypothesis: true location shift is not equal to 0


# Calculate mean and standard error of the mean (SEM)
summary_stats <- long_data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


long_data %>% group_by(Condition) %>% summarise(mean=mean(Value))

# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="TOT_OFF_DRUG_V0", "OFF pre-op",
                                                   ifelse(Condition=="OFF_TOTALCALC_V1", "OFF post-op",
                                                          ifelse(Condition=="EVAL_MOT_BESTON_V0", "Best-ON pre-op", "Best-ON post-op")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF pre-op", "OFF post-op","Best-ON pre-op", "Best-ON post-op"))) %>%
  ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n Evaluation/Condition",
       y = "Mean ± SEM \n") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#4a4e4d", "#f8dad0", "#0c5776", "#001c44")) +
  scale_fill_manual(values=c("#4a4e4d", "#f8dad0", "#0c5776", "#001c44")) 


ggsave(file="updrs3tot_offsvsons.svg", plot=plot1, width=3, height=5)



names(UPDRSIII_TOTAUX)




UPDRSIII_COMPLET_V3_V5 <- readxl::read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_TOTAUX_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_TOTAUX_v2 <- UPDRSIII_COMPLET_V3_V5 %>% inner_join(UPDRSIII_TOTAUX)

names(UPDRSIII_TOTAUX_v2)
UPDRSIII_TOTAUX_v2 <- UPDRSIII_TOTAUX_v2 %>% select(SUBJID, TOT_OFF_DRUG_V0, EVAL_MOT_BESTON_V0,ON_TOTALCALC_V1 ,ON_TOTAL_V3, ON_TOTAL_V5)

UPDRSIII_TOTAUX_v2 <- UPDRSIII_TOTAUX_v2 %>% drop_na()


UPDRSIII_TOTAUX_v2$TOT_OFF_DRUG_V0  <- as.numeric(UPDRSIII_TOTAUX_v2$TOT_OFF_DRUG_V0 ) 
UPDRSIII_TOTAUX_v2$EVAL_MOT_BESTON_V0   <- as.numeric(UPDRSIII_TOTAUX_v2$EVAL_MOT_BESTON_V0  ) 
UPDRSIII_TOTAUX_v2$ON_TOTALCALC_V1   <- as.numeric(UPDRSIII_TOTAUX_v2$ON_TOTALCALC_V1  ) 
UPDRSIII_TOTAUX_v2$ON_TOTAL_V3   <- as.numeric(UPDRSIII_TOTAUX_v2$ON_TOTAL_V3  ) 
UPDRSIII_TOTAUX_v2$ON_TOTAL_V5  <- as.numeric(UPDRSIII_TOTAUX_v2$ON_TOTAL_V5 ) 


names(UPDRSIII_TOTAUX_v2) <- c("SUBJID",   "OFF_V0", "ON_V0", "ONON_V1", "ONON_V3", "ONON_V5")




# Reshape data to long format
data_long <- UPDRSIII_TOTAUX_v2 %>%
  pivot_longer(cols = c(ONON_V5, ONON_V3, ONON_V1, OFF_V0, ON_V0),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(Brady[, 2:6]))  # Assuming columns 2:5 are conditions



# 	Friedman rank sum test
# 
# data:  as.matrix(Brady[, 2:6])
# Friedman chi-squared = 252.05, df = 4, p-value < 2.2e-16


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#         OFF_V0  ON_V0   ONON_V1 ONON_V3
# ON_V0   < 2e-16 -       -       -      
# ONON_V1 < 2e-16 1.00000 -       -      
# ONON_V3 < 2e-16 1.3e-13 < 2e-16 -      
# ONON_V5 < 2e-16 < 2e-16 < 2e-16 0.00037
# 
# P value adjustment method: bonferroni 


# Create the data as a matrix
mat <- matrix(c(
  "< 2e-16", NA,        NA,        NA,
  "< 2e-16", "1.00000",    NA,        NA,
  "< 2e-16", "1.3e-13 ", "< 2e-16", NA,
  "< 2e-16", "< 2e-16", "< 2e-16", "0.00037"
), nrow = 4, byrow = TRUE)

# Set row and column names
rownames(mat) <- c("ON_V0", "ONON_V1", "ONON_V3", "ONON_V5")
colnames(mat) <- c("OFF_V0", "ON_V0", "ONON_V1", "ONON_V3")

# Convert to data frame and melt
df <- melt(mat, varnames = c("Row", "Col"), value.name = "Pvalue")

# Convert "< 2e-16" to numeric value (e.g., 2e-16), and others as.numeric
df$Pvalue_num <- as.numeric(gsub("< ?2e-16", "2e-16", df$Pvalue))

# Plot heatmap
plot <- ggplot(df, aes(x = Col, y = Row, fill = -log10(Pvalue_num))) +
  geom_tile(color = "white") +
  geom_text(aes(label = Pvalue), size = 4) +
  scale_fill_gradientn(colors = c("white", "#53a5cf", "#1f6ea5", "#024175"),
                       na.value = "grey90",
                       name = "-log10(p)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Multiplicity-adjusted p-values heatmap", x = "", y = "")

ggsave(file="pvals.svg", plot=plot, width=5, height=5)






# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="ON_V0", "V0 UPDRS III ON",
                                                   ifelse(Condition=="OFF_V0", "V0 UPDRS III OFF",
                                                      ifelse(Condition=="ONON_V1", "V1 UPDRS III ON",
                                                           ifelse(Condition=="ONON_V3", "V3 UPDRS III ON", "V5 UPDRS III ON"))))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 UPDRS III OFF", "V0 UPDRS III ON", "V1 UPDRS III ON","V3 UPDRS III ON", "V5 UPDRS III ON"))) %>%
ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n ON Condition",
       y = "Mean ± SEM \n") +
  scale_fill_brewer(palette = "Set1") +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values=c("lightgray", "#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("lightgray", "#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="UPDRS_III_vs.svg", plot=plot1, width=3, height=5)





percent_deltas <- UPDRSIII_TOTAUX %>%
  mutate(percent_after_LD=100*(OFF_TOTALCALC_V1-OFFON_TOTALCALC_V1)/OFF_TOTALCALC_V1) %>%
  mutate(percent_after_DBS=100*(OFF_TOTALCALC_V1-ONOFF_TOTALCALC_V1)/OFF_TOTALCALC_V1) %>%
  mutate(percent_after_Both=100*(OFF_TOTALCALC_V1-ON_TOTALCALC_V1)/OFF_TOTALCALC_V1)  %>%
  select(SUBJID, percent_after_LD, percent_after_DBS, percent_after_Both)

percent_deltas <- UPDRSIII_TOTAUX_before_vs_after %>% 
  mutate(percent_before=100*(TOT_OFF_DRUG_V0-EVAL_MOT_BESTON_V0)/TOT_OFF_DRUG_V0) %>%
  select(SUBJID, percent_before) %>%
  inner_join(percent_deltas)

percent_deltas <- percent_deltas %>% drop_na()

cor.test(percent_deltas$percent_before, percent_deltas$percent_after_LD, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_LD
# S = 15890881, p-value = 2.399e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1895751 
 	
cor.test(percent_deltas$percent_before, percent_deltas$percent_after_DBS, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_DBS
# S = 16495675, p-value = 0.0004202
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.158731 

cor.test(percent_deltas$percent_before, percent_deltas$percent_after_Both, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_Both
# S = 13467246, p-value = 1.303e-12
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3131789 













# # Create data frame
# correlation_data <- data.frame(
#   Outcome = rep(c("MDS-UPDRS", "Axial", "Bradykinesia"), each = 3),
#   Treatment = rep(c("LD-only", "DBS-only", "LD+DBS"), times = 3),
#   Rho = c(0.19, 0.16, 0.31,
#           0.28, 0.18, 0.24,
#           0.18, 0.06, 0.22),
#   p_value = c(2.4e-5, 0.00042, 1.3e-12,
#               4.0e-5, 0.0087, 0.00044,
#               0.00045, 0.24, 1.5e-5)
# )
# 
# # Optional: mark significance
# correlation_data <- correlation_data %>%
#   mutate(Significant = ifelse(p_value < 0.001, "***", 
#                               ifelse(p_value < 0.01, "**",
#                                      ifelse(p_value < 0.05, "*", "ns"))))
# 
# # Plot
# plot <- ggplot(correlation_data, aes(x = Treatment, y = Rho, fill = Treatment)) +
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.9, alpha=0.7) +
#   facet_wrap(~ Outcome) +
#   geom_text(aes(label = Significant), vjust = -0.3, size = 5) +
#   scale_fill_manual(values = c("#f6cd61", "#0e9aa7","#d11141")) +
#   labs(
#     title = "Spearman Correlations Between Pre-Op LCT and Post-Op Response",
#     y = "Spearman's ρ \n",
#     x = "\n Treatment Condition"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# 
# ggsave(file="correlations.svg", plot=plot, width=8, height=6)
