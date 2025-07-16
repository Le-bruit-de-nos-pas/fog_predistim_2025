library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)



# ************************************
# Post OP ****************************
# ************************************

sheets_list <- excel_sheets(path = "data/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "


UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)

data.frame(df_names) %>%
  filter(grepl("3.10", df_names))

Item3.10 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, 
                                              OFF_3.10_,ON_3.10_, ON_3.10_1, ON_3.10_2,ON_3.10_3,ON_3.10_4,ON_3.10_5,
                                              OFF_3.10_1,ONOFF_3.10_,OFFON_3.10_, ON_3.10_6)


Item3.10 <- Item3.10[-1,]

names(Item3.10) <- c("SUBJID", "OFF_Before", "ON_15min_Before",
                     "ON_30min_Before","ON_45min_Before","ON_60min_Before",
                     "ON_90min_Before", "ON_120min_Before", "OFF_After",
                     "ONOFF_After", "OFFON_After", "ONON_After")

Item3.10 <- data.frame(Item3.10) %>% mutate_each(as.numeric, OFF_Before:ONON_After)


length(unique(Item3.10$SUBJID)) # 835

dim(Item3.10)[1] * dim(Item3.10)[2]

sum(is.na(Item3.10))

mean(Item3.10$OFF_Before,na.rm = T)
mean(Item3.10$ON_60min_Before,na.rm = T)





Item3.10_after <- Item3.10 %>% select(SUBJID, OFF_After, ONOFF_After, OFFON_After, ONON_After)

dim(Item3.10_after)[1] * dim(Item3.10_after)[2]

sum(is.na(Item3.10_after))

length(unique(Item3.10_after$SUBJID)) # 835

Item3.10_after <- Item3.10_after %>% drop_na()

length(unique(Item3.10_after$SUBJID)) # 520


fwrite(Item3.10_after, "data/Item3.10_after.txt")

mean(Item3.10_after$OFF_After,na.rm = T)
mean(Item3.10_after$ONOFF_After,na.rm = T)
mean(Item3.10_after$OFFON_After,na.rm = T)
mean(Item3.10_after$ONON_After,na.rm = T)


summary_stats <- sapply(Item3.10_after, function(col) {
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
data_long <- Item3.10_after %>%
  pivot_longer(cols = c(OFF_After, ONOFF_After, OFFON_After, ONON_After),
               names_to = "Condition",
               values_to = "Value")

# Shapiro-Wilk normality test for each condition
shapiro.test(Item3.10_after$OFF_After) #W = 0.89277, p-value < 2.2e-16
shapiro.test(Item3.10_after$ONOFF_After) #W = 0.81849, p-value < 2.2e-16
shapiro.test(Item3.10_after$OFFON_After) #W = 0.74309, p-value < 2.2e-16
shapiro.test(Item3.10_after$ONON_After) #W = 0.6335, p-value < 2.2e-16


friedman.test(as.matrix(Item3.10_after[, 2:5]))  # Assuming columns 2:5 are conditions

# 	Friedman rank sum test
# 
# data:  as.matrix(Item3.10_after[, 2:5])
# Friedman chi-squared = 769.01, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#             OFF_After OFFON_After ONOFF_After
# OFFON_After < 2e-16   -           -          
# ONOFF_After < 2e-16   2.4e-13     -          
# ONON_After  < 2e-16   < 2e-16     < 2e-16    
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
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                          ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                 ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
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


ggsave(file="p1.svg", plot=plot1, width=3, height=5)





# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 520) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                          ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                 ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats # %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Post-OP Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.10") +
  scale_fill_brewer(palette = "Blues") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file="p1.svg", plot=plot1, width=4, height=5)

library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik   AIC     niter     max.grad cond.H 
#  logit flexible  2080 -1932.53 3881.07 572(5378) 2.52e-04 8.8e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 7.351    2.711   
# Number of groups:  SUBJID 520 
# 
# Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
# ConditionOFFON_After  -3.5965     0.1716  -20.95   <2e-16 ***
# ConditionONOFF_After  -2.4097     0.1488  -16.20   <2e-16 ***
# ConditionONON_After   -4.7857     0.1990  -24.04   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  -3.2357     0.1849 -17.500
# 1|2   0.1079     0.1602   0.674
# 2|3   3.1916     0.1951  16.359
# 3|4   5.4182     0.2859  18.952


library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# $emmeans
#  Condition   emmean    SE  df asymp.LCL asymp.UCL
#  OFF_After    -1.37 0.160 Inf     -1.68     -1.06
#  OFFON_After  -4.97 0.215 Inf     -5.39     -4.55
#  ONOFF_After  -3.78 0.191 Inf     -4.15     -3.41
#  ONON_After   -6.16 0.241 Inf     -6.63     -5.68
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                  estimate    SE  df z.ratio p.value
#  OFF_After - OFFON_After       3.60 0.172 Inf  20.954  <.0001
#  OFF_After - ONOFF_After       2.41 0.149 Inf  16.196  <.0001
#  OFF_After - ONON_After        4.79 0.199 Inf  24.044  <.0001
#  OFFON_After - ONOFF_After    -1.19 0.147 Inf  -8.086  <.0001
#  OFFON_After - ONON_After      1.19 0.160 Inf   7.424  <.0001
#  ONOFF_After - ONON_After      2.38 0.167 Inf  14.268  <.0001
# 
# P value adjustment: bonferroni method for 6 tests 





# ************************************
# Before vs After surger *************
# ************************************





Item3.10_after

Item3.10_before_vs_after <- Item3.10

# Define the relevant columns
columns_to_check <- c("ON_15min_Before", "ON_30min_Before", "ON_45min_Before", 
                      "ON_60min_Before", "ON_90min_Before", "ON_120min_Before")

# Compute the max value for each SUBJID, ignoring NAs
Item3.10_before_vs_after <- Item3.10_before_vs_after %>%
  mutate(Min_ON_Before = pmin(!!!syms(columns_to_check), na.rm = TRUE))

# Define the relevant columns
columns_to_check <- c("OFFON_After", "ONOFF_After", "ONON_After")

# Compute the max value for each SUBJID, ignoring NAs
Item3.10_before_vs_after <- Item3.10_before_vs_after %>%
  mutate(Min_ON_After = pmin(!!!syms(columns_to_check), na.rm = TRUE))



# View the first few rows
head(Item3.10_before_vs_after)

Item3.10_before_vs_after <- Item3.10_before_vs_after %>% select(SUBJID, OFF_Before, OFF_After, Min_ON_Before, Min_ON_After)

Item3.10_before_vs_after <- Item3.10_before_vs_after %>% drop_na()

length(unique(Item3.10_before_vs_after$SUBJID)) # 527


summary_stats <- sapply(Item3.10_before_vs_after, function(col) {
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






fwrite(Item3.10_before_vs_after, "data/Item3.10_before_vs_after.txt")


# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(Item3.10_before_vs_after$OFF_Before, Item3.10_before_vs_after$OFF_After, paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Item3.10_before_vs_after$OFF_Before and Item3.10_before_vs_after$OFF_After
# V = 23531, p-value = 0.05324
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Item3.10_before_vs_after$Min_ON_Before, Item3.10_before_vs_after$Min_ON_After, paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Item3.10_before_vs_after$Min_ON_Before and Item3.10_before_vs_after$Min_ON_After
# V = 4987.5, p-value = 0.0005452
# alternative hypothesis: true location shift is not equal to 0



# Reshape the data into long format
long_data <- Item3.10_before_vs_after %>%
  pivot_longer(cols = c(OFF_Before, OFF_After, Min_ON_Before, Min_ON_After),
               names_to = c("Condition"),
               values_to = "Value") %>%
  mutate(Group = ifelse(grepl("Before", Condition), "Before", "After"),
         Measure = case_when(
           grepl("OFF_", Condition) ~ "OFF",
           grepl("ON", Condition) ~ "ON"
         ))


# Wilcoxon Signed-Rank Test
wilcox.test(Value ~ Group, data = filter(long_data, Measure == "OFF"), paired = TRUE)
# 
# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 18374, p-value = 0.05324
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Value ~ Group, data = filter(long_data, Measure == "ON"), paired = TRUE)

# 
# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 8873.5, p-value = 0.0005452
# alternative hypothesis: true location shift is not equal to 0


library(ordinal)


off_data <- long_data %>% filter(Measure=="OFF")
on_data <- long_data %>% filter(Measure=="ON")

data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)
off_data$Value <- factor(off_data$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

off_data %>% group_by(Condition) %>% summarise(Value=mean(as.numeric(Value)))

on_data$Value <- factor(on_data$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

on_data %>% group_by(Condition) %>% summarise(Value=mean(as.numeric(Value)))


model <- clmm(Value ~ Condition + (1 | SUBJID), data = off_data, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    off_data
# 
#  link  threshold nobs logLik   AIC     niter     max.grad cond.H 
#  logit flexible  1060 -1393.98 2799.96 412(1868) 1.16e-04 4.1e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 3.682    1.919   
# Number of groups:  SUBJID 530 
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)  
# ConditionOFF_Before   0.2547     0.1223   2.083   0.0372 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1 -2.77253    0.18123 -15.299
# 1|2 -0.05219    0.13135  -0.397
# 2|3  2.90487    0.18519  15.686
# 3|4  4.26876    0.24397  17.497

model <- clmm(Value ~ Condition + (1 | SUBJID), data = off_data, link = "logit", control = clmm.control(method = "ucminf"))
 
summary(model)

# 
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    on_data
# 
#  link  threshold nobs logLik  AIC     niter     max.grad cond.H 
#  logit flexible  1060 -697.64 1405.28 327(1267) 1.25e-05 5.8e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 2.015    1.419   
# Number of groups:  SUBJID 530 
# 
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)   
# ConditionMin_ON_Before  -0.5238     0.1619  -3.237  0.00121 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.2670     0.1569   8.077
# 1|2   4.0796     0.2939  13.880
# 2|3   5.9785     0.5005  11.946

long_data


# Calculate mean and standard error of the mean (SEM)
summary_stats <- long_data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


long_data %>% group_by(Condition) %>% summarise(mean=mean(Value))

# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before", "OFF pre-op",
                                          ifelse(Condition=="OFF_After", "OFF post-op",
                                                 ifelse(Condition=="Min_ON_Before", "Best-ON pre-op", "Best-ON post-op")))) %>%
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


ggsave(file="p1.svg", plot=plot1, width=3, height=5)





# Convert Value to numeric for calculations
long_data$Value <- as.numeric(as.character(long_data$Value))

# Calculate percentage breakdown of each score per condition
percentage_stats <- long_data %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 527) * 100)  # Always using 527 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before", "OFF pre-op",
                                          ifelse(Condition=="OFF_After", "OFF post-op",
                                                 ifelse(Condition=="Min_ON_Before", "Best-ON pre-op", "Best-ON post-op")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF pre-op", "OFF post-op","Best-ON pre-op", "Best-ON post-op"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats # %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Evaluation/Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.10") +
  scale_fill_brewer(palette = "Blues") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file="p2.svg", plot=plot1, width=4, height=5)


