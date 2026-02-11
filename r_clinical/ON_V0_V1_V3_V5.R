library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)

UPDRSII_V3_V5 <- read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSII_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSII_V3_V5 <- UPDRSII_V3_V5 %>% select(SUBJID, VISIT, MDS2_12ON, MDS2_13ON)

unique(UPDRSII_V3_V5$VISIT)

UPDRSII_V3_V5 <- UPDRSII_V3_V5[-1,]

UPDRSII_V3_V5 <- UPDRSII_V3_V5 %>% filter(VISIT=="Visite Bilan à 3 ans - V3") %>% select(-VISIT) %>%
  rename("V3_MDS2_12ON"="MDS2_12ON",  "V3_MDS2_13ON"="MDS2_13ON") %>%
  full_join(
    UPDRSII_V3_V5 %>% filter(VISIT=="Visite Bilan à 5 ans - V5") %>% select(-VISIT) %>%
  rename( "V5_MDS2_12ON"="MDS2_12ON",  "V5_MDS2_13ON"="MDS2_13ON")
  )





UPDRSII_ON_2.12 <- fread("data/UPDRSII_ON_2.12.txt")

UPDRSII_2.12 <- UPDRSII_ON_2.12 %>% select(SUBJID, V0_MDS2_12ON,  V1_MDS2_12ON) %>%
  full_join(UPDRSII_V3_V5 %>% select(SUBJID, V3_MDS2_12ON,V5_MDS2_12ON  )) %>%
  drop_na() %>% mutate(V3_MDS2_12ON=as.numeric(V3_MDS2_12ON),V5_MDS2_12ON=as.numeric(V5_MDS2_12ON) ) %>% drop_na()

UPDRSII_OFF_2.12 <- fread("data/UPDRSII_OFF_2.12.txt")





# Reshape data to long format
data_long <- UPDRSII_2.12 %>%
  pivot_longer(cols = c(V0_MDS2_12ON, V1_MDS2_12ON, V3_MDS2_12ON, V5_MDS2_12ON),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSII_2.12[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(UPDRSII_2.12[, 2:5])
# Friedman chi-squared = 19.769, df = 3, p-value = 0.0001895


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#              V0_MDS2_12ON V1_MDS2_12ON V3_MDS2_12ON
# V1_MDS2_12ON 1.0000       -            -           
# V3_MDS2_12ON 1.0000       0.0878       -           
# V5_MDS2_12ON 0.0296       0.0019       0.2473      
# 
# P value adjustment method: bonferroni 


# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_12ON", "V0 2.12 ON",
                                          ifelse(Condition=="V1_MDS2_12ON", "V1 2.12 ON",
                                                 ifelse(Condition=="V3_MDS2_12ON", "V3 2.12 ON", "V5 2.12 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.12 ON", "V1 2.12 ON","V3 2.12 ON", "V5 2.12 ON"))) %>%
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
    scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="mds2.12.svg", plot=plot1, width=3, height=5)


# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

length(unique(data_long$SUBJID))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 23) * 100)  # Always using 23 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_12ON", "V0 2.12 ON",
                                          ifelse(Condition=="V1_MDS2_12ON", "V1 2.12 ON",
                                                 ifelse(Condition=="V3_MDS2_12ON", "V3 2.12 ON", "V5 2.12 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.12 ON", "V1 2.12 ON","V3 2.12 ON", "V5 2.12 ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats 

plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n ON Condition",
       y = "Percentage (%) \n",
       fill = "Item 2.12") +
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


ggsave(file="mds2.12_perc.svg", plot=plot1, width=4, height=5)




library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik AIC    niter     max.grad cond.H 
#  logit flexible  92   -94.76 205.52 419(1158) 3.58e-06 4.7e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 0.5493   0.7412  
# Number of groups:  SUBJID 23 
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)   
# ConditionV1_MDS2_12ON  -0.7724     0.6385  -1.210  0.22643   
# ConditionV3_MDS2_12ON   0.8231     0.5934   1.387  0.16541   
# ConditionV5_MDS2_12ON   2.0436     0.6373   3.207  0.00134 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   0.1550     0.4544   0.341
# 1|2   2.7138     0.5913   4.589
# 2|3   3.8143     0.7020   5.434
# 3|4   5.8154     1.1811   4.924




library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")

# $emmeans
#  Condition    emmean    SE  df asymp.LCL asymp.UCL
#  V0_MDS2_12ON  -3.12 0.611 Inf     -4.32    -1.927
#  V1_MDS2_12ON  -3.90 0.682 Inf     -5.23    -2.560
#  V3_MDS2_12ON  -2.30 0.557 Inf     -3.39    -1.209
#  V5_MDS2_12ON  -1.08 0.491 Inf     -2.04    -0.119
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                    estimate    SE  df z.ratio p.value
#  V0_MDS2_12ON - V1_MDS2_12ON    0.772 0.639 Inf   1.210  1.0000
#  V0_MDS2_12ON - V3_MDS2_12ON   -0.823 0.593 Inf  -1.387  0.9925
#  V0_MDS2_12ON - V5_MDS2_12ON   -2.044 0.637 Inf  -3.207  0.0081
#  V1_MDS2_12ON - V3_MDS2_12ON   -1.595 0.640 Inf  -2.491  0.0764
#  V1_MDS2_12ON - V5_MDS2_12ON   -2.816 0.697 Inf  -4.040  0.0003
#  V3_MDS2_12ON - V5_MDS2_12ON   -1.220 0.592 Inf  -2.060  0.2362
# 
# P value adjustment: bonferroni method for 6 tests 










UPDRSII_V0_2.13 <- fread("data/UPDRSII_V0_2.13.txt")
UPDRSII_V1_2.13 <- fread("data/UPDRSII_V1_2.13.txt")

UPDRSII_2.13 <- UPDRSII_V0_2.13 %>% select(-V0_MDS2_13OFF) %>% inner_join(UPDRSII_V1_2.13 %>% select(-V1_MDS2_13OFF ))

UPDRSII_2.13 <- UPDRSII_2.13 %>% 
  full_join(UPDRSII_V3_V5 %>% select(SUBJID, V3_MDS2_13ON ,V5_MDS2_13ON  )) %>%
  drop_na() %>% mutate(V3_MDS2_13ON=as.numeric(V3_MDS2_13ON),V5_MDS2_13ON=as.numeric(V5_MDS2_13ON) ) %>% drop_na()



# Reshape data to long format
data_long <- UPDRSII_2.13 %>%
  pivot_longer(cols = c( V0_MDS2_13ON, V1_MDS2_13ON, V3_MDS2_13ON, V5_MDS2_13ON),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSII_2.13[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(UPDRSII_2.13[, 2:5])
# Friedman chi-squared = 14.25, df = 3, p-value = 0.002584


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# V
#              V0_MDS2_13ON V1_MDS2_13ON V3_MDS2_13ON
# V1_MDS2_13ON 1.000        -            -           
# V3_MDS2_13ON 0.142        0.047        -           
# V5_MDS2_13ON 0.213        0.034        1.000       
# 
# P value adjustment method: bonferroni 


# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_13ON", "V0 2.13 ON",
                                          ifelse(Condition=="V1_MDS2_13ON", "V1 2.13 ON",
                                                 ifelse(Condition=="V3_MDS2_13ON", "V3 2.13 ON", "V5 2.13 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.13 ON", "V1 2.13 ON","V3 2.13 ON", "V5 2.13 ON"))) %>%
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
    scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="mds2.13.svg", plot=plot1, width=3, height=5)


# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

length(unique(data_long$SUBJID))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 21) * 100)  # Always using 21 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_13ON", "V0 2.13 ON",
                                          ifelse(Condition=="V1_MDS2_13ON", "V1 2.13 ON",
                                                 ifelse(Condition=="V3_MDS2_13ON", "V3 2.13 ON", "V5 2.13 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.13 ON", "V1 2.13 ON","V3 2.13 ON", "V5 2.13 ON")))

# Filter only Value = 0 for labeling
labels_data <- percentage_stats 

plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n ON Condition",
       y = "Percentage (%) \n",
       fill = "Item 2.13") +
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


ggsave(file="mds2.13_perc.svg", plot=plot1, width=4, height=5)






library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)

# 
# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik AIC    niter    max.grad cond.H 
#  logit flexible  84   -74.87 163.74 325(978) 1.84e-06 4.5e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 1.442    1.201   
# Number of groups:  SUBJID 21 
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)  
# ConditionV1_MDS2_13ON  -0.5001     0.7672  -0.652   0.5145  
# ConditionV3_MDS2_13ON   1.8714     0.7287   2.568   0.0102 *
# ConditionV5_MDS2_13ON   1.7012     0.7099   2.396   0.0166 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.1216     0.6077   1.845
# 1|2   3.2901     0.7534   4.367
# 2|3   6.2800     1.2902   4.867



library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# $emmeans
#  Condition    emmean    SE  df asymp.LCL asymp.UCL
#  V0_MDS2_13ON  -3.56 0.769 Inf     -5.07    -2.056
#  V1_MDS2_13ON  -4.06 0.828 Inf     -5.69    -2.442
#  V3_MDS2_13ON  -1.69 0.603 Inf     -2.88    -0.510
#  V5_MDS2_13ON  -1.86 0.615 Inf     -3.07    -0.657
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                    estimate    SE  df z.ratio p.value
#  V0_MDS2_13ON - V1_MDS2_13ON     0.50 0.767 Inf   0.652  1.0000
#  V0_MDS2_13ON - V3_MDS2_13ON    -1.87 0.729 Inf  -2.568  0.0614
#  V0_MDS2_13ON - V5_MDS2_13ON    -1.70 0.710 Inf  -2.396  0.0994
#  V1_MDS2_13ON - V3_MDS2_13ON    -2.37 0.777 Inf  -3.052  0.0137
#  V1_MDS2_13ON - V5_MDS2_13ON    -2.20 0.753 Inf  -2.923  0.0208
#  V3_MDS2_13ON - V5_MDS2_13ON     0.17 0.620 Inf   0.274  1.0000
# 
# P value adjustment: bonferroni method for 6 tests 















UPDRSIII_COMPLET_V3_V5  <- read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MARCHE,	ON_FREEZING)

unique(UPDRSIII_COMPLET_V3_V5$VISIT)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 3 ans - V3") %>% select(-VISIT) %>%
  rename("V3_ON_MARCHE"="ON_MARCHE", "V3_ON_FREEZING"="ON_FREEZING") %>%
  full_join(
    UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 5 ans - V5") %>% select(-VISIT) %>%
  rename("V5_ON_MARCHE"="ON_MARCHE", "V5_ON_FREEZING"="ON_FREEZING")
  )


Item3.11_before_vs_after <- fread("data/Item3.11_before_vs_after.txt")
Item3.10_before_vs_after <- fread("data/Item3.10_before_vs_after.txt")





UPDRSII_3.10 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, V3_ON_MARCHE, V5_ON_MARCHE) %>% 
  full_join(Item3.10_before_vs_after %>% select(SUBJID, Min_ON_Before ,Min_ON_After  )) %>%
  drop_na() %>% mutate(V3_ON_MARCHE=as.numeric(V3_ON_MARCHE),V5_ON_MARCHE=as.numeric(V5_ON_MARCHE) ) %>% drop_na()



# Reshape data to long format
data_long <- UPDRSII_3.10 %>%
  pivot_longer(cols = c( V3_ON_MARCHE, V5_ON_MARCHE, Min_ON_Before ,Min_ON_After),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSII_3.10[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(UPDRSII_3.10[, 2:5])
# Friedman chi-squared = 193.1, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#               Min_ON_After Min_ON_Before V3_ON_MARCHE
# Min_ON_Before 0.0031       -             -           
# V3_ON_MARCHE  2.4e-11      3.9e-14       -           
# V5_ON_MARCHE  < 2e-16      < 2e-16       4.6e-06     
# 
# P value adjustment method: bonferroni 


# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="Min_ON_Before", "V0 3.10 ON",
                                          ifelse(Condition=="Min_ON_After", "V1 3.10 ON",
                                                 ifelse(Condition=="V3_ON_MARCHE", "V3 3.10 ON", "V5 3.10 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 3.10 ON", "V1 3.10 ON","V3 3.10 ON", "V5 3.10 ON"))) %>%
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
    scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="mds3.10.svg", plot=plot1, width=3, height=5)


# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

length(unique(data_long$SUBJID))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 163) * 100)  # Always using 21 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="Min_ON_Before", "V0 3.10 ON",
                                          ifelse(Condition=="Min_ON_After", "V1 3.10 ON",
                                                 ifelse(Condition=="V3_ON_MARCHE", "V3 3.10 ON", "V5 3.10 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 3.10 ON", "V1 3.10 ON","V3 3.10 ON", "V5 3.10 ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats 

plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n ON Condition",
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


ggsave(file="mds3.10_perc.svg", plot=plot1, width=4, height=5)








library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik  AIC     niter     max.grad cond.H 
#  logit flexible  652  -567.98 1151.96 487(2988) 4.89e-06 1.1e+02
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 3.512    1.874   
# Number of groups:  SUBJID 163 
# 
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
# ConditionMin_ON_Before  -1.2138     0.3219  -3.771 0.000163 ***
# ConditionV3_ON_MARCHE    1.9624     0.2762   7.105  1.2e-12 ***
# ConditionV5_ON_MARCHE    3.0020     0.3014   9.960  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.3689     0.2604   5.257
# 1|2   4.1151     0.3499  11.759
# 2|3   6.5400     0.4633  14.117
# 3|4   8.1479     0.6066  13.432



library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")



# $emmeans
#  Condition     emmean    SE  df asymp.LCL asymp.UCL
#  Min_ON_After   -5.04 0.372 Inf     -5.77     -4.31
#  Min_ON_Before  -6.26 0.439 Inf     -7.12     -5.40
#  V3_ON_MARCHE   -3.08 0.288 Inf     -3.65     -2.52
#  V5_ON_MARCHE   -2.04 0.255 Inf     -2.54     -1.54
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                     estimate    SE  df z.ratio p.value
#  Min_ON_After - Min_ON_Before     1.21 0.322 Inf   3.771  0.0010
#  Min_ON_After - V3_ON_MARCHE     -1.96 0.276 Inf  -7.105  <.0001
#  Min_ON_After - V5_ON_MARCHE     -3.00 0.301 Inf  -9.960  <.0001
#  Min_ON_Before - V3_ON_MARCHE    -3.18 0.340 Inf  -9.353  <.0001
#  Min_ON_Before - V5_ON_MARCHE    -4.22 0.368 Inf -11.448  <.0001
#  V3_ON_MARCHE - V5_ON_MARCHE     -1.04 0.235 Inf  -4.429  0.0001
# 
# P value adjustment: bonferroni method for 6 tests 




UPDRSII_3.11 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, V3_ON_FREEZING , V5_ON_FREEZING) %>% 
  full_join(Item3.11_before_vs_after %>% select(SUBJID, Min_ON_Before ,Min_ON_After  )) %>%
  drop_na() %>% mutate(V3_ON_FREEZING=as.numeric(V3_ON_FREEZING),V5_ON_FREEZING=as.numeric(V5_ON_FREEZING) ) %>% drop_na()



# Reshape data to long format
data_long <- UPDRSII_3.11 %>%
  pivot_longer(cols = c( V3_ON_FREEZING, V5_ON_FREEZING, Min_ON_Before ,Min_ON_After),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSII_3.11[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(UPDRSII_3.11[, 2:5])
# Friedman chi-squared = 70.593, df = 3, p-value = 3.186e-15

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#                Min_ON_After Min_ON_Before V3_ON_FREEZING
# Min_ON_Before  1.00         -             -             
# V3_ON_FREEZING 1.1e-05      9.7e-05       -             
# V5_ON_FREEZING 1.4e-06      6.3e-06       0.26          
# 
# P value adjustment method: bonferroni 

# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="Min_ON_Before", "V0 3.11 ON",
                                          ifelse(Condition=="Min_ON_After", "V1 3.11 ON",
                                                 ifelse(Condition=="V3_ON_FREEZING", "V3 3.11 ON", "V5 3.11 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 3.11 ON", "V1 3.11 ON","V3 3.11 ON", "V5 3.11 ON"))) %>%
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
    scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="mds3.11.svg", plot=plot1, width=3, height=5)


# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

length(unique(data_long$SUBJID))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 161) * 100)  # Always using 21 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="Min_ON_Before", "V0 3.11 ON",
                                          ifelse(Condition=="Min_ON_After", "V1 3.11 ON",
                                                 ifelse(Condition=="V3_ON_FREEZING", "V3 3.11 ON", "V5 3.11 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 3.11 ON", "V1 3.11 ON","V3 3.11 ON", "V5 3.11 ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats 

plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n ON Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.11") +
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


ggsave(file="mds3.11_perc.svg", plot=plot1, width=4, height=5)









library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik  AIC    niter     max.grad cond.H 
#  logit flexible  644  -251.50 519.01 606(3085) 8.97e-06 3.3e+03
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 15.66    3.957   
# Number of groups:  SUBJID 161 
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# ConditionMin_ON_Before   -0.2480     0.8709  -0.285    0.776    
# ConditionV3_ON_FREEZING   3.5200     0.7779   4.525 6.04e-06 ***
# ConditionV5_ON_FREEZING   4.2338     0.8671   4.883 1.05e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1    7.992      2.134   3.745
# 1|2    9.735      2.374   4.101
# 2|3   11.460      2.542   4.508
# 3|4   12.018      2.578   4.663

library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# $emmeans
#  Condition      emmean   SE  df asymp.LCL asymp.UCL
#  Min_ON_After   -10.30 2.40 Inf    -15.00     -5.60
#  Min_ON_Before  -10.55 2.41 Inf    -15.28     -5.82
#  V3_ON_FREEZING  -6.78 1.92 Inf    -10.54     -3.02
#  V5_ON_FREEZING  -6.07 1.77 Inf     -9.54     -2.59
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                        estimate    SE  df z.ratio p.value
#  Min_ON_After - Min_ON_Before       0.248 0.871 Inf   0.285  1.0000
#  Min_ON_After - V3_ON_FREEZING     -3.520 0.778 Inf  -4.525  <.0001
#  Min_ON_After - V5_ON_FREEZING     -4.234 0.867 Inf  -4.883  <.0001
#  Min_ON_Before - V3_ON_FREEZING    -3.768 0.832 Inf  -4.531  <.0001
#  Min_ON_Before - V5_ON_FREEZING    -4.482 0.913 Inf  -4.910  <.0001
#  V3_ON_FREEZING - V5_ON_FREEZING   -0.714 0.380 Inf  -1.880  0.3606
# 
# P value adjustment: bonferroni method for 6 tests 


