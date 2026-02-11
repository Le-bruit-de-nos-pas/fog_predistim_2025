
library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)


# 2.13 -------------------------------------------

UPDRSII_complet_V0_V1_V3_V5  <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSII complet V0_V1_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSII_complet_V0_V1_V3_V5 <- UPDRSII_complet_V0_V1_V3_V5 %>% select(SUBJID, VISIT, MDS2_13OFF, MDS2_13ON) %>%
  filter(SUBJID!="Subject Identifier for the Study") %>%
pivot_longer(cols = c(MDS2_13OFF, MDS2_13ON),
               names_to = "Condition",
               values_to = "Value") %>%
  filter((VISIT=="Visite de screening"&Condition=="MDS2_13OFF")| Condition=="MDS2_13ON") %>%
  drop_na() %>% filter(Value!=".D") %>%
  group_by(SUBJID) %>% count() %>% filter(n==5) %>%
  left_join(UPDRSII_complet_V0_V1_V3_V5 %>% select(SUBJID, VISIT, MDS2_13OFF, MDS2_13ON) %>%
  filter(SUBJID!="Subject Identifier for the Study") %>%
pivot_longer(cols = c(MDS2_13OFF, MDS2_13ON),
               names_to = "Condition",
               values_to = "Value") %>%
  filter((VISIT=="Visite de screening"&Condition=="MDS2_13OFF")| Condition=="MDS2_13ON") %>%
  drop_na() ) %>% ungroup() %>%
  mutate(Value=ifelse(Value=="Normal",0,
                      ifelse(Value=="Minime",1,
                             ifelse(Value=="Léger",2,
                                    ifelse(Value=="Modéré",3,4)))))


UPDRSII_complet_V0_V1_V3_V5 <- UPDRSII_complet_V0_V1_V3_V5 %>% mutate(Condition=ifelse(VISIT=="Visite de screening"&Condition=="MDS2_13OFF", "[V0] OFF",
                                                        ifelse(VISIT=="Visite de screening"&Condition=="MDS2_13ON", "[V0] ON",
                                                               ifelse(VISIT=="Visite Bilan à 1 an - V1", "[V1] ON",
                                                                      ifelse(VISIT=="Visite Bilan à 3 ans - V3", "[V3] ON", "[V5] ON")))))



pairwise.wilcox.test(UPDRSII_complet_V0_V1_V3_V5$Value, UPDRSII_complet_V0_V1_V3_V5$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")

# ata:  UPDRSII_complet_V0_V1_V3_V5$Value and UPDRSII_complet_V0_V1_V3_V5$Condition 
# 
#              [V0] 2.13 OFF [V0] 2.13 ON [V1] 2.13 ON [V3] 2.13 ON
# [V0] 2.13 ON 5.8e-06       -            -            -           
# [V1] 2.13 ON 0.00013       1.00000      -            -           
# [V3] 2.13 ON 0.59854       0.00017      0.00030      -           
# [V5] 2.13 ON 1.00000       0.00031      0.00035      1.00000     
# 
# P value adjustment method: bonferroni 




# Calculate mean and standard error of the mean (SEM)
summary_stats <- UPDRSII_complet_V0_V1_V3_V5 %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- UPDRSII_complet_V0_V1_V3_V5 %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS 2.13 item \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)






# Calculate percentage breakdown of each score per condition
percentage_stats <- UPDRSII_complet_V0_V1_V3_V5 %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 60) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Follow-up Condition",
       y = "Percentage (%) \n",
       fill = "Item 2.13") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  theme(text = element_text(face = "bold")) +
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) 

plot1

ggsave(file="p1.svg", plot=plot1, width=4, height=4)

library(ordinal)

UPDRSII_complet_V0_V1_V3_V5$Value <- factor(UPDRSII_complet_V0_V1_V3_V5$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = UPDRSII_complet_V0_V1_V3_V5, link = "logit", control = clmm.control(method = "ucminf"))
summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    UPDRSII_complet_V0_V1_V3_V5
# 
#  link  threshold nobs logLik  AIC    niter     max.grad cond.H 
#  logit flexible  300  -264.91 547.83 525(2084) 4.39e-06 4.2e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 2.152    1.467   
# Number of groups:  SUBJID 60 
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)    
# Condition[V0] 2.13 ON  -3.0349     0.5279  -5.749 8.98e-09 ***
# Condition[V1] 2.13 ON  -2.5745     0.4830  -5.330 9.82e-08 ***
# Condition[V3] 2.13 ON  -0.4998     0.3726  -1.341    0.180    
# Condition[V5] 2.13 ON  -0.3660     0.3745  -0.977    0.328    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  -0.5382     0.3380  -1.592
# 1|2   1.5776     0.3637   4.338
# 2|3   3.3152     0.4649   7.130
# 3|4   4.5950     0.6285   7.311




# 3.11 -------------------------------------------


UPDRSIII_complet_V3_V5  <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSIII complet V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSIII_complet_V3_V5 <- UPDRSIII_complet_V3_V5 %>% select(SUBJID, VISIT, ON_FREEZING)

UPDRSIII_complet_V3_V5 <- UPDRSIII_complet_V3_V5 %>% 
  filter(SUBJID!="Subject Identifier for the Study") %>%
  filter(ON_FREEZING!=".D"&ON_FREEZING!="00"&ON_FREEZING!=".F"&!is.na(ON_FREEZING))


Item3.11_before_vs_after <- fread("data/Item3.11_before_vs_after.txt")

UPDRSIII_complet_V3_V5 <- Item3.11_before_vs_after %>% select(SUBJID, OFF_Before, Min_ON_Before, Min_ON_After) %>%
inner_join(UPDRSIII_complet_V3_V5 %>% spread(key=VISIT, value=ON_FREEZING)) %>%
  drop_na()

names(UPDRSIII_complet_V3_V5) <-  c("SUBJID",  "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON")


UPDRSIII_complet_V3_V5$`[V3] ON` <- as.numeric(UPDRSIII_complet_V3_V5$`[V3] ON`)
UPDRSIII_complet_V3_V5$`[V5] ON` <- as.numeric(UPDRSIII_complet_V3_V5$`[V5] ON`)

# Reshape data to long format
data_long <- UPDRSIII_complet_V3_V5 %>%
  pivot_longer(cols = c( "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON"),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSIII_complet_V3_V5[, 2:6]))  # Assuming columns 2:5 are conditions


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 
# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#              [V0] 3.11 OFF [V0] 3.11 ON [V1] 3.11 ON [V3] 3.11 ON
# [V0] 3.11 ON 2.3e-14       -            -            -           
# [V1] 3.11 ON 3.8e-14       1.00000      -            -           
# [V3] 3.11 ON 5.0e-07       1.1e-06      9.8e-07      -           
# [V5] 3.11 ON 0.00094       8.9e-08      6.4e-08      0.29292   




# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS 3.11 item \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)






# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 214) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Follow-up Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.11") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  theme(text = element_text(face = "bold")) +
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) 

plot1

ggsave(file="p1.svg", plot=plot1, width=4, height=4)

library(ordinal)

data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))
summary(model)










# 2.12 -------------------------------------------

UPDRSII_complet_V0_V1_V3_V5  <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSII complet V0_V1_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSII_complet_V0_V1_V3_V5 <- UPDRSII_complet_V0_V1_V3_V5 %>% select(SUBJID, VISIT, MDS2_12OFF, MDS2_12ON) %>%
  filter(SUBJID!="Subject Identifier for the Study") %>%
pivot_longer(cols = c(MDS2_12OFF, MDS2_12ON),
               names_to = "Condition",
               values_to = "Value") %>%
  filter((VISIT=="Visite de screening"&Condition=="MDS2_12OFF")| Condition=="MDS2_12ON") %>%
  drop_na() %>% filter(Value!=".D") %>%
  group_by(SUBJID) %>% count() %>% filter(n==5) %>%
  left_join(UPDRSII_complet_V0_V1_V3_V5 %>% select(SUBJID, VISIT, MDS2_12OFF, MDS2_12ON) %>%
  filter(SUBJID!="Subject Identifier for the Study") %>%
pivot_longer(cols = c(MDS2_12OFF, MDS2_12ON),
               names_to = "Condition",
               values_to = "Value") %>%
  filter((VISIT=="Visite de screening"&Condition=="MDS2_12OFF")| Condition=="MDS2_12ON") %>%
  drop_na() ) %>% ungroup() %>%
  mutate(Value=ifelse(Value=="Normal",0,
                      ifelse(Value=="Minime",1,
                             ifelse(Value=="Léger",2,
                                    ifelse(Value=="Modéré",3,4)))))


UPDRSII_complet_V0_V1_V3_V5 <- UPDRSII_complet_V0_V1_V3_V5 %>% mutate(Condition=ifelse(VISIT=="Visite de screening"&Condition=="MDS2_12OFF", "[V0] OFF",
                                                        ifelse(VISIT=="Visite de screening"&Condition=="MDS2_12ON", "[V0] ON",
                                                               ifelse(VISIT=="Visite Bilan à 1 an - V1", "[V1] ON",
                                                                      ifelse(VISIT=="Visite Bilan à 3 ans - V3", "[V3] ON", "[V5] ON")))))



pairwise.wilcox.test(UPDRSII_complet_V0_V1_V3_V5$Value, UPDRSII_complet_V0_V1_V3_V5$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")

#	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 

# data:  UPDRSII_complet_V0_V1_V3_V5$Value and UPDRSII_complet_V0_V1_V3_V5$Condition 
# 
#         [V0] OFF [V0] ON [V1] ON [V3] ON
# [V0] ON 1.3e-08  -       -       -      
# [V1] ON 1.7e-06  1.00000 -       -      
# [V3] ON 0.03060  0.00033 0.00077 -      
# [V5] ON 1.00000  4.0e-06 1.4e-05 0.14174

# 
# P value adjustment method: bonferroni 




# Calculate mean and standard error of the mean (SEM)
summary_stats <- UPDRSII_complet_V0_V1_V3_V5 %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- UPDRSII_complet_V0_V1_V3_V5 %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS 2.12 item \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)






# Calculate percentage breakdown of each score per condition
percentage_stats <- UPDRSII_complet_V0_V1_V3_V5 %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 60) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Follow-up Condition",
       y = "Percentage (%) \n",
       fill = "Item 2.12") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  theme(text = element_text(face = "bold")) +
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) 

plot1

ggsave(file="p1.svg", plot=plot1, width=4, height=4)

library(ordinal)

UPDRSII_complet_V0_V1_V3_V5$Value <- factor(UPDRSII_complet_V0_V1_V3_V5$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = UPDRSII_complet_V0_V1_V3_V5, link = "logit", control = clmm.control(method = "ucminf"))
summary(model)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    UPDRSII_complet_V0_V1_V3_V5
# 
#  link  threshold nobs logLik  AIC    niter     max.grad cond.H 
#  logit flexible  300  -334.29 686.57 563(1692) 5.18e-06 4.3e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 0.9866   0.9933  
# Number of groups:  SUBJID 60 
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# Condition[V0] ON  -3.2637     0.4310  -7.573 3.66e-14 ***
# Condition[V1] ON  -2.8213     0.4085  -6.907 4.95e-12 ***
# Condition[V3] ON  -1.1414     0.3506  -3.255  0.00113 ** 
# Condition[V5] ON  -0.3666     0.3451  -1.062  0.28809    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  -2.2830     0.3262  -6.998
# 1|2   0.2974     0.2764   1.076
# 2|3   1.4935     0.3062   4.878
# 3|4   3.2763     0.4602   7.119


library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# $emmeans
#  Condition emmean    SE  df asymp.LCL asymp.UCL
#  [V0] OFF  -0.696 0.281 Inf     -1.25    -0.146
#  [V0] ON   -3.960 0.404 Inf     -4.75    -3.168
#  [V1] ON   -3.517 0.379 Inf     -4.26    -2.775
#  [V3] ON   -1.837 0.312 Inf     -2.45    -1.227
#  [V5] ON   -1.063 0.296 Inf     -1.64    -0.482
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast           estimate    SE  df z.ratio p.value
#  [V0] OFF - [V0] ON    3.264 0.431 Inf   7.573  <.0001
#  [V0] OFF - [V1] ON    2.821 0.408 Inf   6.907  <.0001
#  [V0] OFF - [V3] ON    1.141 0.351 Inf   3.255  0.0113
#  [V0] OFF - [V5] ON    0.367 0.345 Inf   1.062  1.0000
#  [V0] ON - [V1] ON    -0.442 0.409 Inf  -1.082  1.0000
#  [V0] ON - [V3] ON    -2.122 0.407 Inf  -5.210  <.0001
#  [V0] ON - [V5] ON    -2.897 0.430 Inf  -6.736  <.0001
#  [V1] ON - [V3] ON    -1.680 0.388 Inf  -4.327  0.0002
#  [V1] ON - [V5] ON    -2.455 0.409 Inf  -6.002  <.0001
#  [V3] ON - [V5] ON    -0.775 0.358 Inf  -2.162  0.3061
# 
# P value adjustment: bonferroni method for 10 tests 


# 3.10 -------------------------------------------


UPDRSIII_complet_V3_V5  <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSIII complet V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSIII_complet_V3_V5 <- UPDRSIII_complet_V3_V5 %>% select(SUBJID, VISIT, ON_MARCHE)

unique(UPDRSIII_complet_V3_V5$ON_MARCHE)

UPDRSIII_complet_V3_V5 <- UPDRSIII_complet_V3_V5 %>% 
  filter(SUBJID!="Subject Identifier for the Study") %>%
  filter(ON_MARCHE!=".D"&ON_MARCHE!="00"&ON_MARCHE!=".F"&!is.na(ON_MARCHE))


Item3.10_before_vs_after <- fread("data/Item3.10_before_vs_after.txt")

UPDRSIII_complet_V3_V5 <- Item3.10_before_vs_after %>% select(SUBJID, OFF_Before, Min_ON_Before, Min_ON_After) %>%
inner_join(UPDRSIII_complet_V3_V5 %>% spread(key=VISIT, value=ON_MARCHE)) %>%
  drop_na()

names(UPDRSIII_complet_V3_V5) <-  c("SUBJID",  "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON")


UPDRSIII_complet_V3_V5$`[V3] ON` <- as.numeric(UPDRSIII_complet_V3_V5$`[V3] ON`)
UPDRSIII_complet_V3_V5$`[V5] ON` <- as.numeric(UPDRSIII_complet_V3_V5$`[V5] ON`)

# Reshape data to long format
data_long <- UPDRSIII_complet_V3_V5 %>%
  pivot_longer(cols = c( "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON"),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSIII_complet_V3_V5[, 2:6]))  # Assuming columns 2:5 are conditions


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 
# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#        [V0] OFF [V0] ON [V1] ON [V3] ON
# [V0] ON < 2e-16  -       -       -      
# [V1] ON < 2e-16  0.043   -       -      
# [V3] ON 4.9e-16  2.2e-15 4.9e-14 -      
# [V5] ON 3.6e-06  < 2e-16 < 2e-16 9.7e-08




# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS 3.10 item \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)






# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 216) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Follow-up Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.10") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  theme(text = element_text(face = "bold")) +
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) 

plot1

ggsave(file="p1.svg", plot=plot1, width=4, height=4)

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
#  logit flexible  1080 -1042.33 2102.67 734(3503) 1.42e-05 4.0e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 1.875    1.369   
# Number of groups:  SUBJID 216 
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# Condition[V0] ON  -4.5394     0.2854 -15.904  < 2e-16 ***
# Condition[V1] ON  -3.7791     0.2530 -14.939  < 2e-16 ***
# Condition[V3] ON  -2.0671     0.2057 -10.049  < 2e-16 ***
# Condition[V5] ON  -1.1183     0.1923  -5.814  6.1e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1 -2.42809    0.19689 -12.332
# 1|2 -0.09779    0.17079  -0.573
# 2|3  2.24067    0.20715  10.816
# 3|4  3.44312    0.27674  12.442


library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")



# $emmeans
#  Condition emmean    SE  df asymp.LCL asymp.UCL
#  [V0] OFF  -0.789 0.171 Inf     -1.12    -0.454
#  [V0] ON   -5.329 0.288 Inf     -5.89    -4.764
#  [V1] ON   -4.569 0.255 Inf     -5.07    -4.069
#  [V3] ON   -2.857 0.202 Inf     -3.25    -2.461
#  [V5] ON   -1.908 0.184 Inf     -2.27    -1.548
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast           estimate    SE  df z.ratio p.value
#  [V0] OFF - [V0] ON    4.539 0.285 Inf  15.904  <.0001
#  [V0] OFF - [V1] ON    3.779 0.253 Inf  14.939  <.0001
#  [V0] OFF - [V3] ON    2.067 0.206 Inf  10.049  <.0001
#  [V0] OFF - [V5] ON    1.118 0.192 Inf   5.814  <.0001
#  [V0] ON - [V1] ON    -0.760 0.260 Inf  -2.929  0.0340
#  [V0] ON - [V3] ON    -2.472 0.255 Inf  -9.690  <.0001
#  [V0] ON - [V5] ON    -3.421 0.266 Inf -12.870  <.0001
#  [V1] ON - [V3] ON    -1.712 0.224 Inf  -7.627  <.0001
#  [V1] ON - [V5] ON    -2.661 0.234 Inf -11.383  <.0001
#  [V3] ON - [V5] ON    -0.949 0.194 Inf  -4.894  <.0001
# 
# P value adjustment: bonferroni method for 10 tests 


# Axials  -------------------------------------------



UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)


UPDRSIII_COMPLET_V0_V1$ONOFF_3.9

Item3.9 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.9_,	OFF_3.9_1,	OFFON_3.9_,	ON_3.9_)
Item3.9 <- Item3.9[-1,]
names(Item3.9) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.9 <- data.frame(Item3.9) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.9) <- c("SUBJID", "ONOFF_After_3.9", "OFF_After_3.9",  "OFFON_After_3.9", "ONON_After_3.9")

Item3.10 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.10_,	OFF_3.10_1,	OFFON_3.10_,	ON_3.10_)
Item3.10 <- Item3.10[-1,]
names(Item3.10) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.10 <- data.frame(Item3.10) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.10) <- c("SUBJID", "ONOFF_After_3.10", "OFF_After_3.10",  "OFFON_After_3.10", "ONON_After_3.10")

Item3.11 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.11_,	OFF_3.11_1,	OFFON_3.11_,	ON_3.11_)
Item3.11 <- Item3.11[-1,]
names(Item3.11) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.11 <- data.frame(Item3.11) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.11) <- c("SUBJID", "ONOFF_After_3.11", "OFF_After_3.11",  "OFFON_After_3.11", "ONON_After_3.11")

Item3.12 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.12_,	OFF_3.12_1,	OFFON_3.12_,	ON_3.12_)
Item3.12 <- Item3.12[-1,]
names(Item3.12) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.12 <- data.frame(Item3.12) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.12) <- c("SUBJID", "ONOFF_After_3.12", "OFF_After_3.12",  "OFFON_After_3.12", "ONON_After_3.12")

Axials <- Item3.9 %>% inner_join(Item3.10) %>% inner_join(Item3.11) %>% inner_join(Item3.12) %>% drop_na() 
names(Axials)
Axials <- Axials %>% 
  mutate(OFF_After=OFF_After_3.9+OFF_After_3.10+OFF_After_3.11+OFF_After_3.12) %>%
  mutate(ONOFF_After=ONOFF_After_3.9+ONOFF_After_3.10+ONOFF_After_3.11+ONOFF_After_3.12) %>%
  mutate(OFFON_After=OFFON_After_3.9+OFFON_After_3.10+OFFON_After_3.11+OFFON_After_3.12) %>%
  mutate(ONON_After=ONON_After_3.9+ONON_After_3.10+ONON_After_3.11+ONON_After_3.12) 

Axials <- Axials %>% select(SUBJID, OFF_After, ONOFF_After, OFFON_After, ONON_After)



Axials_V1 <- Axials %>% select(SUBJID, ONON_After) %>% rename("ONON_V1"="ONON_After")






UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V0_V1)

Axials_V1_ON <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ON_3.9_6, ON_3.10_6, ON_3.11_6, ON_3.12_6)
Axials_V1_ON <- Axials_V1_ON[-1,]
Axials_V1_ON$VISIT <- 1
Axials_V1_ON <- Axials_V1_ON %>% select(SUBJID, VISIT, ON_3.9_6, ON_3.10_6, ON_3.11_6, ON_3.12_6)

Axials_V1_ON <- Axials_V1_ON %>% 
  mutate(ON_3.9_6=as.numeric(ON_3.9_6)) %>% 
  mutate(ON_3.10_6=as.numeric(ON_3.10_6)) %>% 
  mutate(ON_3.11_6=as.numeric(ON_3.11_6)) %>% 
  mutate(ON_3.12_6=as.numeric(ON_3.12_6))  %>%
  mutate(Axial_ON_v1=ON_3.9_6+ON_3.10_6+ON_3.11_6+ON_3.12_6) %>% drop_na() %>%
  select(SUBJID, Axial_ON_v1)


Axials_V1_OFF <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.9_1, OFF_3.10_1, OFF_3.11_1, OFF_3.12_1)
Axials_V1_OFF <- Axials_V1_OFF[-1,]
Axials_V1_OFF$VISIT <- 1
Axials_V1_OFF <- Axials_V1_OFF %>% select(SUBJID, VISIT, OFF_3.9_1, OFF_3.10_1, OFF_3.11_1, OFF_3.12_1)

Axials_V1_OFF <- Axials_V1_OFF %>% 
  mutate(OFF_3.9_1=as.numeric(OFF_3.9_1)) %>% 
  mutate(OFF_3.10_1=as.numeric(OFF_3.10_1)) %>% 
  mutate(OFF_3.11_1=as.numeric(OFF_3.11_1)) %>% 
  mutate(OFF_3.12_1=as.numeric(OFF_3.12_1))  %>%
  mutate(Axial_OFF_v1=OFF_3.9_1+OFF_3.10_1+OFF_3.11_1+OFF_3.12_1) %>% drop_na() %>%
  select(SUBJID, Axial_OFF_v1)





Axials_V0_ON <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ON_3.9_3, ON_3.10_3, ON_3.11_3, ON_3.12_3)
Axials_V0_ON <- Axials_V0_ON[-1,]
Axials_V0_ON$VISIT <- 0
Axials_V0_ON <- Axials_V0_ON %>% select(SUBJID, VISIT, ON_3.9_3, ON_3.10_3, ON_3.11_3, ON_3.12_3)

Axials_V0_ON <- Axials_V0_ON %>% 
  mutate(ON_3.9_3=as.numeric(ON_3.9_3)) %>% 
  mutate(ON_3.10_3=as.numeric(ON_3.10_3)) %>% 
  mutate(ON_3.11_3=as.numeric(ON_3.11_3)) %>% 
  mutate(ON_3.12_3=as.numeric(ON_3.12_3))  %>%
  mutate(Axial_ON_v0=ON_3.9_3+ON_3.10_3+ON_3.11_3+ON_3.12_3) %>% drop_na() %>%
  select(SUBJID, Axial_ON_v0)



Axials_V0_OFF <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.9_, OFF_3.10_, OFF_3.11_, OFF_3.12_)
Axials_V0_OFF <- Axials_V0_OFF[-1,]
Axials_V0_OFF$VISIT <- 0
Axials_V0_OFF <- Axials_V0_OFF %>% select(SUBJID, VISIT, OFF_3.9_, OFF_3.10_, OFF_3.11_, OFF_3.12_)

Axials_V0_OFF <- Axials_V0_OFF %>% 
  mutate(OFF_3.9_=as.numeric(OFF_3.9_)) %>% 
  mutate(OFF_3.10_=as.numeric(OFF_3.10_)) %>% 
  mutate(OFF_3.11_=as.numeric(OFF_3.11_)) %>% 
  mutate(OFF_3.12_=as.numeric(OFF_3.12_))  %>%
  mutate(Axial_OFF_v0=OFF_3.9_+OFF_3.10_+OFF_3.11_+OFF_3.12_) %>% drop_na() %>%
  select(SUBJID, Axial_OFF_v0)


Axials <- Axials_V0_OFF %>% inner_join(Axials_V0_ON) %>%
  inner_join(Axials_V1_OFF) %>% inner_join(Axials_V1_ON)






Axials_V0 <- Axials %>% select(SUBJID, Axial_OFF_v0 , Axial_ON_v0) %>% rename("OFF_V0"="Axial_OFF_v0", "ONON_V0"="Axial_ON_v0")


UPDRSIII_COMPLET_V3_V5 <- readxl::read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_LEVER, ON_MARCHE, ON_FREEZING, ON_STAB_POST)

unique(UPDRSIII_COMPLET_V3_V5$VISIT)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 3 ans - V3")  %>% select(-VISIT)

UPDRSIII_COMPLET_V3_V5$ON_LEVER <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_LEVER) 
UPDRSIII_COMPLET_V3_V5$ON_MARCHE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MARCHE ) 
UPDRSIII_COMPLET_V3_V5$ON_FREEZING  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_FREEZING ) 
UPDRSIII_COMPLET_V3_V5$ON_STAB_POST  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_STAB_POST ) 

Axial_V3 <- UPDRSIII_COMPLET_V3_V5 %>% mutate(Axial_V3=ON_LEVER+ON_MARCHE+ON_FREEZING+ON_STAB_POST) %>% drop_na() %>%
  select(SUBJID, Axial_V3)



UPDRSIII_COMPLET_V3_V5 <- readxl::read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_LEVER, ON_MARCHE, ON_FREEZING, ON_STAB_POST)

unique(UPDRSIII_COMPLET_V3_V5$VISIT)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 5 ans - V5")  %>% select(-VISIT)

UPDRSIII_COMPLET_V3_V5$ON_LEVER <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_LEVER) 
UPDRSIII_COMPLET_V3_V5$ON_MARCHE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MARCHE ) 
UPDRSIII_COMPLET_V3_V5$ON_FREEZING  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_FREEZING ) 
UPDRSIII_COMPLET_V3_V5$ON_STAB_POST  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_STAB_POST ) 

Axial_V5 <- UPDRSIII_COMPLET_V3_V5 %>% mutate(Axial_V5=ON_LEVER+ON_MARCHE+ON_FREEZING+ON_STAB_POST) %>% drop_na() %>%
  select(SUBJID, Axial_V5)

Axials <- Axial_V5 %>% inner_join(Axial_V3) %>% inner_join(Axials_V1) %>% inner_join(Axials_V0) 

names(Axials) <- c("SUBJID", "ONON_V5", "ONON_V3", "ONON_V1", "OFF_V0", "ON_V0")


summary_stats <- sapply(Axials, function(col) {
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
data_long <- Axials %>% 
  rename("[V0] OFF"="OFF_V0", "[V0] ON"="ON_V0", "[V1] ON"="ONON_V1", "[V3] ON"="ONON_V3", "[V5] ON"="ONON_V5") %>%
  pivot_longer(cols = c( "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON"),
               names_to = "Condition",
               values_to = "Value")


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# data:  data_long$Value and data_long$Condition 
# 
#               [V0] Axial OFF [V0] Axial ON [V1] Axial ON [V3] Axial ON
# [V0] Axial ON 9.3e-14        -             -             -            
# [V1] Axial ON 9.6e-09        6.3e-12       -             -            
# [V3] Axial ON 8.8e-05        3.5e-09       1.000         -            
# [V5] Axial ON 1.000          1.8e-10       0.086         0.002      




# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS Axial scores \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)



# Bradykinesia -----------------



UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

Item3.4_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.4_Right_,	OFF_3.4_Right_1,	OFFON_3.4_Right_,	ON_3.4_Right_)
Item3.4_right <- Item3.4_right[-1,]
names(Item3.4_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.4_right <- data.frame(Item3.4_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.4_right) <- c("SUBJID", "ONOFF_After_3.4_right", "OFF_After_3.4_right",  "OFFON_After_3.4_right", "ONON_After_3.4_right")
Item3.4_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID,ONOFF_3.4_Left_	, OFF_3.4_Left_1	, OFFON_3.4_Left_	, ON_3.4_Left_)
Item3.4_left <- Item3.4_left[-1,]
names(Item3.4_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.4_left <- data.frame(Item3.4_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.4_left) <- c("SUBJID", "ONOFF_After_3.4_left", "OFF_After_3.4_left",  "OFFON_After_3.4_left", "ONON_After_3.4_left")


Item3.5_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.5_Right_	, OFF_3.5_Right_1	, OFFON_3.5_Right_	, ON_3.5_Right_)
Item3.5_right <- Item3.5_right[-1,]
names(Item3.5_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.5_right <- data.frame(Item3.5_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.5_right) <- c("SUBJID", "ONOFF_After_3.5_right", "OFF_After_3.5_right",  "OFFON_After_3.5_right", "ONON_After_3.5_right")

Item3.5_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.5_Left_	, OFF_3.5_Left_1	, OFFON_3.5_Left_	, ON_3.5_Left_)
Item3.5_left <- Item3.5_left[-1,]
names(Item3.5_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.5_left <- data.frame(Item3.5_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.5_left) <- c("SUBJID", "ONOFF_After_3.5_left", "OFF_After_3.5_left",  "OFFON_After_3.5_left", "ONON_After_3.5_left")


Item3.6_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.6_Right_	, OFF_3.6_Right_1	, OFFON_3.6_Right_	, ON_3.6_Right_)
Item3.6_right <- Item3.6_right[-1,]
names(Item3.6_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.6_right <- data.frame(Item3.6_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.6_right) <- c("SUBJID", "ONOFF_After_3.6_right", "OFF_After_3.6_right",  "OFFON_After_3.6_right", "ONON_After_3.6_right")
Item3.6_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.6_Left_	, OFF_3.6_Left_1	, OFFON_3.6_Left_	, ON_3.6_Left_)
Item3.6_left <- Item3.6_left[-1,]
names(Item3.6_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.6_left <- data.frame(Item3.6_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.6_left) <- c("SUBJID", "ONOFF_After_3.6_left", "OFF_After_3.6_left",  "OFFON_After_3.6_left", "ONON_After_3.6_left")


Item3.7_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.7_Right_	, OFF_3.7_Right_1	, OFFON_3.7_Right_	, ON_3.7_Right_)
Item3.7_right <- Item3.7_right[-1,]
names(Item3.7_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.7_right <- data.frame(Item3.7_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.7_right) <- c("SUBJID", "ONOFF_After_3.7_right", "OFF_After_3.7_right",  "OFFON_After_3.7_right", "ONON_After_3.7_right")
Item3.7_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.7_Left	, OFF_3.7_Left1	, OFFON_3.7_Left,	ON_3.7_Left)
Item3.7_left <- Item3.7_left[-1,]
names(Item3.7_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.7_left <- data.frame(Item3.7_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.7_left) <- c("SUBJID", "ONOFF_After_3.7_left", "OFF_After_3.7_left",  "OFFON_After_3.7_left", "ONON_After_3.7_left")


Item3.8_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.8_Right_	, OFF_3.8_Right_1	, OFFON_3.8_Right_	, ON_3.8_Right_6)
Item3.8_right <- Item3.8_right[-1,]
names(Item3.8_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.8_right <- data.frame(Item3.8_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.8_right) <- c("SUBJID", "ONOFF_After_3.8_right", "OFF_After_3.8_right",  "OFFON_After_3.8_right", "ONON_After_3.8_right")
Item3.8_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.8_Left	, OFF_3.8_Left1	, OFFON_3.8_Left	,ON_3.8_Left6)
Item3.8_left <- Item3.8_left[-1,]
names(Item3.8_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.8_left <- data.frame(Item3.8_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.8_left) <- c("SUBJID", "ONOFF_After_3.8_left", "OFF_After_3.8_left",  "OFFON_After_3.8_left", "ONON_After_3.8_left")


BradyUp <- Item3.4_left %>% select(SUBJID, ONOFF_After_3.4_left, ONOFF_After_3.4_left, OFF_After_3.4_left, OFFON_After_3.4_left, ONON_After_3.4_left) %>%
  inner_join(Item3.4_right %>% select(SUBJID, ONOFF_After_3.4_right, OFF_After_3.4_right, OFFON_After_3.4_right, ONON_After_3.4_right)) %>%
  mutate(ONOFF_After_3.4=ONOFF_After_3.4_left+ONOFF_After_3.4_right) %>%
  mutate(OFFON_After_3.4=OFFON_After_3.4_left+OFFON_After_3.4_right) %>%
  mutate(OFF_After_3.4=OFF_After_3.4_left+OFF_After_3.4_right) %>%
  mutate(ON_After_3.4=ONON_After_3.4_left+ONON_After_3.4_right) %>%
    select(SUBJID, OFF_After_3.4, ONOFF_After_3.4, OFFON_After_3.4, ON_After_3.4) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.5_left %>% select(SUBJID, ONOFF_After_3.5_left, ONOFF_After_3.5_left, OFF_After_3.5_left, OFFON_After_3.5_left, ONON_After_3.5_left) %>%
      inner_join(Item3.5_right %>% select(SUBJID, ONOFF_After_3.5_right, OFF_After_3.5_right, OFFON_After_3.5_right, ONON_After_3.5_right)) %>%
      mutate(ONOFF_After_3.5=ONOFF_After_3.5_left+ONOFF_After_3.5_right) %>%
      mutate(OFFON_After_3.5=OFFON_After_3.5_left+OFFON_After_3.5_right) %>%
      mutate(OFF_After_3.5=OFF_After_3.5_left+OFF_After_3.5_right) %>%
      mutate(ON_After_3.5=ONON_After_3.5_left+ONON_After_3.5_right) %>%
            select(SUBJID, OFF_After_3.5, ONOFF_After_3.5, OFFON_After_3.5, ON_After_3.5) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  inner_join(
    Item3.6_left %>% select(SUBJID, ONOFF_After_3.6_left, ONOFF_After_3.6_left, OFF_After_3.6_left, OFFON_After_3.6_left, ONON_After_3.6_left) %>%
      inner_join(Item3.6_right %>% select(SUBJID, ONOFF_After_3.6_right, OFF_After_3.6_right, OFFON_After_3.6_right, ONON_After_3.6_right)) %>%
      mutate(ONOFF_After_3.6=ONOFF_After_3.6_left+ONOFF_After_3.6_right) %>%
      mutate(OFFON_After_3.6=OFFON_After_3.6_left+OFFON_After_3.6_right) %>%
      mutate(OFF_After_3.6=OFF_After_3.6_left+OFF_After_3.6_right) %>%
      mutate(ON_After_3.6=ONON_After_3.6_left+ONON_After_3.6_right) %>%
      select(SUBJID, OFF_After_3.6, ONOFF_After_3.6, OFFON_After_3.6, ON_After_3.6) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_After_BradyUp=OFF_After_3.6+OFF_After_3.5+OFF_After_3.4) %>%
  mutate(ONOFF_After_BradyUp=ONOFF_After_3.6+ONOFF_After_3.5+ONOFF_After_3.4) %>%
  mutate(OFFON_After_BradyUp=OFFON_After_3.6+OFFON_After_3.5+OFFON_After_3.4) %>%
  mutate(ON_After_BradyUp=ON_After_3.6+ON_After_3.5+ON_After_3.4,) %>%
  select(SUBJID, OFF_After_BradyUp, ONOFF_After_BradyUp, OFFON_After_BradyUp, ON_After_BradyUp) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))




BradyDown <- Item3.7_left %>% select(SUBJID, ONOFF_After_3.7_left, ONOFF_After_3.7_left, OFF_After_3.7_left, OFFON_After_3.7_left, ONON_After_3.7_left) %>%
  inner_join(Item3.7_right %>% select(SUBJID, ONOFF_After_3.7_right, OFF_After_3.7_right, OFFON_After_3.7_right, ONON_After_3.7_right)) %>%
  mutate(ONOFF_After_3.7=ONOFF_After_3.7_left+ONOFF_After_3.7_right) %>%
  mutate(OFFON_After_3.7=OFFON_After_3.7_left+OFFON_After_3.7_right) %>%
  mutate(OFF_After_3.7=OFF_After_3.7_left+OFF_After_3.7_right) %>%
  mutate(ON_After_3.7=ONON_After_3.7_left+ONON_After_3.7_right) %>%
    select(SUBJID, OFF_After_3.7, ONOFF_After_3.7, OFFON_After_3.7, ON_After_3.7) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.8_left %>% select(SUBJID, ONOFF_After_3.8_left, ONOFF_After_3.8_left, OFF_After_3.8_left, OFFON_After_3.8_left, ONON_After_3.8_left) %>%
      inner_join(Item3.8_right %>% select(SUBJID, ONOFF_After_3.8_right, OFF_After_3.8_right, OFFON_After_3.8_right, ONON_After_3.8_right)) %>%
      mutate(ONOFF_After_3.8=ONOFF_After_3.8_left+ONOFF_After_3.8_right) %>%
      mutate(OFFON_After_3.8=OFFON_After_3.8_left+OFFON_After_3.8_right) %>%
      mutate(OFF_After_3.8=OFF_After_3.8_left+OFF_After_3.8_right) %>%
      mutate(ON_After_3.8=ONON_After_3.8_left+ONON_After_3.8_right) %>%
      select(SUBJID, OFF_After_3.8, ONOFF_After_3.8, OFFON_After_3.8, ON_After_3.8) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_After_BradyDown=OFF_After_3.7+OFF_After_3.8) %>%
  mutate(ONOFF_After_BradyDown=ONOFF_After_3.7+ONOFF_After_3.8) %>%
  mutate(OFFON_After_BradyDown=OFFON_After_3.7+OFFON_After_3.8) %>%
  mutate(ON_After_BradyDown=ON_After_3.8+ON_After_3.8) %>%
    select(SUBJID, OFF_After_BradyDown, ONOFF_After_BradyDown, OFFON_After_BradyDown, ON_After_BradyDown) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


Brady_scores <- BradyDown %>% inner_join(BradyUp) %>%
  mutate(OFF_After_Brady=OFF_After_BradyDown+OFF_After_BradyUp) %>%
  mutate(ONOFF_After_Brady=ONOFF_After_BradyDown+ONOFF_After_BradyUp) %>%
  mutate(OFFON_After_Brady=OFFON_After_BradyDown+OFFON_After_BradyUp) %>%
  mutate(ON_After_Brady=ON_After_BradyDown+ON_After_BradyUp) %>%
  select(SUBJID, OFF_After_Brady, ONOFF_After_Brady, OFFON_After_Brady, ON_After_Brady)


Brady_V1 <- Brady_scores %>% select(SUBJID, ON_After_Brady) %>% rename("ONON_V1"="ON_After_Brady")
Brady_scores <- Brady_scores %>% drop_na() # 515


UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)

Item3.4_Right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.4_Right_, ON_3.4_Right_60)
Item3.4_Right <- Item3.4_Right[-1,]
names(Item3.4_Right) <- c("SUBJID", "OFF_3.4_Before_Right", "ON_3.4_Before_Right")
Item3.4_Right <- data.frame(Item3.4_Right) %>% mutate_each(as.numeric, OFF_3.4_Before_Right:ON_3.4_Before_Right)

Item3.4_Left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.4_Left_, ON_3.4_Left_60)
Item3.4_Left <- Item3.4_Left[-1,]
names(Item3.4_Left) <- c("SUBJID", "OFF_3.4_Before_Left", "ON_3.4_Before_Left")
Item3.4_Left <- data.frame(Item3.4_Left) %>% mutate_each(as.numeric, OFF_3.4_Before_Left:ON_3.4_Before_Left)


Item3.5_Right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.5_Right_, ON_3.5_Right_60)
Item3.5_Right <- Item3.5_Right[-1,]
names(Item3.5_Right) <- c("SUBJID", "OFF_3.5_Before_Right", "ON_3.5_Before_Right")
Item3.5_Right <- data.frame(Item3.5_Right) %>% mutate_each(as.numeric, OFF_3.5_Before_Right:ON_3.5_Before_Right)

Item3.5_Left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.5_Left_, ON_3.5_Left_60)
Item3.5_Left <- Item3.5_Left[-1,]
names(Item3.5_Left) <- c("SUBJID", "OFF_3.5_Before_Left", "ON_3.5_Before_Left")
Item3.5_Left <- data.frame(Item3.5_Left) %>% mutate_each(as.numeric, OFF_3.5_Before_Left:ON_3.5_Before_Left)


Item3.6_Right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.6_Right_, ON_3.6_Right_60)
Item3.6_Right <- Item3.6_Right[-1,]
names(Item3.6_Right) <- c("SUBJID", "OFF_3.6_Before_Right", "ON_3.6_Before_Right")
Item3.6_Right <- data.frame(Item3.6_Right) %>% mutate_each(as.numeric, OFF_3.6_Before_Right:ON_3.6_Before_Right)

Item3.6_Left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.6_Left_, ON_3.6_Left_60)
Item3.6_Left <- Item3.6_Left[-1,]
names(Item3.6_Left) <- c("SUBJID", "OFF_3.6_Before_Left", "ON_3.6_Before_Left")
Item3.6_Left <- data.frame(Item3.6_Left) %>% mutate_each(as.numeric, OFF_3.6_Before_Left:ON_3.6_Before_Left)


Item3.7_Right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.7_Right_, ON_3.7_Right_60)
Item3.7_Right <- Item3.7_Right[-1,]
names(Item3.7_Right) <- c("SUBJID", "OFF_3.7_Before_Right", "ON_3.7_Before_Right")
Item3.7_Right <- data.frame(Item3.7_Right) %>% mutate_each(as.numeric, OFF_3.7_Before_Right:ON_3.7_Before_Right)

Item3.7_Left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.7_Left, ON_3.7_Left60)
Item3.7_Left <- Item3.7_Left[-1,]
names(Item3.7_Left) <- c("SUBJID", "OFF_3.7_Before_Left", "ON_3.7_Before_Left")
Item3.7_Left <- data.frame(Item3.7_Left) %>% mutate_each(as.numeric, OFF_3.7_Before_Left:ON_3.7_Before_Left)



Item3.8_Right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.8_Right_, ON_3.8_Right_3)
Item3.8_Right <- Item3.8_Right[-1,]
names(Item3.8_Right) <- c("SUBJID", "OFF_3.8_Before_Right", "ON_3.8_Before_Right")
Item3.8_Right <- data.frame(Item3.8_Right) %>% mutate_each(as.numeric, OFF_3.8_Before_Right:ON_3.8_Before_Right)

Item3.8_Left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.8_Left, ON_3.8_Left3)
Item3.8_Left <- Item3.8_Left[-1,]
names(Item3.8_Left) <- c("SUBJID", "OFF_3.8_Before_Left", "ON_3.8_Before_Left")
Item3.8_Left <- data.frame(Item3.8_Left) %>% mutate_each(as.numeric, OFF_3.8_Before_Left:ON_3.8_Before_Left)


names(Item3.4_Left)

BradyUp <- Item3.4_Left %>% select(SUBJID, OFF_3.4_Before_Left, ON_3.4_Before_Left) %>%
  inner_join(Item3.4_Right %>% select(SUBJID, OFF_3.4_Before_Right, ON_3.4_Before_Right)) %>%
  mutate(OFF_3.4_Before=OFF_3.4_Before_Left+OFF_3.4_Before_Right) %>%
  mutate(ON_3.4_Before=ON_3.4_Before_Left+ON_3.4_Before_Right) %>%
  select(SUBJID, OFF_3.4_Before, ON_3.4_Before) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.5_Left %>% select(SUBJID, OFF_3.5_Before_Left, ON_3.5_Before_Left) %>%
      inner_join(Item3.5_Right %>% select(SUBJID, OFF_3.5_Before_Right, ON_3.5_Before_Right)) %>%
      mutate(OFF_3.5_Before=OFF_3.5_Before_Left+OFF_3.5_Before_Right) %>%
      mutate(ON_3.5_Before=ON_3.5_Before_Left+ON_3.5_Before_Right) %>%
      select(SUBJID, OFF_3.5_Before, ON_3.5_Before) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  inner_join(
    Item3.6_Left %>% select(SUBJID, OFF_3.6_Before_Left, ON_3.6_Before_Left) %>%
      inner_join(Item3.6_Right %>% select(SUBJID, OFF_3.6_Before_Right, ON_3.6_Before_Right)) %>%
      mutate(OFF_3.6_Before=OFF_3.6_Before_Left+OFF_3.6_Before_Right) %>%
      mutate(ON_3.6_Before=ON_3.6_Before_Left+ON_3.6_Before_Right) %>%
      select(SUBJID, OFF_3.6_Before, ON_3.6_Before) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_Before_BradyUp=OFF_3.4_Before +OFF_3.5_Before +OFF_3.6_Before ) %>%
  mutate(ON_Before_BradyUp=ON_3.4_Before +ON_3.5_Before +ON_3.6_Before) %>%
  select(SUBJID, OFF_Before_BradyUp, ON_Before_BradyUp) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))



BradyDown <- Item3.7_Left %>% select(SUBJID, OFF_3.7_Before_Left, ON_3.7_Before_Left) %>%
  inner_join(Item3.7_Right %>% select(SUBJID, OFF_3.7_Before_Right, ON_3.7_Before_Right)) %>%
  mutate(OFF_3.7_Before=OFF_3.7_Before_Left+OFF_3.7_Before_Right) %>%
  mutate(ON_3.7_Before=ON_3.7_Before_Left+ON_3.7_Before_Right) %>%
  select(SUBJID, OFF_3.7_Before, ON_3.7_Before) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.8_Left %>% select(SUBJID, OFF_3.8_Before_Left, ON_3.8_Before_Left) %>%
      inner_join(Item3.8_Right %>% select(SUBJID, OFF_3.8_Before_Right, ON_3.8_Before_Right)) %>%
      mutate(OFF_3.8_Before=OFF_3.8_Before_Left+OFF_3.8_Before_Right) %>%
      mutate(ON_3.8_Before=ON_3.8_Before_Left+ON_3.8_Before_Right) %>%
      select(SUBJID, OFF_3.8_Before, ON_3.8_Before) %>%
      drop_na() %>%
      filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_Before_BradyDown=OFF_3.7_Before  +OFF_3.8_Before  ) %>%
  mutate(ON_Before_BradyDown=ON_3.7_Before  +ON_3.8_Before ) %>%
  select(SUBJID, OFF_Before_BradyDown, ON_Before_BradyDown) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))




Brady_scores_pre_op <- BradyDown %>% inner_join(BradyUp) %>%
  mutate(OFF_Before_Brady=OFF_Before_BradyDown +OFF_Before_BradyUp ) %>%
  mutate(ON_Before_Brady=ON_Before_BradyDown +ON_Before_BradyUp) %>%
  select(SUBJID, OFF_Before_Brady, ON_Before_Brady)


Brady_scores_pre_op <- Brady_scores_pre_op %>% drop_na() #  578


Brady_V0 <- Brady_scores_pre_op %>% rename("OFF_V0"="OFF_Before_Brady", "ON_V0"="ON_Before_Brady")

Brady_scores








Brady_scores_pre_op
names(Brady_scores_pre_op)
Brady_scores_post_op <- Brady_scores %>% select(SUBJID, OFF_After_Brady, ON_After_Brady)
names(Brady_scores_post_op)


Brady_scores_pre_vs_post <- Brady_scores_pre_op %>% inner_join(Brady_scores_post_op)

Brady_scores_pre_vs_post # 372



UPDRSIII_COMPLET_V3_V5 <- readxl::read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA, ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE)

unique(UPDRSIII_COMPLET_V3_V5$VISIT)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 3 ans - V3")  %>% select(-VISIT)


UPDRSIII_COMPLET_V3_V5$ON_MS_DROIT_DOIGT <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MS_DROIT_DOIGT) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_DOIGT  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_DOIGT ) 
UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MAINS  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MAINS ) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MAINS  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MAINS ) 
UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MA  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MA ) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MA  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MA ) 
UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_PIED  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_PIED ) 
UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_PIED  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_PIED ) 
UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_JAMBE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_JAMBE ) 
UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_JAMBE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_JAMBE ) 

Brady_V3 <- UPDRSIII_COMPLET_V3_V5 %>% mutate(Brady_V3=ON_MS_DROIT_DOIGT+ON_MSGCHE_DOIGT+ON_MSDROIT_MAINS+
                                                ON_MSGCHE_MAINS+ON_MSDROIT_MA+ON_MSGCHE_MA+
                                                ON_MIDROIT_PIED+ON_MIGCHE_PIED+ON_MIDROIT_JAMBE+ON_MIGCHE_JAMBE) %>% drop_na() %>%
  select(SUBJID, Brady_V3)



UPDRSIII_COMPLET_V3_V5 <- readxl::read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA, ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE)

 
unique(UPDRSIII_COMPLET_V3_V5$VISIT)

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% filter(VISIT=="Visite Bilan à 5 ans - V5")  %>% select(-VISIT)


UPDRSIII_COMPLET_V3_V5$ON_MS_DROIT_DOIGT <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MS_DROIT_DOIGT) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_DOIGT  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_DOIGT ) 
UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MAINS  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MAINS ) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MAINS  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MAINS ) 
UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MA  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSDROIT_MA ) 
UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MA  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MSGCHE_MA ) 
UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_PIED  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_PIED ) 
UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_PIED  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_PIED ) 
UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_JAMBE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIDROIT_JAMBE ) 
UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_JAMBE  <- as.numeric(UPDRSIII_COMPLET_V3_V5$ON_MIGCHE_JAMBE ) 

Brady_V5 <- UPDRSIII_COMPLET_V3_V5 %>% mutate(Brady_V5=ON_MS_DROIT_DOIGT+ON_MSGCHE_DOIGT+ON_MSDROIT_MAINS+
                                                ON_MSGCHE_MAINS+ON_MSDROIT_MA+ON_MSGCHE_MA+
                                                ON_MIDROIT_PIED+ON_MIGCHE_PIED+ON_MIDROIT_JAMBE+ON_MIGCHE_JAMBE) %>% drop_na() %>%
  select(SUBJID, Brady_V5)



Brady <- Brady_V5 %>% inner_join(Brady_V3) %>% inner_join(Brady_V1) %>% inner_join(Brady_V0) 

names(Brady) <- c("SUBJID", "ONON_V5", "ONON_V3", "ONON_V1", "OFF_V0", "ON_V0")



# Reshape data to long format
data_long <- Brady %>% 
  rename("[V0] OFF"="OFF_V0", "[V0] ON"="ON_V0", "[V1] ON"="ONON_V1", "[V3] ON"="ONON_V3", "[V5] ON"="ONON_V5") %>%
  pivot_longer(cols = c( "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON"),
               names_to = "Condition",
               values_to = "Value")


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#                      [V0] Bradykinesia OFF [V0] Bradykinesia ON [V1] Bradykinesia ON [V3] Bradykinesia ON
# [V0] Bradykinesia ON < 2e-16               -                    -                    -                   
# [V1] Bradykinesia ON < 2e-16               0.29                 -                    -                   
# [V3] Bradykinesia ON < 2e-16               1.9e-06              3.0e-10              -                   
# [V5] Bradykinesia ON 4.9e-14               2.5e-08              1.6e-13              0.23                
# 
# P value adjustment method: bonferroni 



# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS Bradykinesia scores \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)



# UPDRS III total ---------



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

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX

# Define the relevant columns
columns_to_check <- c("ON_15min_Before", "ON_30min_Before", "ON_45min_Before", 
                      "ON_60min_Before", "ON_90min_Before", "ON_120min_Before")

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% select(SUBJID, TOT_OFF_DRUG_V0, OFF_TOTALCALC_V1, EVAL_MOT_BESTON_V0, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% drop_na()


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
  rename("[V0] OFF"="OFF_V0", "[V0] ON"="ON_V0", "[V1] ON"="ONON_V1", "[V3] ON"="ONON_V3", "[V5] ON"="ONON_V5") %>%
  pivot_longer(cols = c( "[V0] OFF", "[V0] ON", "[V1] ON", "[V3] ON", "[V5] ON"),
               names_to = "Condition",
               values_to = "Value")


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#                       [V0] MDS-UPDRS III OFF [V0] MDS-UPDRS III ON [V1] MDS-UPDRS III ON [V3] MDS-UPDRS III ON
# [V0] MDS-UPDRS III ON < 2e-16                -                     -                     -                    
# [V1] MDS-UPDRS III ON < 2e-16                1.00000               -                     -                    
# [V3] MDS-UPDRS III ON < 2e-16                1.3e-13               < 2e-16               -                    
# [V5] MDS-UPDRS III ON < 2e-16                < 2e-16               < 2e-16               0.00037              
# 
# P value adjustment method: bonferroni 



# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )




plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "MDS-UPDRS III Total score \n") +
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
  scale_color_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c("#bfcbd4", "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.11_postops.svg", plot=plot1, width=4, height=5)



