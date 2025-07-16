
library(tidyverse)
library(data.table)



# POST OPS **********************+

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
  pivot_longer(cols = c(OFF_After, ONOFF_After, OFFON_After, ONON_After),
               names_to = "Condition",
               values_to = "Value")



friedman.test(as.matrix(Axials[, 2:5]))  # Assuming columns 2:5 are conditions

# 	Friedman rank sum test
# 
# data:  as.matrix(Axials[, 2:5])
# Friedman chi-squared = 287.37, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")

#	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 

	
# data:  data_long$Value and data_long$Condition 
# 
#             OFF_After OFFON_After ONOFF_After
# OFFON_After < 2e-16   -           -          
# ONOFF_After < 2e-16   2.2e-13     -          
# ONON_After  < 2e-16   4.5e-05     0.89       
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


ggsave(file="axial_postops.svg", plot=plot1, width=3, height=5)

Axials_V1 <- Axials %>% select(SUBJID, ONON_After) %>% rename("ONON_V1"="ONON_After")



# BEFORE vs AFTER ********************

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



percent_deltas <- Axials_V0_OFF %>% inner_join(Axials_V0_ON) %>% 
  mutate(percent_before=100*(Axial_OFF_v0 -Axial_ON_v0)/Axial_OFF_v0 )  %>%
  select(SUBJID, percent_before) %>%
  inner_join( 
    Axials %>%
     mutate(percent_after_LD=100*(OFF_After -OFFON_After )/OFF_After ) %>%
     mutate(percent_after_DBS=100*(OFF_After -ONOFF_After )/OFF_After ) %>%
     mutate(percent_after_Both=100*(OFF_After -ONON_After)/OFF_After )  %>%
     select(SUBJID, percent_after_LD, percent_after_DBS, percent_after_Both)
  ) %>% drop_na()



cor.test(percent_deltas$percent_before, percent_deltas$percent_after_LD, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_LD
# S = 982542, p-value = 4.004e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2847486 
 	
cor.test(percent_deltas$percent_before, percent_deltas$percent_after_DBS, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_DBS
# S = 1120850, p-value = 0.008734
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1840659 


cor.test(percent_deltas$percent_before, percent_deltas$percent_after_Both, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_Both
# S = 1037259, p-value = 0.0004431
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2449164 







mean(Axials_V1_OFF$Axial_OFF_v1)


Axials <- Axials_V0_OFF %>% inner_join(Axials_V0_ON) %>%
  inner_join(Axials_V1_OFF) %>% inner_join(Axials_V1_ON)


# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(Axials$Axial_OFF_v0 , Axials$Axial_OFF_v1      , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Axials$Axial_OFF_v0 and Axials$Axial_OFF_v1
# V = 22628, p-value = 0.5142
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(Axials$Axial_ON_v0    , Axials$Axial_ON_v1       , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Axials$Axial_ON_v0 and Axials$Axial_ON_v1
# V = 4125, p-value = 8.023e-08
# alternative hypothesis: true location shift is not equal to 0



# Reshape the data into long format
long_data <- Axials %>%
  pivot_longer(cols = c(Axial_OFF_v0, Axial_ON_v0, Axial_OFF_v1, Axial_ON_v1),
               names_to = c("Condition"),
               values_to = "Value") %>%
  mutate(Group = ifelse(grepl("V0", Condition), "Before", "After"),
         Measure = case_when(
           grepl("OFF", Condition) ~ "OFF",
           grepl("ON", Condition) ~ "ON"
         ))


median(Axials$Axial_OFF_v0)
quantile(Axials$Axial_OFF_v0, 0.25)
quantile(Axials$Axial_OFF_v0, 0.75)


median(Axials$Axial_ON_v0)
quantile(Axials$Axial_ON_v0, 0.25)
quantile(Axials$Axial_ON_v0, 0.75)


# Calculate mean and standard error of the mean (SEM)
summary_stats <- long_data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


long_data %>% group_by(Condition) %>% summarise(mean=mean(Value))


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="Axial_OFF_v0", "OFF pre-op",
                                                   ifelse(Condition=="Axial_OFF_v1", "OFF post-op",
                                                          ifelse(Condition=="Axial_ON_v0", "Best-ON pre-op", "Best-ON post-op")))) %>%
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


ggsave(file="axial_offsvsons.svg", plot=plot1, width=3, height=5)


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



# Reshape data to long format
data_long <- Axials %>%
  pivot_longer(cols = c(ONON_V5, ONON_V3, ONON_V1, OFF_V0, ON_V0),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(Axials[, 2:6]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(Axials[, 2:6])
# Friedman chi-squared = 149.43, df = 4, p-value < 2.2e-16


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#         OFF_V0  ON_V0   ONON_V1 ONON_V3
# ON_V0   9.3e-14 -       -       -      
# ONON_V1 9.6e-09 6.3e-12 -       -      
# ONON_V3 8.8e-05 3.5e-09 1.000   -      
# ONON_V5 1.000   1.8e-10 0.086   0.002  
# 
# P value adjustment method: bonferroni 


# Load necessary packages
library(ggplot2)
library(reshape2)

# Create the new matrix with the updated values
mat <- matrix(c(
  "9.3e-14", NA,       NA,      NA,
  "9.6e-09", "6.3e-12", NA,     NA,
  "8.8e-05", "3.5e-09", "1.000", NA,
  "1.000",   "1.8e-10", "0.086", "0.002"
), nrow = 4, byrow = TRUE)

# Set row and column names
rownames(mat) <- c("ON_V0", "ONON_V1", "ONON_V3", "ONON_V5")
colnames(mat) <- c("OFF_V0", "ON_V0", "ONON_V1", "ONON_V3")

# Convert to data frame and melt for ggplot
df <- melt(mat, varnames = c("Row", "Col"), value.name = "Pvalue")

# Convert string to numeric for plotting (-log10 transformation)
df$Pvalue_num <- as.numeric(df$Pvalue)

# Plot the heatmap
plot <- ggplot(df, aes(x = Col, y = Row, fill = -log10(Pvalue_num))) +
  geom_tile(color = "white") +
  geom_text(aes(label = Pvalue), size = 4) +
  scale_fill_gradientn(
    colors = c("white", "#53a5cf", "#1f6ea5", "#024175"),
    na.value = "grey90",
    name = "-log10(p)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Multiplicity-adjusted p-values",
    x = "",
    y = ""
  )

ggsave(file="pvals.svg", plot=plot, width=5, height=5)




# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="ON_V0", "V0 Axial ON",
                                                   ifelse(Condition=="OFF_V0", "V0 Axial OFF",
                                                      ifelse(Condition=="ONON_V1", "V1 Axial ON",
                                                           ifelse(Condition=="ONON_V3", "V3 Axial ON", "V5 Axial ON"))))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 Axial OFF", "V0 Axial ON", "V1 Axial ON","V3 Axial ON", "V5 Axial ON"))) %>%
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


ggsave(file="axials_vs.svg", plot=plot1, width=3, height=5)

