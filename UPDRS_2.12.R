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


UPDRSII <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRS II", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSII <- UPDRSII %>% select(SUBJID, VISIT, MDS2_12OFF, MDS2_12ON)

UPDRSII <- UPDRSII[-1,]

UPDRSII <- UPDRSII %>% filter(VISIT=="Visite de screening") %>% select(-VISIT) %>%
  rename("V0_MDS2_12OFF"="MDS2_12OFF", "V0_MDS2_12ON"="MDS2_12ON") %>%
  full_join(UPDRSII %>% filter(VISIT=="Visite Bilan à 1 an - V1") %>% select(-VISIT) %>%
  rename("V1_MDS2_12OFF"="MDS2_12OFF", "V1_MDS2_12ON"="MDS2_12ON"))


UPDRSII <- UPDRSII %>% mutate(V0_MDS2_12OFF=ifelse(V0_MDS2_12OFF=="Normal",0,
                                        ifelse(V0_MDS2_12OFF=="Minime",1,
                                               ifelse(V0_MDS2_12OFF=="Léger", 2,
                                                      ifelse(V0_MDS2_12OFF=="Modéré",3,
                                                             ifelse(V0_MDS2_12OFF=="Sévère",4,NA)))))) %>%
   mutate(V0_MDS2_12ON=ifelse(V0_MDS2_12ON=="Normal",0,
                                        ifelse(V0_MDS2_12ON=="Minime",1,
                                               ifelse(V0_MDS2_12ON=="Léger", 2,
                                                      ifelse(V0_MDS2_12ON=="Modéré",3,
                                                             ifelse(V0_MDS2_12ON=="Sévère",4,NA)))))) %>%
   mutate(V1_MDS2_12OFF=ifelse(V1_MDS2_12OFF=="Normal",0,
                                        ifelse(V1_MDS2_12OFF=="Minime",1,
                                               ifelse(V1_MDS2_12OFF=="Léger", 2,
                                                      ifelse(V1_MDS2_12OFF=="Modéré",3,
                                                             ifelse(V1_MDS2_12OFF=="Sévère",4,NA)))))) %>%
   mutate(V1_MDS2_12ON=ifelse(V1_MDS2_12ON=="Normal",0,
                                        ifelse(V1_MDS2_12ON=="Minime",1,
                                               ifelse(V1_MDS2_12ON=="Léger", 2,
                                                      ifelse(V1_MDS2_12ON=="Modéré",3,
                                                             ifelse(V1_MDS2_12ON=="Sévère",4,NA))))))


UPDRSII <- data.frame(UPDRSII) %>% mutate_each(as.numeric, V0_MDS2_12OFF :V1_MDS2_12ON)


UPDRSII_V0 <- UPDRSII %>% select(SUBJID, V0_MDS2_12OFF, V0_MDS2_12ON) %>% drop_na() 

UPDRSII_V1 <- UPDRSII %>% select(SUBJID, V1_MDS2_12OFF, V1_MDS2_12ON) %>% drop_na() 

UPDRSII_OFF <- UPDRSII %>% select(SUBJID, V0_MDS2_12OFF, V1_MDS2_12OFF) %>% drop_na() 

UPDRSII_ON <- UPDRSII %>% select(SUBJID, V0_MDS2_12ON, V1_MDS2_12ON) %>% drop_na() 

UPDRSII <- UPDRSII %>% drop_na()



fwrite(UPDRSII_V0, "data/UPDRSII_V0_2.12.txt")
fwrite(UPDRSII_V1, "data/UPDRSII_V1_2.12.txt")
fwrite(UPDRSII_OFF, "data/UPDRSII_OFF_2.12.txt")
fwrite(UPDRSII_ON, "data/UPDRSII_ON_2.12.txt")
fwrite(UPDRSII, "data/UPDRSII_2.12.txt")

length(unique(UPDRSII_V0$SUBJID)) # 287
length(unique(UPDRSII_V1$SUBJID)) # 237
length(unique(UPDRSII_OFF$SUBJID)) # 186
length(unique(UPDRSII_ON$SUBJID)) # 231
length(unique(UPDRSII$SUBJID)) # 182




summary_stats <- sapply(UPDRSII_V0, function(col) {
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


summary_stats <- sapply(UPDRSII, function(col) {
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




UPDRSII

# Reshape data to long format
data_long <- UPDRSII %>%
  pivot_longer(cols = c(V0_MDS2_12OFF, V0_MDS2_12ON, V1_MDS2_12OFF, V1_MDS2_12ON),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(UPDRSII[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(UPDRSII[, 2:5])
# Friedman chi-squared = 263.93, df = 3, p-value < 2.2e-16

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#               V0_MDS2_12OFF V0_MDS2_12ON V1_MDS2_12OFF
# V0_MDS2_12ON  <2e-16        -            -            
# V1_MDS2_12OFF 0.076         <2e-16       -            
# V1_MDS2_12ON  <2e-16        0.342        <2e-16       
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
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_12OFF", "V0 2.12 OFF",
                                          ifelse(Condition=="V0_MDS2_12ON", "V0 2.12 ON",
                                                 ifelse(Condition=="V1_MDS2_12OFF", "V1 2.12 OFF", "V1 2.12 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.12 OFF", "V0 2.12 ON","V1 2.12 OFF", "V1 2.12 ON"))) %>%
ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n Condition",
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
  mutate(Percentage = (Count / 182) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_12OFF", "V0 2.12 OFF",
                                          ifelse(Condition=="V0_MDS2_12ON", "V0 2.12 ON",
                                                 ifelse(Condition=="V1_MDS2_12OFF", "V1 2.12 OFF", "V1 2.12 ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 2.12 OFF", "V1 2.12 OFF", "V0 2.12 ON", "V1 2.12 ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Condition",
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


ggsave(file="p2.svg", plot=plot1, width=4, height=5)

