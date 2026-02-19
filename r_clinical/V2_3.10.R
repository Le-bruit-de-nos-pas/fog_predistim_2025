library(readxl)
library(tidyverse)
library(data.table)


# Post OP 

UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

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

Item3.10_after <- Item3.10 %>% select(SUBJID, OFF_After, ONOFF_After, OFFON_After, ONON_After)

length(unique(Item3.10_after$SUBJID)) # 835

Item3.10_after <- Item3.10_after %>% drop_na()

length(unique(Item3.10_after$SUBJID)) # 520

# fwrite(Item3.10_after, "data/Item3.10_after.txt")

# Reshape data to long format
data_long <- Item3.10_after %>%
  pivot_longer(cols = c(OFF_After, ONOFF_After, OFFON_After, ONON_After),
               names_to = "Condition",
               values_to = "Value")


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


data_long %>% group_by(Condition) %>% summarise(mean=mean(Value))


plot1 <- data_long %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, height=0.1, shape=1, stroke=1) +
  theme_minimal() +
 labs(x = "\n Post-OP Condition",
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
  scale_color_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) +
  scale_fill_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="item3.10_postops.svg", plot=plot1, width=4, height=5)





# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 520) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))





percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Condition",
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

ggsave(file="p1.svg", plot=plot1, width=5, height=5)



# Before vs After surger

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


#fwrite(Item3.10_before_vs_after, "data/Item3.10_before_vs_after.txt")




before_after_off_on <- Item3.10_before_vs_after  %>%
  pivot_longer(cols = c("OFF_Before","OFF_After", "Min_ON_Before", "Min_ON_After"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="OFF_Before"|Condition=="Min_ON_Before", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="OFF_Before"|Condition=="OFF_After", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=TRUE) +
  geom_jitter(alpha=0.25, size=0.5, height=0.1, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
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
  scale_color_manual(values=c( "#193a71", "#193a71", "#b5667b", "#b5667b")) +
  scale_fill_manual(values=c( "#193a71", "#193a71", "#b5667b", "#b5667b")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_pre_postops_offon.svg", plot=plot1, width=4, height=5)



before_after_off_on %>% group_by(Condition) %>% summarise(mean=mean(Value))

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
#       Effect     df  MSE           F   ges p.value
# 1       Time 1, 529 0.43        0.00 <.001   >.999
# 2      State 1, 529 0.61 1468.04 ***  .390   <.001
# 3 Time:State 1, 529 0.29   17.77 ***  .004   <.001

library(emmeans)

emm <- emmeans(anova_res, ~ Time * State)

# OFF vs ON within each Time
contrast(emm, method = "pairwise", by = "Time", adjust = "bonferroni")

# Time = V0:
#  contrast estimate     SE  df t.ratio p.value
#  OFF - ON      1.4 0.0416 529  33.654  <.0001
# 
# Time = V1:
#  contrast estimate     SE  df t.ratio p.value
#  OFF - ON      1.2 0.0408 529  29.527  <.0001
 
# Before vs After within each State
contrast(emm, method = "pairwise", by = "State", adjust = "bonferroni")

# State = OFF:
#  contrast estimate     SE  df t.ratio p.value
#  V0 - V1    0.0981 0.0441 529   2.226  0.0265
# 
# State = ON:
#  contrast estimate     SE  df t.ratio p.value
#  V0 - V1   -0.0981 0.0278 529  -3.527  0.0005




# Calculate percentage breakdown of each score per condition
percentage_stats <- before_after_off_on %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 528) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before", "V0 [OFF]",
                                          ifelse(Condition=="Min_ON_Before", "V0 [ON]",
                                                 ifelse(Condition=="OFF_After", "V1 [OFF]", "V1 [ON]")))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 [OFF]", "V0 [ON]", "V1 [OFF]", "V1 [ON]"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats#  %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Condition",
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










