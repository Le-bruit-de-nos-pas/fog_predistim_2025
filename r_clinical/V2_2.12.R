library(readxl)
library(tidyverse)
library(data.table)


UPDRSII_OFF_2.12 <- fread("data/UPDRSII_OFF_2.12.txt")
UPDRSII_ON_2.12 <- fread("data/UPDRSII_ON_2.12.txt")


UPDRSII_V0_2.12 <- fread("data/UPDRSII_V0_2.12.txt")
UPDRSII_V1_2.12 <- fread("data/UPDRSII_V1_2.12.txt")

UPDRSII_V0_V1_2.12 <- UPDRSII_V0_2.12 %>% inner_join(UPDRSII_V1_2.12)





# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(UPDRSII_V0_V1_2.12$V0_MDS2_12OFF  , UPDRSII_V0_V1_2.12$V0_MDS2_12ON  , paired = TRUE, alternative = "two.sided")

# 
# 	Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSII_V0_V1_2.12$V0_MDS2_12OFF and UPDRSII_V0_V1_2.12$V0_MDS2_12ON
# V = 10158, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(UPDRSII_V0_V1_2.12$V1_MDS2_12OFF   , UPDRSII_V0_V1_2.12$V1_MDS2_12ON    , paired = TRUE, alternative = "two.sided")


# 	Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSII_V0_V1_2.12$V1_MDS2_12OFF and UPDRSII_V0_V1_2.12$V1_MDS2_12ON
# V = 6990.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

before_after_off_on <- UPDRSII_V0_V1_2.12  %>%
  pivot_longer(cols = c("V1_MDS2_12OFF","V1_MDS2_12ON", "V0_MDS2_12OFF", "V0_MDS2_12ON"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="V0_MDS2_12ON"|Condition=="V0_MDS2_12OFF", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="V0_MDS2_12OFF"|Condition=="V1_MDS2_12OFF", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, height=0.1, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
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
  scale_color_manual(values=c( "#193a71", "#193a71", "#b5667b", "#b5667b")) +
  scale_fill_manual(values=c( "#193a71", "#193a71", "#b5667b", "#b5667b")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_pre_postops_offon.svg", plot=plot1, width=4, height=5)





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
#       Effect     df  MSE          F   ges p.value
# 1       Time 1, 181 0.62       0.64 <.001    .426
# 2      State 1, 181 0.50 351.29 ***  .257   <.001
# 3 Time:State 1, 181 0.33  14.55 ***  .009   <.001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

library(emmeans)

emm <- emmeans(anova_res, ~ Time * State)

# OFF vs ON within each Time
contrast(emm, method = "pairwise", by = "Time", adjust = "bonferroni")

# Time = V1:
#  contrast estimate     SE  df t.ratio p.value
#  OFF - ON    0.703 0.0642 181  10.947  <.0001
# 
# Time = V0:
#  contrast estimate     SE  df t.ratio p.value
#  OFF - ON    0.885 0.0788 181  11.225  <.0001
 
# Before vs After within each State
contrast(emm, method = "pairwise", by = "State", adjust = "bonferroni")

#State = OFF:
#  contrast estimate     SE  df t.ratio p.value
#  V1 - V0    -0.209 0.0817 181  -2.555  0.0115
# 
# State = ON:
#  contrast estimate     SE  df t.ratio p.value
#  V1 - V0     0.115 0.0615 181   1.876  0.0622






# Calculate percentage breakdown of each score per condition
percentage_stats <- before_after_off_on %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 182) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_12OFF", "V0 [OFF]",
                                          ifelse(Condition=="V0_MDS2_12ON", "V0 [ON]",
                                                 ifelse(Condition=="V1_MDS2_12OFF", "V1 [OFF]", "V1 [ON]")))) %>%
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







