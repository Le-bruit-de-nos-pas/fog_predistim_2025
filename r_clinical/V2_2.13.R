library(readxl)
library(tidyverse)
library(data.table)


UPDRSII_OFF_2.13 <- fread("data/UPDRSII_OFF_2.13.txt")
UPDRSII_ON_2.13 <- fread("data/UPDRSII_ON_2.13.txt")


UPDRSII_V0_2.13 <- fread("data/UPDRSII_V0_2.13.txt")
UPDRSII_V1_2.13 <- fread("data/UPDRSII_V1_2.13.txt")

UPDRSII_V0_V1_2.13 <- UPDRSII_V0_2.13 %>% inner_join(UPDRSII_V1_2.13)





# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(UPDRSII_V0_V1_2.13$V0_MDS2_13OFF  , UPDRSII_V0_V1_2.13$V0_MDS2_13ON  , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSII_V0_V1_2.13$V0_MDS2_13OFF and UPDRSII_V0_V1_2.13$V0_MDS2_13ON
# V = 4560, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(UPDRSII_V0_V1_2.13$V1_MDS2_13OFF   , UPDRSII_V0_V1_2.13$V1_MDS2_13ON    , paired = TRUE, alternative = "two.sided")


# 	Wilcoxon signed rank test with continuity correction
# 
# data:  UPDRSII_V0_V1_2.13$V1_MDS2_13OFF and UPDRSII_V0_V1_2.13$V1_MDS2_13ON
# V = 4155, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

before_after_off_on <- UPDRSII_V0_V1_2.13  %>%
  pivot_longer(cols = c("V1_MDS2_13OFF","V1_MDS2_13ON", "V0_MDS2_13OFF", "V0_MDS2_13ON"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="V0_MDS2_13ON"|Condition=="V0_MDS2_13OFF", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="V0_MDS2_13OFF"|Condition=="V1_MDS2_13OFF", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, height = 0.1, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
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
# 1       Time 1, 181 0.67       0.25 <.001    .620
# 2      State 1, 181 0.62 184.54 ***  .164   <.001
# 3 Time:State 1, 181 0.32     4.69 *  .003    .032
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

# State = OFF:
#  contrast estimate     SE  df t.ratio p.value
#  V1 - V0   -0.1209 0.0944 181  -1.281  0.2018
# 
# State = ON:
#  contrast estimate     SE  df t.ratio p.value
#  V1 - V0    0.0604 0.0449 181   1.347  0.1797






# Calculate percentage breakdown of each score per condition
percentage_stats <- before_after_off_on %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 182) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="V0_MDS2_13OFF", "V0 [OFF]",
                                          ifelse(Condition=="V0_MDS2_13ON", "V0 [ON]",
                                                 ifelse(Condition=="V1_MDS2_13OFF", "V1 [OFF]", "V1 [ON]")))) %>%
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












sankey_df <- UPDRSII_V0_V1_2.13 %>%
  mutate(`[V0]`=ifelse(V0_MDS2_13OFF==0&V0_MDS2_13ON==0, "No FOG",
                      ifelse(V0_MDS2_13OFF!=0&V0_MDS2_13ON==0, "FOG OFF",
                             ifelse(V0_MDS2_13OFF==0&V0_MDS2_13ON!=0, "FOG ON", "Resistant FOG")))) %>%
  mutate(`[V1]`=ifelse(V1_MDS2_13OFF ==0&V1_MDS2_13ON==0, "No FOG",
                      ifelse(V1_MDS2_13OFF!=0&V1_MDS2_13ON==0, "FOG OFF",
                             ifelse(V1_MDS2_13OFF==0&V1_MDS2_13ON!=0, "FOG ON", "Resistant FOG"))))



unique(sankey_df$`[V0]`)
unique(sankey_df$`[V1]`)

FOG_2.13_bin_long <- sankey_df %>% select(SUBJID, `[V0]`, `[V1]`) %>%
  pivot_longer(
    cols = `[V0]`:`[V1]`,
    names_to = "Visit",
    values_to = "FOG"
  ) %>%
  mutate(
    Visit = factor(Visit, levels = c("[V0]", "[V1]")),
    FOG = factor(FOG, levels = c("No FOG", "FOG OFF", "FOG ON", "Resistant FOG"))
  )

unique(FOG_2.13_bin_long$FOG)

FOG_2.13_bin_long  %>%
  arrange(SUBJID, Visit) %>%
  group_by(SUBJID) %>%
  mutate(FOG_prev = lag(FOG)) %>%
  ungroup()  %>%
  filter(!is.na(FOG_prev)) %>%      # drops V0
  count(
    Transition = paste0(FOG_prev, " → ", FOG),
    From = FOG_prev,
    To   = FOG,
    Visit
  )
 

#   Transition                    From          To            Visit     n
#   <chr>                         <fct>         <fct>         <fct> <int>
# 1 FOG OFF → FOG OFF             FOG OFF       FOG OFF       [V1]     35
# 2 FOG OFF → No FOG              FOG OFF       No FOG        [V1]     23
# 3 FOG OFF → Resistant FOG       FOG OFF       Resistant FOG [V1]     21
# 4 No FOG → FOG OFF              No FOG        FOG OFF       [V1]     22
# 5 No FOG → No FOG               No FOG        No FOG        [V1]     52
# 6 No FOG → Resistant FOG        No FOG        Resistant FOG [V1]      4
# 7 Resistant FOG → FOG OFF       Resistant FOG FOG OFF       [V1]      6
# 8 Resistant FOG → No FOG        Resistant FOG No FOG        [V1]      6
# 9 Resistant FOG → Resistant FOG Resistant FOG Resistant FOG [V1]     13

library(ggalluvial)

plot <- ggplot(FOG_2.13_bin_long, aes(x = Visit, stratum = FOG, alluvium = SUBJID, fill = FOG)) +
  geom_flow(stat = "alluvium", alpha = 0.7) +
  geom_stratum(width = 0.5, color = "white", alpha=0.8) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4
  ) +
  scale_fill_manual(
    values = c("No FOG" = "#B0B0B5",
               "FOG OFF" = "#b5667b",
               "Resistant FOG" = "#193a71")
  ) +
  labs( x = "Visit", y = "Number of patients", fill = "FOG status"  ) +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(text = element_text(face = "bold"))

plot

ggsave(file="sankey.svg", plot=plot, width=6, height=4)

