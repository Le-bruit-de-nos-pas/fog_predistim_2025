library(tidyverse)
library(data.table)

UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)


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
names(Brady_scores)

mean(Brady_scores$OFF_After_Brady,na.rm = T)
mean(Brady_scores$OFFON_After_Brady,na.rm = T)
mean(Brady_scores$ONOFF_After_Brady,na.rm = T)
mean(Brady_scores$ON_After_Brady,na.rm = T)


summary_stats <- sapply(Brady_scores, function(col) {
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
data_long <- Brady_scores %>%
  pivot_longer(cols = c(OFF_After_Brady, ONOFF_After_Brady, OFFON_After_Brady, ON_After_Brady),
               names_to = "Condition",
               values_to = "Value")



friedman.test(as.matrix(Brady_scores[, 2:5]))  # Assuming columns 2:5 are conditions

# Friedman rank sum test
# 
# data:  as.matrix(Brady_scores[, 2:5])
# Friedman chi-squared = 1216.5, df = 3, p-value < 2.2e-16


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")


# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#                 OFF_After_Brady OFFON_After_Brady ON_After_Brady
# OFFON_After_Brady < 2e-16         -                 -             
#   ON_After_Brady    < 2e-16         < 2e-16           -             
#   ONOFF_After_Brady < 2e-16         2.5e-06           < 2e-16       

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
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_After_Brady", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After_Brady", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After_Brady", "Med-OFF/DBS-ON", "ON/ON")))) %>%
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


ggsave(file="brady_postops.svg", plot=plot1, width=3, height=5)






# ************************************
# Before vs After surgery *************
# ************************************


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



percent_deltas <- Brady_V0 %>% 
  mutate(percent_before=100*(OFF_V0  -ON_V0)/OFF_V0  )  %>%
  select(SUBJID, percent_before) %>%
  inner_join( 
    Brady_scores %>%
     mutate(percent_after_LD=100*(OFF_After_Brady  -OFFON_After_Brady  )/OFF_After_Brady  ) %>%
     mutate(percent_after_DBS=100*(OFF_After_Brady  -ONOFF_After_Brady  )/OFF_After_Brady  ) %>%
     mutate(percent_after_Both=100*(OFF_After_Brady  -ON_After_Brady)/OFF_After_Brady  )  %>%
     select(SUBJID, percent_after_LD, percent_after_DBS, percent_after_Both)
  ) %>% drop_na()




cor.test(percent_deltas$percent_before, percent_deltas$percent_after_LD, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_LD
# S = 6850675, p-value = 0.0004454
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1818956 
 	
cor.test(percent_deltas$percent_before, percent_deltas$percent_after_DBS, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_DBS
# S = 7855331, p-value = 0.2354
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.06192009 

cor.test(percent_deltas$percent_before, percent_deltas$percent_after_Both, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  percent_deltas$percent_before and percent_deltas$percent_after_Both
# S = 6502882, p-value = 1.476e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2234289 







Brady_scores_pre_op
names(Brady_scores_pre_op)
Brady_scores_post_op <- Brady_scores %>% select(SUBJID, OFF_After_Brady, ON_After_Brady)
names(Brady_scores_post_op)


Brady_scores_pre_vs_post <- Brady_scores_pre_op %>% inner_join(Brady_scores_post_op)

Brady_scores_pre_vs_post # 372




summary_stats <- sapply(Brady_scores_pre_vs_post, function(col) {
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
wilcox.test(Brady_scores_pre_vs_post$OFF_Before_Brady, Brady_scores_pre_vs_post$OFF_After_Brady     , paired = TRUE, alternative = "two.sided")


# Wilcoxon signed rank test with continuity correction
# 
# data:  Brady_scores_pre_vs_post$OFF_Before_Brady and Brady_scores_pre_vs_post$OFF_After_Brady
# V = 23328, p-value = 0.000325
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(Brady_scores_pre_vs_post$ON_Before_Brady   , Brady_scores_pre_vs_post$ON_After_Brady       , paired = TRUE, alternative = "two.sided")

# Wilcoxon signed rank test with continuity correction
# 
# data:  Brady_scores_pre_vs_post$ON_Before_Brady and Brady_scores_pre_vs_post$ON_After_Brady
# V = 35496, p-value = 1.948e-05
# alternative hypothesis: true location shift is not equal to 0

# Reshape the data into long format
long_data <- Brady_scores_pre_vs_post %>%
  pivot_longer(cols = c(OFF_Before_Brady, ON_Before_Brady, OFF_After_Brady, ON_After_Brady),
               names_to = c("Condition"),
               values_to = "Value") %>%
  mutate(Group = ifelse(grepl("Before", Condition), "Before", "After"),
         Measure = case_when(
           grepl("OFF", Condition) ~ "OFF",
           grepl("ON", Condition) ~ "ON"
         ))


# Wilcoxon Signed-Rank Test
wilcox.test(Value ~ Group, data = filter(long_data, Measure == "OFF"), paired = TRUE)

# Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 36704, p-value = 0.000325
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(Value ~ Group, data = filter(long_data, Measure == "ON"), paired = TRUE)


# Wilcoxon signed rank test with continuity correction
# 
# data:  Value by Group
# V = 20450, p-value = 1.948e-05
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
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before_Brady", "OFF pre-op",
                                                   ifelse(Condition=="OFF_After_Brady", "OFF post-op",
                                                          ifelse(Condition=="ON_Before_Brady", "Best-ON pre-op", "Best-ON post-op")))) %>%
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


ggsave(file="brady_offsvsons.svg", plot=plot1, width=3, height=5)










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
# ONON_V1 < 2e-16 0.29    -       -      
# ONON_V3 < 2e-16 1.9e-06 3.0e-10 -      
# ONON_V5 4.9e-14 2.5e-08 1.6e-13 0.23   
# 
# P value adjustment method: bonferroni 


# Create the data as a matrix
mat <- matrix(c(
  "< 2e-16", NA,        NA,        NA,
  "< 2e-16", "0.29",    NA,        NA,
  "< 2e-16", "1.9e-06", "3.0e-10", NA,
  "4.9e-14", "2.5e-08", "1.6e-13", "0.23"
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
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="ON_V0", "V0 Brady ON",
                                                   ifelse(Condition=="OFF_V0", "V0 Brady OFF",
                                                      ifelse(Condition=="ONON_V1", "V1 Brady ON",
                                                           ifelse(Condition=="ONON_V3", "V3 Brady ON", "V5 Brady ON"))))) %>%
  mutate(Condition=factor(Condition, levels=c("V0 Brady OFF", "V0 Brady ON", "V1 Brady ON","V3 Brady ON", "V5 Brady ON"))) %>%
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


ggsave(file="bradys_vs.svg", plot=plot1, width=3, height=5)



