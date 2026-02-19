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


Brady_scores <- Brady_scores %>% drop_na()

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




plot1 <- data_long %>%  mutate(Condition=ifelse(Condition=="OFF_After_Brady", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After_Brady", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After_Brady", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=TRUE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() +
 labs(x = "\n Post-OP Condition",
       y = "Bradykinesia sub-score \n") +
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

ggsave(file="brady_postops.svg", plot=plot1, width=4, height=5)




# Deltas 4 conditions post op 

deltas_df <- Brady_scores %>% mutate(`Med-OFF/DBS-ON`=100*(ONOFF_After_Brady -OFF_After_Brady  )/OFF_After_Brady ,
                               `Med-ON/DBS-OFF`=100*(OFFON_After_Brady  -OFF_After_Brady )/OFF_After_Brady ,
                               `ON/ON`=100*(ON_After_Brady-OFF_After_Brady )/OFF_After_Brady ) %>%
  select(SUBJID, `Med-OFF/DBS-ON`,`Med-ON/DBS-OFF`, `ON/ON`)


friedman.test(as.matrix(deltas_df[, 2:4]))  

# Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:4])
# Friedman chi-squared = 575.4, df = 2, p-value < 2.2e-16

 deltas_df_long <- deltas_df  %>%
  pivot_longer(cols = c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"),
               names_to = "Condition",
               values_to = "Value") 
 
pairwise.wilcox.test(deltas_df_long$Value, deltas_df_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  deltas_df_long$Value and deltas_df_long$Condition 
# 
#                Med-OFF/DBS-ON Med-ON/DBS-OFF
# Med-ON/DBS-OFF 3.8e-06        -             
# ON/ON          < 2e-16        < 2e-16       
# 
# P value adjustment method: bonferroni


plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=TRUE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() + 
  ylim(-120, 50) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Post-OP Condition",
       y = "% Drop Bradykinesia sub-score \n") +
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
  scale_color_manual(values=c( "#484c7a", "#725a84", "#986981")) +
  scale_fill_manual(values=c( "#484c7a", "#725a84", "#986981")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_postops_dorps.svg", plot=plot1, width=3, height=5)







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
Brady_scores_post_op <- Brady_scores %>% select(SUBJID, OFF_After_Brady, ON_After_Brady)



Brady_scores_pre_vs_post <- Brady_scores_pre_op %>% inner_join(Brady_scores_post_op)

Brady_scores_pre_vs_post # 372






# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(Brady_scores_pre_vs_post$OFF_Before_Brady  , Brady_scores_pre_vs_post$ON_Before_Brady  , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Brady_scores_pre_vs_post$OFF_Before_Brady and Brady_scores_pre_vs_post$ON_Before_Brady
# V = 67890, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(Brady_scores_pre_vs_post$OFF_After_Brady   , Brady_scores_pre_vs_post$ON_After_Brady    , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Brady_scores_pre_vs_post$OFF_After_Brady and Brady_scores_pre_vs_post$ON_After_Brady
# V = 69005, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

before_after_off_on <- Brady_scores_pre_vs_post  %>%
  pivot_longer(cols = c("OFF_Before_Brady","ON_Before_Brady", "OFF_After_Brady", "ON_After_Brady"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="OFF_Before_Brady"|Condition=="ON_Before_Brady", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="OFF_Before_Brady"|Condition=="OFF_After_Brady", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
       y = "Bradykinesia sub-score \n") +
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

Anova Table (Type 3 tests)

# Response: Value
#       Effect     df   MSE           F   ges p.value
# 1       Time 1, 371 32.76        1.04 <.001    .309
# 2      State 1, 371 27.98 2452.00 ***  .549   <.001
# 3 Time:State 1, 371 13.03   45.30 ***  .010   <.001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

library(emmeans)

emm <- emmeans(anova_res, ~ Time * State)

# OFF vs ON within each Time
contrast(emm, method = "pairwise", by = "Time", adjust = "bonferroni")

# Time = V0:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     12.3 0.312 371  39.437  <.0001
# 
# Time = V1:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     14.8 0.350 371  42.336  <.0001
 
# Before vs After within each State
contrast(emm, method = "pairwise", by = "State", adjust = "bonferroni")

# State = OFF:
#  contrast estimate    SE  df t.ratio p.value
#  V0 - V1    -1.562 0.413 371  -3.781  0.0002
# 
# State = ON:
#  contrast estimate    SE  df t.ratio p.value
#  V0 - V1     0.957 0.275 371   3.482  0.0006






deltas_df <- Brady_scores_pre_vs_post %>% mutate(`Drop V0`=100*(ON_Before_Brady  -OFF_Before_Brady  )/OFF_Before_Brady  ,
                               `Drop V1`=100*(ON_After_Brady -OFF_After_Brady  )/OFF_After_Brady  ) %>%
  select(SUBJID, `Drop V0`,`Drop V1`)


friedman.test(as.matrix(deltas_df[, 2:3]))  


# 	Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:3])
# Friedman chi-squared = 30.044, df = 1, p-value = 4.223e-08

deltas_df_long <- deltas_df  %>%
  pivot_longer(cols = c("Drop V0","Drop V1"),
               names_to = "Condition",
               values_to = "Value") 
 
pairwise.wilcox.test(deltas_df_long$Value, deltas_df_long$Condition, 
                     paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  deltas_df_long$Value and deltas_df_long$Condition 
# 
#         Drop V0
# Drop V1 2.2e-10
# 
# P value adjustment method: bonferroni 

plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Drop V0","Drop V1"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Drop V0", "Drop V1"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() + 
  #ylim(-120, 50) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Pre|Post Evaluation",
       y = "% Drop Bradykinesia sub-score \n") +
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
  scale_color_manual(values=c(  "#193a71", "#b5667b")) +
  scale_fill_manual(values=c( "#193a71", "#b5667b")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_prepos_dorps.svg", plot=plot1, width=3, height=4)


cor.test(deltas_df$`Drop V0`, deltas_df$`Drop V1`, method="spearman")

# 	Spearman's rank correlation rho
# 
# data:  deltas_df$`Drop V0` and deltas_df$`Drop V1`
# S = 6502882, p-value = 1.476e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2234289 

plot1 <- deltas_df  %>% 
  ggplot(aes(x = `Drop V0`, y = `Drop V1`)) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=1) +
  theme_minimal() + 
  ylim(-110, -30) + xlim(-110, -30) +
  labs(x = "\n Pre-OP % Delta",
       y = "Post-OP % Delta \n") +
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
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_dorps_cors_pre_post.svg", plot=plot1, width=3, height=3)







