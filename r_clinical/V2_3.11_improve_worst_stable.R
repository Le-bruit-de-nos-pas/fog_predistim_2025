library(tidyverse)
library(data.table)


pats_to_track <- fread("data/Item3.10_after.txt")
pats_to_track <- pats_to_track[,"SUBJID"]
SUBJID <- pats_to_track %>% select(SUBJID)

item3.11_pats_groups_dbs_resp <- fread("data/item3.11_pats_groups_dbs_resp.txt")


UPDRSIII_TOTAUX <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- SUBJID %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX[-1,]

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX %>% select(SUBJID, TOT_OFF_DRUG_V0, EVAL_MOT_BESTON_V0,
                                              OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX <- data.frame(UPDRSIII_TOTAUX) %>% mutate_each(as.numeric, TOT_OFF_DRUG_V0:ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- UPDRSIII_TOTAUX %>% select(SUBJID, OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- Tot_UPDRS_III_after %>% drop_na() # 503


# Reshape data to long format
data_long <- Tot_UPDRS_III_after %>%
  pivot_longer(cols = c(OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1),
               names_to = "Condition",
               values_to = "Value")



item3.11_pats_groups_dbs_resp <- item3.11_pats_groups_dbs_resp %>% 
   mutate(group=ifelse(group=="Better", "Better [Decrease FOG]",
                      ifelse(group=="Worst", "Worst [Increased FOG]",
                             ifelse(group=="Stable_FOG", "Stable [With FOG]", "Stable [No FOG]")))) 
 

plot1 <- data_long %>%
  inner_join(item3.11_pats_groups_dbs_resp %>% select(SUBJID, group)) %>%
  mutate(Condition=ifelse(Condition=="OFF_TOTALCALC_V1", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_TOTALCALC_V1", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_TOTALCALC_V1", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=FALSE) +
  geom_jitter(alpha=0.25, size=1, height=0.5, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Post-OP Condition",
       y = "MDS-UPDRS III Total Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) +
  scale_fill_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) + 
  theme(text = element_text(face = "bold")) +
  facet_wrap(~group, ncol=4)

plot1

ggsave(file="updrs3tot_postops.svg", plot=plot1, width=9, height=5)













# POST OPS **********************

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




# Reshape data to long format
data_long <- Axials %>%
  pivot_longer(cols = c(OFF_After, ONOFF_After, OFFON_After, ONON_After),
               names_to = "Condition",
               values_to = "Value")



plot1 <- data_long %>% 
   inner_join(item3.11_pats_groups_dbs_resp %>% select(SUBJID, group)) %>%
    mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=FALSE) +
  geom_jitter(alpha=0.25, size=1, height=0.5, shape=1, stroke=1) +
  theme_minimal() +
 labs(x = "\n Post-OP Condition",
       y = "Axial sub-score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_color_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) +
  scale_fill_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) + 
  theme(text = element_text(face = "bold")) +
  facet_wrap(~group, ncol=4)

plot1

ggsave(file="axial_postops.svg", plot=plot1, width=9, height=5)



 

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




plot1 <- data_long %>% 
    inner_join(item3.11_pats_groups_dbs_resp %>% select(SUBJID, group)) %>%
  
  mutate(Condition=ifelse(Condition=="OFF_After_Brady", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After_Brady", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After_Brady", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
 geom_boxplot(alpha=0.6, outliers = FALSE, notch=FALSE) +
  geom_jitter(alpha=0.25, size=1, height=0.5, shape=1, stroke=1) +
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
        strip.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) +
  scale_fill_manual(values=c("#193a71", "#484c7a", "#725a84", "#986981")) + 
  theme(text = element_text(face = "bold")) +
  facet_wrap(~group, ncol=4)

plot1

ggsave(file="brady_postops.svg", plot=plot1, width=9, height=5)
