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


Brady_scores_Down <- BradyDown %>% 
  mutate(OFF_After_Brady=OFF_After_BradyDown) %>%
  mutate(ONOFF_After_Brady=ONOFF_After_BradyDown) %>%
  mutate(OFFON_After_Brady=OFFON_After_BradyDown) %>%
  mutate(ON_After_Brady=ON_After_BradyDown) %>%
  select(SUBJID, OFF_After_Brady, ONOFF_After_Brady, OFFON_After_Brady, ON_After_Brady)


Brady_scores_Up <- BradyUp %>%
  mutate(OFF_After_Brady=OFF_After_BradyUp) %>%
  mutate(ONOFF_After_Brady=ONOFF_After_BradyUp) %>%
  mutate(OFFON_After_Brady=OFFON_After_BradyUp) %>%
  mutate(ON_After_Brady=ON_After_BradyUp) %>%
  select(SUBJID, OFF_After_Brady, ONOFF_After_Brady, OFFON_After_Brady, ON_After_Brady)


Brady_scores_Down <- Brady_scores_Down %>% drop_na()
Brady_scores_Up <- Brady_scores_Up %>% drop_na()


Brady_scores_Down <- Brady_scores_Down %>% 
  mutate(Delta_Med=100* (OFFON_After_Brady -OFF_After_Brady)/OFF_After_Brady ) %>%
  mutate(Delta_Stim=100* (ONOFF_After_Brady-OFF_After_Brady)/OFF_After_Brady ) %>%
  select(SUBJID, Delta_Stim, Delta_Med)

Brady_scores_Up <- Brady_scores_Up %>% 
  mutate(Delta_Med=100* (OFFON_After_Brady -OFF_After_Brady)/OFF_After_Brady ) %>%
  mutate(Delta_Stim=100* (ONOFF_After_Brady-OFF_After_Brady)/OFF_After_Brady ) %>%
  select(SUBJID, Delta_Stim, Delta_Med)



Brady_scores_Up <- Brady_scores_Up %>% drop_na()
Brady_scores_Down <- Brady_scores_Down %>% drop_na()


Brady_scores_Down <- Brady_scores_Down %>%
  filter(if_all(c(Delta_Stim  , Delta_Med), ~ is.finite(.)))

Brady_scores_Up <- Brady_scores_Up %>%
  filter(if_all(c(Delta_Stim  , Delta_Med), ~ is.finite(.)))

Brady_scores_Down <- Brady_scores_Down %>% select(SUBJID) %>% distinct() %>%
  inner_join(Brady_scores_Up %>% select(SUBJID) %>% distinct()) %>%
  inner_join(Brady_scores_Down)

Brady_scores_Up <- Brady_scores_Down %>% select(SUBJID) %>% distinct() %>%
  inner_join(Brady_scores_Up %>% select(SUBJID) %>% distinct()) %>%
  inner_join(Brady_scores_Up)

Brady_scores_Down <- Brady_scores_Down %>% arrange(SUBJID)

Brady_scores_Up <- Brady_scores_Up %>% arrange(SUBJID)

names(Brady_scores_Down) <- c("SUBJID", "Delta_Stim_Down", "Delta_Med_Down" )
names(Brady_scores_Up) <- c("SUBJID", "Delta_Stim_Up", "Delta_Med_Up" )


sum(is.na(Brady_scores_Down))
sum(is.na(Brady_scores_Up))


cor.test(Brady_scores_Down$Delta_Med_Down, Brady_scores_Up$Delta_Med_Up, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Down$Delta_Med_Down and Brady_scores_Up$Delta_Med_Up
# S = 10535116, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.5149702 


plot <- Brady_scores_Down %>% inner_join(Brady_scores_Up)  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Med_Up )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nLower-Body Brady Med-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_med.svg", plot=plot, width=4, height=4)


cor.test(Brady_scores_Down$Delta_Stim_Down, Brady_scores_Up$Delta_Stim_Up, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Down$Delta_Stim_Down and Brady_scores_Up$Delta_Stim_Up
# S = 13660181, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3710943 


plot <- Brady_scores_Down %>% inner_join(Brady_scores_Up)  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Stim_Up )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nLower-Body Brady Stim-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_stim.svg", plot=plot, width=4, height=4)












cor.test(Brady_scores_Down$Delta_Med_Down, Brady_scores_Up$Delta_Stim_Up, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Down$Delta_Med_Down and Brady_scores_Up$Delta_Stim_Up
# S = 18486134, p-value = 0.0007697
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1489106 


plot <- Brady_scores_Down %>% inner_join(Brady_scores_Up)  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Stim_Up )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nLower-Body Brady Med-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_med.svg", plot=plot, width=4, height=4)


cor.test(Brady_scores_Down$Delta_Stim_Down, Brady_scores_Up$Delta_Med_Up, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Down$Delta_Stim_Down and Brady_scores_Up$Delta_Med_Up
# S = 17246693, p-value = 2.916e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2059737 


plot <- Brady_scores_Down %>% inner_join(Brady_scores_Up)  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Med_Up )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nLower-Body Brady Stim-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_stim.svg", plot=plot, width=4, height=4)





cor.test(Brady_scores_Down$Delta_Stim_Down, Brady_scores_Down$Delta_Med_Down, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Down$Delta_Stim_Down and Brady_scores_Down$Delta_Med_Down
# S = 14486165, p-value = 1.339e-14
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3330666 

plot <- Brady_scores_Down %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Med_Down )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nLower-Body Brady Stim-ON % Delta",
       y = "Lower-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_stim.svg", plot=plot, width=4, height=4)






cor.test(Brady_scores_Up$Delta_Med_Up, Brady_scores_Up$Delta_Stim_Up, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  Brady_scores_Up$Delta_Med_Up and Brady_scores_Up$Delta_Stim_Up
# S = 13559068, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3757495  


plot <- Brady_scores_Up %>% 
  ggplot(aes(x = Delta_Med_Up , y = Delta_Stim_Up )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nUpper-Body Brady Med-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_lower_brady_med.svg", plot=plot, width=4, height=4)













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



Axials <- Axials %>% 
  mutate(Delta_Med=100* (OFFON_After  -OFF_After )/OFF_After  ) %>%
  mutate(Delta_Stim=100* (ONOFF_After -OFF_After )/OFF_After  ) %>%
  select(SUBJID, Delta_Stim, Delta_Med)

Axials <- Axials %>% drop_na()

Axials <- Axials %>%
  filter(if_all(c(Delta_Stim  , Delta_Med), ~ is.finite(.)))

names(Axials) <- c("SUBJID", "Delta_Stim_Axial", "Delta_Med_Axial" )






# UPPER  BODY BRADY MED and STIM - MED AXIAL



test <- Axials %>% 
  inner_join(Brady_scores_Up) 


cor.test(test$Delta_Med_Up, test$Delta_Med_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Up and test$Delta_Med_Axial
# S = 1530902, p-value = 1.839e-08
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.351873 


plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Up , y = Delta_Med_Axial )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nAxial Med-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Up, test$Delta_Med_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Up and test$Delta_Med_Axial
# S = 2148896, p-value = 0.1617
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09023749 



plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Up , y = Delta_Med_Axial )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Axial Med-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)










# LOWER BODY BRADY MED and STIM - MED AXIAL


test <- Axials %>% 
  inner_join(Brady_scores_Down) 


cor.test(test$Delta_Med_Down, test$Delta_Med_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Down and test$Delta_Med_Axial
# S = 1568652, p-value = 8.576e-08
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3358913 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Med_Axial )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nAxial Med-ON % Delta",
       y = "Lower-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Down, test$Delta_Med_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Down and test$Delta_Med_Axial
# S = 2191837, p-value = 0.2642
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.07205796 



plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Med_Axial )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Axial Med-ON % Delta",
       y = "Lower-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)








# UPPER  BODY BRADY MED and STIM - STIM AXIAL



test <- Axials %>% 
  inner_join(Brady_scores_Up) 


cor.test(test$Delta_Med_Up, test$Delta_Stim_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Up and test$Delta_Stim_Axial
# S = 2077411, p-value = 0.06125
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1205016 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Up , y = Delta_Stim_Axial )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nAxial Stim-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Up, test$Delta_Stim_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Up and test$Delta_Stim_Axial
# S = 1675005, p-value = 4.2e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2908654 


plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Up , y = Delta_Stim_Axial )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Axial Stim-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)










# LOWER BODY BRADY MED and STIM - STIM AXIAL


test <- Axials %>% 
  inner_join(Brady_scores_Down) 


cor.test(test$Delta_Med_Down, test$Delta_Stim_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Down and test$Delta_Stim_Axial
# S = 2135415, p-value = 0.1367
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09594481 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Stim_Axial )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nAxial Stim-ON % Delta",
       y = "Lower-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Down, test$Delta_Stim_Axial, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Down and test$Delta_Stim_Axial
# S = 1885563, p-value = 0.001609
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2017232 


plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Stim_Axial )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Axial Stim-ON % Delta",
       y = "Lower-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)





































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



Item3.10_after <- Item3.10_after %>% 
  mutate(Delta_Med=100* (OFFON_After  -OFF_After )/OFF_After  ) %>%
  mutate(Delta_Stim=100* (ONOFF_After -OFF_After )/OFF_After  ) %>%
  select(SUBJID, Delta_Stim, Delta_Med)

Item3.10_after <- Item3.10_after %>% drop_na()

Item3.10_after <- Item3.10_after %>%
  filter(if_all(c(Delta_Stim  , Delta_Med), ~ is.finite(.)))

names(Item3.10_after) <- c("SUBJID", "Delta_Stim_Gait", "Delta_Med_Gait" )




# UPPER  BODY BRADY MED and STIM - MED AXIAL



test <- Item3.10_after %>% 
  inner_join(Brady_scores_Up) 


cor.test(test$Delta_Med_Up, test$Delta_Med_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Up and test$Delta_Med_Gait
# S = 8034911, p-value = 3.509e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3807713 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Up , y = Delta_Med_Gait )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nGait Med-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Up, test$Delta_Med_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Up and test$Delta_Med_Gait
# S = 11674769, p-value = 0.03837
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1002574 



plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Up , y = Delta_Med_Gait )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Gait Med-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)










# LOWER BODY BRADY MED and STIM - MED AXIAL


test <- Item3.10_after %>% 
  inner_join(Brady_scores_Down) 


cor.test(test$Delta_Med_Down, test$Delta_Med_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Down and test$Delta_Med_Gait
# S = 8132493, p-value = 1.458e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3732509 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Med_Gait )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nGait Med-ON % Delta",
       y = "Lower-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Down, test$Delta_Med_Gait, method = "spearman")
# 
# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Down and test$Delta_Med_Gait
# S = 11714505, p-value = 0.04472
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09719499 



plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Med_Gait )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Gait Med-ON % Delta",
       y = "Lower-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)








# UPPER  BODY BRADY MED and STIM - STIM AXIAL



test <- Item3.10_after %>% 
  inner_join(Brady_scores_Up) 


cor.test(test$Delta_Med_Up, test$Delta_Stim_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Up and test$Delta_Stim_Gait
# S = 11795620, p-value = 0.06043
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09094373 

plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Up , y = Delta_Stim_Gait )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nGait Stim-ON % Delta",
       y = "Upper-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Up, test$Delta_Stim_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Up and test$Delta_Stim_Gait
# S = 9255554, p-value = 1.598e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2866997 

plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Up , y = Delta_Stim_Gait )) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Gait Stim-ON % Delta",
       y = "Upper-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)










# LOWER BODY BRADY MED and STIM - STIM AXIAL


test <- Item3.10_after %>% 
  inner_join(Brady_scores_Down) 


cor.test(test$Delta_Med_Down, test$Delta_Stim_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Med_Down and test$Delta_Stim_Gait
# S = 11062885, p-value = 0.002259
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1474136 
	
plot <- test  %>% 
  ggplot(aes(x = Delta_Med_Down , y = Delta_Stim_Gait )) +
  geom_smooth(method="lm", colour= "#193a71", fill="#193a71" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#193a71", stroke=2) +
  theme_minimal() + 
 ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\nGait Stim-ON % Delta",
       y = "Lower-Body Brady Med-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_axial_med.svg", plot=plot, width=4, height=4)


cor.test(test$Delta_Stim_Down, test$Delta_Stim_Gait, method = "spearman")

# 	Spearman's rank correlation rho
# 
# data:  test$Delta_Stim_Down and test$Delta_Stim_Gait
# S = 9195747, p-value = 8.488e-10
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2913088 


plot <- test  %>% 
  ggplot(aes(x = Delta_Stim_Down , y = Delta_Stim_Gait)) +
  geom_smooth(method="lm", colour= "#b5667b", fill="#b5667b" ) +
  geom_jitter(alpha=0.25, size=0.5, width=2.2, height=2, shape=1, colour= "#b5667b", stroke=2) +
  theme_minimal() + 
  ylim(-120, 120) + xlim(-120, 120) +
  labs(x = "\n Gait Stim-ON % Delta",
       y = "Lower-Body Brady Stim-ON % Delta \n") +
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

plot

ggsave(file="cor_upper_brady_stim_axial_med.svg", plot=plot, width=4, height=4)
