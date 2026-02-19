library(tidyverse)
library(data.table)



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


# with(Axials,pmin(ONOFF_After,OFFON_After,ONON_After, ONON_After))



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


plot1 <- data_long %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                                   ifelse(Condition=="OFFON_After", "Med-ON/DBS-OFF",
                                                          ifelse(Condition=="ONOFF_After", "Med-OFF/DBS-ON", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
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

ggsave(file="axial_postops.svg", plot=plot1, width=4, height=5)





# Deltas 4 conditions post op 

deltas_df <- Axials %>% mutate(`Med-OFF/DBS-ON`=100*(ONOFF_After-OFF_After)/OFF_After,
                               `Med-ON/DBS-OFF`=100*(OFFON_After -OFF_After)/OFF_After,
                               `ON/ON`=100*(ONON_After-OFF_After)/OFF_After) %>%
  select(SUBJID, `Med-OFF/DBS-ON`,`Med-ON/DBS-OFF`, `ON/ON`)


friedman.test(as.matrix(deltas_df[, 2:4]))  

# 	Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:4])
# Friedman chi-squared = 40.294, df = 2, p-value =
# 1.78e-09


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
# Med-ON/DBS-OFF 1.5e-12        -             
# ON/ON          1              2.4e-06       
# 
# P value adjustment method: bonferroni 

range(deltas_df$`Med-OFF/DBS-ON`,na.rm=T)

plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, outliers = FALSE, notch=TRUE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() + 
  ylim(-150, 200) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Post-OP Condition",
       y = "% Drop Axial sub-score \n") +
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

Axials <- Axials_V0_OFF %>% inner_join(Axials_V0_ON) %>% inner_join(Axials_V1_OFF) %>% inner_join(Axials_V1_ON) %>% drop_na()



# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(Axials$Axial_OFF_v0 , Axials$Axial_ON_v0 , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Axials$Axial_OFF_v0 and Axials$Axial_ON_v0
# V = 57254, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(Axials$Axial_OFF_v1  , Axials$Axial_ON_v1    , paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Axials$Axial_OFF_v1 and Axials$Axial_ON_v1
# V = 50601, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

before_after_off_on <- Axials  %>%
  pivot_longer(cols = c("Axial_OFF_v0","Axial_ON_v0", "Axial_OFF_v1", "Axial_ON_v1"),
               names_to = "Condition",
               values_to = "Value")  %>%
   mutate(Time=ifelse(Condition=="Axial_OFF_v0"|Condition=="Axial_ON_v0", "V0", "V1")) %>%
   mutate(State=ifelse(Condition=="Axial_OFF_v0"|Condition=="Axial_OFF_v1", "OFF", "ON")) 
 


plot1 <-  before_after_off_on %>% mutate(Condition=paste0(Time, paste0(" [", paste0(State, "]")))) %>%
 mutate(Condition=factor(Condition, levels=c("V0 [OFF]","V0 [ON]", "V1 [OFF]", "V1 [ON]"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, shape=1, stroke=1) +
  theme_minimal() +
  labs(x = "\n Pre-OP|Post-OP OFF|ON Condition",
       y = "Axial sub-score \n") +
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
# 1       Time 1, 374 3.36       2.67 <.001    .103
# 2      State 1, 374 7.27 530.69 ***  .272   <.001
# 3 Time:State 1, 374 2.02  11.17 ***  .002   <.001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

library(emmeans)

emm <- emmeans(anova_res, ~ Time * State)

# OFF vs ON within each Time
contrast(emm, method = "pairwise", by = "Time", adjust = "bonferroni")

# Time = V0:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     3.45 0.163 374  21.184  <.0001
# 
# Time = V1:
#  contrast estimate    SE  df t.ratio p.value
#  OFF - ON     2.96 0.152 374  19.542  <.0001
 
# Before vs After within each State
contrast(emm, method = "pairwise", by = "State", adjust = "bonferroni")

# State = OFF:
#  contrast estimate     SE  df t.ratio p.value
#  V0 - V1    0.0907 0.1530 374   0.594  0.5527
# 
# State = ON:
#  contrast estimate     SE  df t.ratio p.value
#  V0 - V1   -0.4000 0.0737 374  -5.427  <.0001







deltas_df <- Axials %>% mutate(`Drop V0`=100*(Axial_ON_v0 -Axial_OFF_v0 )/Axial_OFF_v0 ,
                               `Drop V1`=100*(Axial_ON_v1 -Axial_OFF_v1 )/Axial_OFF_v1 ) %>%
  select(SUBJID, `Drop V0`,`Drop V1`)


friedman.test(as.matrix(deltas_df[, 2:3]))  


# 	Friedman rank sum test
# 
# data:  as.matrix(deltas_df[, 2:3])
# Friedman chi-squared = 24.005, df = 1, p-value = 9.607e-07

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
# Drop V1 3.4e-08
# 
# P value adjustment method: bonferroni

plot1 <- deltas_df  %>%
  pivot_longer(cols = c("Drop V0","Drop V1"),
               names_to = "Condition",
               values_to = "Value") %>%
  mutate(Condition=factor(Condition, levels=c("Drop V0", "Drop V1"))) %>%
  ggplot(aes(x = Condition, y = Value, fill = Condition, colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE, outliers = FALSE) +
  geom_jitter(alpha=0.25, size=0.5, size=1, shape=1, stroke=1) +
  theme_minimal() + 
  #ylim(-120, 50) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "\n Pre|Post Evaluation",
       y = "% Drop Axial sub-score \n") +
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
  scale_fill_manual(values=c(  "#193a71", "#b5667b")) + 
  theme(text = element_text(face = "bold"))

plot1

ggsave(file="axial_prepos_dorps.svg", plot=plot1, width=3, height=4)


cor.test(deltas_df$`Drop V0`, deltas_df$`Drop V1`, method="spearman")

# 	Spearman's rank correlation rho
# 
# data:  deltas_df$`Drop V0` and deltas_df$`Drop V1`
# S = 3631110, p-value = 7.746e-10
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3351175 

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

