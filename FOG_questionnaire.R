
library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)


pats_to_track <- fread("data/Item3.10_after.txt")
pats_to_track <- pats_to_track[,"SUBJID"]

SUBJID <- pats_to_track %>% select(SUBJID) # 520


# FOG Questionnaire

FOG <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "FOG", skip=0, col_types = "text", trim_ws = TRUE)

length(unique(FOG$SUBJID)) # 836

#FOG <- SUBJID %>% inner_join(FOG) 

fog_items <- paste0("FOG", 1:16)

FOG <- FOG %>%
  mutate(across(all_of(fog_items), as.numeric))

FOG <- FOG %>%
  mutate(
    fog_tot = rowSums(
      across(all_of(fog_items), ~  .x),
      na.rm = TRUE
    )
    )


FOG <- FOG[-1, ]
FOG <- FOG[, -3]

FOG <- FOG %>%
  filter(!if_all(FOG1:FOG16, is.na))

length(unique(FOG$SUBJID)) # 793

FOG %>% group_by(VISIT ) %>%
  mutate(fog_tot =as.numeric(fog_tot )) %>%
  summarise(mean=mean(fog_tot , na.rm=T),
            sd=sd(fog_tot , na.rm=T),
            median=median(fog_tot , na.rm=T),
            q1=quantile(fog_tot , 0.25, na.rm=T),
            q3=quantile(fog_tot , 0.75, na.rm=T))


FOG_tot <- FOG %>% select(SUBJID, VISIT, fog_tot) %>%
  mutate(VISIT=ifelse(VISIT=="Visite de screening", "V0",
                      ifelse(VISIT=="Visite Bilan à 1 an - V1", "V1",
                             ifelse(VISIT=="Visite Bilan à 3 ans - V3", "V3", "V5")))) %>%
  spread(key=VISIT, value=fog_tot)

FOG_tot <- FOG_tot %>% drop_na()

FOG_tot #190


friedman.test(as.matrix(FOG_tot[, 2:5])) 


# 	Friedman rank sum test
# 
# data:  as.matrix(FOG_tot[, 2:5])
# Friedman chi-squared = 91.769, df = 3, p-value < 2.2e-16

data_long <- FOG_tot %>%
  pivot_longer(cols = c(V0, V1, V3, V5),
               names_to = "Condition",
               values_to = "Value")

pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")



# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#    V0      V1      V3     
# V1 0.0037  -       -      
# V3 0.4593  8.2e-09 -      
# V5 1.2e-06 < 2e-16 1.2e-06
# 
# P value adjustment method: bonferroni


plot1 <- data_long %>% 
  ggplot(aes(x = Condition, y = Value, fill =Condition , colour=Condition)) +
  geom_boxplot(alpha=0.6, notch=TRUE) +
  geom_jitter(alpha=0.5, height = 0.1, shape=1, stroke=2) +
  theme_minimal() +
 labs(x = "\n Follow-up Condition",
       y = "FOG Questionaire Total score \n") +
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
  scale_color_manual(values=c( "#86a4b5", "#51798a", "#344b5b", "#20292a")) +
  scale_fill_manual(values=c( "#86a4b5", "#51798a", "#344b5b", "#20292a")) + 
  theme(text = element_text(face = "bold"))

plot1


ggsave(file="fog_quest.svg", plot=plot1, width=4, height=4)





FOG_item4 <- FOG %>% select(SUBJID, VISIT, FOG4) %>%
  mutate(VISIT=ifelse(VISIT=="Visite de screening", "V0",
                      ifelse(VISIT=="Visite Bilan à 1 an - V1", "V1",
                             ifelse(VISIT=="Visite Bilan à 3 ans - V3", "V3", "V5")))) %>%
  spread(key=VISIT, value=FOG4)

FOG_item4 <- FOG_item4 %>% drop_na()

FOG_item4 #180

# Reshape data to long format
data_long <- FOG_item4 %>%
  pivot_longer(cols = c(V0, V1, V3, V5),
               names_to = "Condition",
               values_to = "Value")


friedman.test(as.matrix(FOG_item4[, 2:5]))  # Assuming columns 2:5 are conditions


# 	Friedman rank sum test
# 
# data:  as.matrix(FOG_item4[, 2:5])
# Friedman chi-squared = 32.839, df = 3, p-value = 3.482e-07


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")


# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#    V0      V1      V3     
# V1 0.00022 -       -      
# V3 0.80185 0.03040 -      
# V5 1.00000 4.1e-06 0.01436
# 
# P value adjustment method: bonferroni














# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 180) * 100)  # Always using 180 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


# Filter only Value = 0 for labeling
labels_data <- percentage_stats 

plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.7) +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Follow-up Condition",
       y = "Percentage (%) \n",
       fill = "FOG Questionaire #4") +
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

ggsave(file="p1.svg", plot=plot1, width=6, height=4)




library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik  AIC     niter     max.grad cond.H 
#  logit flexible  720  -971.96 1959.92 487(1917) 2.93e-06 6.1e+01
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 2.814    1.678   
# Number of groups:  SUBJID 180 
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# ConditionV1  -0.8993     0.2132  -4.217 2.47e-05 ***
# ConditionV3  -0.3827     0.2065  -1.853   0.0639 .  
# ConditionV5   0.1820     0.2070   0.879   0.3793    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  -1.5345     0.2129  -7.209
# 1|2  -0.2789     0.2032  -1.373
# 2|3   0.9885     0.2060   4.799
# 3|4   4.4462     0.3277  13.567



library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")

# $emmeans
#  Condition emmean    SE  df asymp.LCL asymp.UCL
#  V0        -0.905 0.201 Inf     -1.30    -0.512
#  V1        -1.805 0.210 Inf     -2.22    -1.393
#  V3        -1.288 0.200 Inf     -1.68    -0.895
#  V5        -0.723 0.196 Inf     -1.11    -0.339
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast estimate    SE  df z.ratio p.value
#  V0 - V1     0.899 0.213 Inf   4.217  0.0001
#  V0 - V3     0.383 0.207 Inf   1.853  0.3835
#  V0 - V5    -0.182 0.207 Inf  -0.879  1.0000
#  V1 - V3    -0.517 0.207 Inf  -2.494  0.0758
#  V1 - V5    -1.081 0.211 Inf  -5.118  <.0001
#  V3 - V5    -0.565 0.204 Inf  -2.768  0.0338
# 
# P value adjustment: bonferroni method for 6 tests 


FOG_item4_bin <- FOG_item4 %>% 
  mutate(V0=ifelse(V0>=1,1,0)) %>%
  mutate(V1=ifelse(V1>=1,1,0)) %>%
  mutate(V3=ifelse(V3>=1,1,0)) %>%
  mutate(V5=ifelse(V5>=1,1,0)) 



FOG_item4_bin_long <- FOG_item4_bin %>%
  pivot_longer(
    cols = V0:V5,
    names_to = "Visit",
    values_to = "FOG"
  ) %>%
  mutate(
    Visit = factor(Visit, levels = c("V0", "V1", "V3", "V5")),
    FOG = factor(FOG, levels = c(0, 1),
                 labels = c("No freezing", "Freezing"))
  )


FOG_item4_bin_long  %>%
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
 

#    Transition                From        To          Visit     n
#    <chr>                     <fct>       <fct>       <fct> <int>
#  1 Freezing → Freezing       Freezing    Freezing    V1       90
#  2 Freezing → Freezing       Freezing    Freezing    V3       92
#  3 Freezing → Freezing       Freezing    Freezing    V5      118
#  4 Freezing → No freezing    Freezing    No freezing V1       41
#  5 Freezing → No freezing    Freezing    No freezing V3       16
#  6 Freezing → No freezing    Freezing    No freezing V5       10
#  7 No freezing → Freezing    No freezing Freezing    V1       18
#  8 No freezing → Freezing    No freezing Freezing    V3       36
#  9 No freezing → Freezing    No freezing Freezing    V5       18
# 10 No freezing → No freezing No freezing No freezing V1       31
# 11 No freezing → No freezing No freezing No freezing V3       36
# 12 No freezing → No freezing No freezing No freezing V5       34

library(ggalluvial)

plot <- ggplot(FOG_item4_bin_long, aes(x = Visit, stratum = FOG, alluvium = SUBJID, fill = FOG)) +
  geom_flow(stat = "alluvium", alpha = 0.5) +
  geom_stratum(width = 0.5, color = "white", alpha=0.8) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4
  ) +
  scale_fill_manual(
    values = c("No freezing" = "#bfcbd4",
               "Freezing" = "#344b5b")
  ) +
  labs( x = "Follow-up Condition", y = "Number of patients \n", fill = "FOG status"  ) +
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

ggsave(file="sankey.svg", plot=plot, width=10, height=5)




FOG_improve_worsen_groups <- FOG_item4_bin_long  %>%
  arrange(SUBJID, Visit) %>%
  group_by(SUBJID) %>%
  mutate(FOG_prev = lag(FOG)) %>%
  ungroup()  %>%
  filter(!is.na(FOG_prev)) %>%
  filter(Visit=="V1") %>%
  mutate(Group=ifelse(FOG=="Freezing"&FOG_prev=="Freezing", "Stable_FOG",
                      ifelse(FOG=="No freezing"&FOG_prev=="No freezing", "Stable_No_FOG",
                             ifelse(FOG=="Freezing"&FOG_prev=="No freezing", "Worsen_FOG", "Improve_FOG")))) %>%
  select(-FOG, -FOG_prev)


FOG_improve_worsen_groups %>% group_by(Group) %>% count()


FOG_improve_worsen_groups <- FOG_improve_worsen_groups %>% select(-Visit)
FOG_improve_worsen_groups %>% group_by(Group) %>% count()





# Item3.11_after

UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

df_names <- names(UPDRSIII_COMPLET_V0_V1)

Item3.11 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, 
                                              OFF_3.11_,ON_3.11_, ON_3.11_1, ON_3.11_2,ON_3.11_3,ON_3.11_4,ON_3.11_5,
                                              OFF_3.11_1,ONOFF_3.11_,OFFON_3.11_, ON_3.11_6)

Item3.11 <- Item3.11[-1,]

names(Item3.11) <- c("SUBJID", "OFF_Before", "ON_15min_Before",
                     "ON_30min_Before","ON_45min_Before","ON_60min_Before",
                     "ON_90min_Before", "ON_120min_Before", "OFF_After",
                     "ONOFF_After", "OFFON_After", "ONON_After")

Item3.11 <- data.frame(Item3.11) %>% mutate_each(as.numeric, OFF_Before:ONON_After)

dim(Item3.11)[1] * dim(Item3.11)[2]

Item3.11_after <- Item3.11 %>% select(SUBJID, OFF_After, ONOFF_After, OFFON_After, ONON_After)

dim(Item3.11_after)[1] * dim(Item3.11_after)[2]

sum(is.na(Item3.11_after))

length(unique(Item3.11_after$SUBJID)) # 835

Item3.11_after <- Item3.11_after %>% drop_na()

length(unique(Item3.11_after$SUBJID)) # 520

Item3.11_before_vs_after <- Item3.11

# Define the relevant columns
columns_to_check <- c("ON_15min_Before", "ON_30min_Before", "ON_45min_Before", 
                      "ON_60min_Before", "ON_90min_Before", "ON_120min_Before")

# Compute the max value for each SUBJID, ignoring NAs
Item3.11_before_vs_after <- Item3.11_before_vs_after %>%
  mutate(Min_ON_Before = pmin(!!!syms(columns_to_check), na.rm = TRUE))

Item3.11_before_vs_after <- Item3.11_before_vs_after %>% select(SUBJID, OFF_Before, Min_ON_Before)

Item3.11_before_vs_after <- Item3.11_before_vs_after %>% drop_na()

Item3.11_after <- Item3.11_before_vs_after %>% inner_join(Item3.11_after)



dat_long <- Item3.11_after %>%
  inner_join(FOG_improve_worsen_groups, by = "SUBJID") %>%
  pivot_longer(
    cols = c(OFF_Before, Min_ON_Before, OFF_After, ONOFF_After, OFFON_After, ONON_After),
    names_to = "Condition",
    values_to = "Item311"
  ) %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("OFF_Before", "Min_ON_Before", "OFF_After", "ONOFF_After", "OFFON_After", "ONON_After"),
      labels = c("Vo [OFF]", "V0 [ON]", "V1 [OFF]", "V1 [DBS ON]", "V1 [LD ON]", "V1 [ON/ON]")
    ),
    Group = factor(Group)
  )



dat_long %>%
  group_by(Group, Condition) %>%
  summarise(
    mean = mean(Item311, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(Condition, mean, color = Group, group = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(y = "Median Item 3.11") +
  theme_minimal()

summary_df <- dat_long %>%
  group_by(Group, Condition) %>%
  summarise(
    mean = mean(Item311, na.rm = TRUE),
    sem   = sd(Item311, na.rm = TRUE)/n(),
    n    = n(),
    .groups = "drop"
  )

ggplot(summary_df, aes(x = Condition, y = mean, color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.1,
    alpha = 0.6
  ) +
  labs(
    y = "Mean Item 3.11 ± SD",
    x = "Condition",
    color = "Freezing group"
  ) +
  theme_minimal()


ggplot(dat_long, aes(x = Condition, y = Item311, colour=Group, fill=Group)) +
  ## Individual patient trajectories
  geom_line(
    aes(group = SUBJID),
   # color = Group,
    alpha = 0.4
  ) +
  geom_point(
    aes(group = SUBJID),
    #color = Group,
    alpha = 0.4,
    size = 1
  ) +
  
  ## Group-level average (median is better than mean here)
  stat_summary(
    aes(color = Group, group = Group),
    fun = mean,
    geom = "line",
    linewidth = 1.3
  ) +
  stat_summary(
    aes(color = Group),
    fun = mean,
    geom = "point",
    size = 2.5
  ) +
  
  labs(
    x = "Condition",
    y = "Item 3.11",
    color = "Freezing group",
    title = "Individual trajectories and group-level trends across conditions"
  ) +
  theme_minimal()






dat_long <- dat_long %>%
  mutate(Item311_bin = as.integer(Item311 > 0))

m_bin_int <- glmer(
  Item311_bin ~ Condition * Group + (1 | SUBJID),
  data = dat_long,
  family = binomial
)



m_bin_main <- glmer(
  Item311_bin ~ Condition + Group + (1 | SUBJID),
  data = dat_long,
  family = binomial
)

anova(m_bin_main, m_bin_int, test = "Chisq")

# Data: dat_long
# Models:
# m_bin_main: Item311_bin ~ Condition + Group + (1 | SUBJID)
# m_bin_int: Item311_bin ~ Condition * Group + (1 | SUBJID)
#            npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# m_bin_main   10 598.17 647.39 -289.09   578.17                     
# m_bin_int    25 606.11 729.16 -278.06   556.11 22.059 15     0.1063

#Do freezing groups respond differently to conditions?
#
#No.
#There is no evidence that freezing groups respond differently to conditions with respect to Item 3.11 presence.








# Axials 


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


#Tot_UPDRS_III_after
UPDRSIII_TOTAUX <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_TOTAUX)

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX[-1,]

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX %>% select(SUBJID, TOT_OFF_DRUG_V0, EVAL_MOT_BESTON_V0,
                                              OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX <- data.frame(UPDRSIII_TOTAUX) %>% mutate_each(as.numeric, TOT_OFF_DRUG_V0:ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- UPDRSIII_TOTAUX %>% select(SUBJID, OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

Tot_UPDRS_III_after <- Tot_UPDRS_III_after %>% drop_na() # 503



