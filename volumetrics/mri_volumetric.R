library(readxl)
library(tidyverse)
library(data.table)

# Freezing flags, only Pre-OP

Item3.11_before_vs_after <- fread("data/Item3.11_before_vs_after.txt")
UPDRSII_V0_2.13 <- fread("data/UPDRSII_V0_2.13.txt")

FOG_df <- UPDRSII_V0_2.13 %>% full_join(Item3.11_before_vs_after %>% select(-OFF_After, -Min_ON_After)) %>%
  rename("OFF_2.13"="V0_MDS2_13OFF", "ON_2.13"="V0_MDS2_13ON",  "OFF_3.11"="OFF_Before", "ON_3.11"="Min_ON_Before")


DEMOGRAPHIE <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
DEMOGRAPHIE <- DEMOGRAPHIE %>% select(SUBJID, AGE) 
DEMOGRAPHIE <- DEMOGRAPHIE[-1,]
DEMOGRAPHIE$AGE <- as.numeric(DEMOGRAPHIE$AGE)

FOG_df <- FOG_df %>% full_join(DEMOGRAPHIE)
