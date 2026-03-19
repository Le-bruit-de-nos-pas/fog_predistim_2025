library(readxl)
library(tidyverse)
library(data.table)

# Age


DEMOGRAPHIE <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
DEMOGRAPHIE <- DEMOGRAPHIE %>% select(SUBJID, AGE) 
DEMOGRAPHIE <- DEMOGRAPHIE[-1,]
DEMOGRAPHIE$AGE <- as.numeric(DEMOGRAPHIE$AGE)


# MRI Volumesn, % Normalized Volumes, and Asymmetries (%)

global_volumetry_info_2026_03_05_13_27 <- fread("data/global_volumetry_info_2026_03_05_13_27.csv")

global_volumetry_info_2026_03_05_13_27 %>% group_by(`Quality control`) %>% count()

# identify duplicates, for reference

global_volumetry_info_2026_03_05_13_27 %>%
  mutate(duplicate_id = str_extract(Subject, "\\d+$"),
         duplicate_id = if_else(is.na(duplicate_id), "1", duplicate_id)) %>%
  select(Subject, duplicate_id) %>% head(50)

global_volumetry_info_2026_03_05_13_27 <- 
  global_volumetry_info_2026_03_05_13_27 %>%
  mutate(
    duplicate_id = coalesce(as.integer(str_extract(Subject, "\\d+$")), 1),
    SUBJID  = str_remove(Subject, "\\d+$")
  ) %>%  filter(`Quality control` == "A")


