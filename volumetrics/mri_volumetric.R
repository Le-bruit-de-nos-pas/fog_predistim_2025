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

# MRI Volumesn, % Normalized Volumes, and Asymmetries (%)

global_volumetry_info_2026_03_05_13_27 <- fread("data/global_volumetry_info_2026_03_05_13_27.csv")


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




# Quite a few replicates

length(unique(global_volumetry_info_2026_03_05_13_27$SUBJID )) / dim(global_volumetry_info_2026_03_05_13_27)[1]

# 74%



# Variability per patient, NOT a good metric given the asymmetries


vol_cols <- names(global_volumetry_info_2026_03_05_13_27)[9:544]


qc_variability <- global_volumetry_info_2026_03_05_13_27 %>%
  group_by(SUBJID ) %>%
  summarise(
    across(all_of(vol_cols),
           ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
           .names = "{.col}_CV"),
    .groups = "drop"
  )


qc_variability <- qc_variability %>%
  rowwise() %>%
  mutate(mean_CV = mean(c_across(ends_with("_CV")), na.rm = TRUE)) %>%
  ungroup()


qc_variability %>% select(SUBJID , mean_CV) %>% drop_na()  %>%
  ggplot(aes(mean_CV)) +
  geom_density()
  
qc_variability %>% select(SUBJID , mean_CV) %>% drop_na() %>% arrange(mean_CV)


# If using mean per patient, NOT RECOMMENDED

global_volumetry_mean <- global_volumetry_info_2026_03_05_13_27 %>%
  group_by(SUBJID ) %>%
  summarise(
    across(9:544, ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )





# original df volumetric

global_volumetry_info_2026_03_05_13_27 <- global_volumetry_info_2026_03_05_13_27 %>%
  mutate(
    SUBJID  = str_replace(SUBJID , "^sub(\\d{2})(\\d{3}).*", "\\1-\\2")
  )


FOG_df <- global_volumetry_info_2026_03_05_13_27 %>% 
  inner_join(FOG_df )

names(FOG_df)

range(FOG_df$OFF_2.13, na.rm=TRUE)
range(FOG_df$ON_2.13, na.rm=TRUE)
range(FOG_df$OFF_3.11, na.rm=TRUE)
range(FOG_df$ON_3.11, na.rm=TRUE)


# FOG_df <- FOG_df %>%
#   mutate(OFF_2.13 = ifelse(is.na(OFF_2.13), NA, ifelse(OFF_2.13==0,0,1))) %>%
#     mutate(ON_2.13 = ifelse(is.na(OFF_2.13), NA, ifelse(ON_2.13==0,0,1))) %>%
#     mutate(OFF_3.11 = ifelse(is.na(OFF_2.13), NA, ifelse(OFF_3.11==0,0,1))) %>%
#     mutate(ON_3.11 = ifelse(is.na(OFF_2.13), NA, ifelse(ON_3.11==0,0,1))) 

sum(is.na(FOG_df$OFF_2.13))
sum(is.na(FOG_df$ON_2.13))
sum(is.na(FOG_df$OFF_3.11))
sum(is.na(FOG_df$ON_3.11))
  
dim(FOG_df)[1]

table(FOG_df$OFF_2.13)
table(FOG_df$ON_2.13)
table(FOG_df$OFF_3.11)
table(FOG_df$ON_3.11)


FOG_df <- FOG_df %>% select(-ON_2.13, -ON_3.11)

