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

global_volumetry_info_2026_03_05_13_27$SUBJID

AGE_df <- global_volumetry_info_2026_03_05_13_27 %>% 
  inner_join(DEMOGRAPHIE )

names(AGE_df)

range(AGE_df$AGE, na.rm=TRUE)


