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



sum(is.na(AGE_df$AGE))

dim(AGE_df)[1]


# Individual Spearman correlations,

mri_cols <- names(AGE_df)[9:544]   


library(dplyr)
library(purrr)

corr_results <- map_dfr(mri_cols, function(col) {
  
  test <- cor.test(
    AGE_df[[col]],
    AGE_df$AGE,
    method = "spearman",
    use = "complete.obs"
  )
  
  tibble(
    feature = col,
    spearman_r = test$estimate,
    p_value = test$p.value
  )
})


corr_results <- corr_results %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr")) %>%
  arrange(desc(abs(spearman_r)))


spearman_corr_age <- corr_results

View(spearman_corr_age)

spearman_corr_age %>% arrange(spearman_r) %>% head(20)
spearman_corr_age %>% arrange(spearman_r) %>% tail(20)

plot <-  ggplot(AGE_df, aes(AGE, `Brain (WM+GM) volume %`)) +
  geom_jitter(alpha = 0.3, colour="#193a71", shape=1, stroke=2) +
  geom_smooth(method="lm", colour="#193a71", fill="#193a71") +
  theme_minimal() +
  labs(x = "\n Biological Age (years)",
       y = "Brain (WM+GM) volume % \n") +
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
        plot.margin = margin(5, 5, 5, 5, "pt")) 

plot1

ggsave(file="age_brain_vol_jitter.svg", plot=plot, width=5, height=5)
