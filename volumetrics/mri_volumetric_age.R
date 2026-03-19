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

plot <-  ggplot(AGE_df, aes(AGE, `Cerebro Spinal Fluid (CSF) volume %`)) +
  geom_jitter(alpha = 0.3, colour="#b5667b", shape=1, stroke=2) +
  geom_smooth(method="lm", colour="#b5667b", fill="#b5667b") +
  theme_minimal() +
  labs(x = "\n Biological Age (years)",
       y = "Cerebro Spinal Fluid (CSF) volume % \n") +
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

plot

ggsave(file="age_csf_vol_jitter.svg", plot=plot, width=5, height=5)



manhattan_age <- spearman_corr_age %>%
  mutate(
    neglog10_p = -log10(p_adj),
    direction = case_when(
      spearman_r > 0 ~ "Increase with age",
      spearman_r < 0 ~ "Decrease with age",
      TRUE ~ "Neutral"
    )
  )


library(ggrepel)

top_features <- manhattan_age %>%
  arrange(p_adj) %>%
  slice(1:50)

plot <- ggplot(manhattan_age,
       aes(spearman_r, neglog10_p, color = direction)) +
  geom_point(alpha = 0.6, stroke=1, shape=1) +
  geom_text_repel(data = top_features,
                  aes(label = feature),
                  size = 3) +
  theme_minimal() +
    geom_hline(yintercept = -log10(0.05),
             linetype = "dashed",
             color = "black") +
  labs(x = "\n Spearman r",
       y = "-Log10 FDR-adjusted p-value \n") +
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
  scale_color_manual(values=c( "#193a71",  "#b5667b", "gray")) +
  scale_fill_manual(values=c( "#193a71",  "#b5667b", "gray")) 

ggsave(file="age_modified_Manhattan_plot.svg", plot=plot, width=6, height=6)





mri_cols <- names(AGE_df)[9:544]

AGE_df_model <- AGE_df %>%
  filter(!is.na(AGE)) 

X <- AGE_df_model %>%
  select(all_of(mri_cols)) %>%
  select(where(~ !all(is.na(.)))) %>%
  as.matrix()

y <- AGE_df_model$AGE

X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0]


library(xgboost)
library(caret)



set.seed(42)
folds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

pred_cv <- numeric(length(y))

for(i in seq_along(folds)){
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_along(y), test_idx)
  
  dtrain <- xgb.DMatrix(data = X[train_idx, ], label = y[train_idx])
  dtest  <- xgb.DMatrix(data = X[test_idx, ], label = y[test_idx])
  
  model <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 500,
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
  
  pred_cv[test_idx] <- predict(model, dtest)
}

cor(pred_cv, y)  

plot <- data.frame(pred_cv) %>%
  bind_cols(data.frame(y)) %>%
  ggplot(aes(x = y, y = pred_cv)) +
  geom_jitter(shape=1, stroke=2, colour="#b5667b", fill="#b5667b", alpha=0.6) +
  geom_smooth(method="lm", colour="#b5667b", fill="#b5667b") +
  labs(
    x = "\n True Biological Age (years)",
    y = "XGBoost 10-fold CV MRI Predicted Age (years) \n" 
  ) +
  theme_minimal() +
  xlim(35,75) + ylim(35,75) +
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

plot 

ggsave(file="pred_obs_age.svg", plot=plot, width=6, height=6)






dtrain_full <- xgb.DMatrix(data = X, label = y)

final_model <- xgboost(
  data = dtrain_full,
  objective = "reg:squarederror",
  nrounds = 500,               # or use your best early-stopped rounds from CV
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)


shap_contrib <- predict(final_model, dtrain_full, predcontrib = TRUE)

# Remove the last column (BIAS / baseline)
shap_contrib_no_bias <- shap_contrib[, -ncol(shap_contrib)]

# Re-create without baseline, as plain matrix
shap_contrib_no_bias <- as.matrix(shap_contrib[, -ncol(shap_contrib)])

library(data.table)
library(forcats)

# Convert to data.table + melt to long format
shap_dt <- as.data.table(shap_contrib_no_bias)
setnames(shap_dt, colnames(X))  # make column names match your features

shap_long <- melt(
  shap_dt[, ID := .I],
  id.vars       = "ID",
  variable.name = "variable",
  value.name    = "shap_value"
)

# Optional: add the actual feature values (helps with dependence plots later)
X_dt <- as.data.table(as.matrix(X))

setnames(X_dt, colnames(X))

X_long <- melt(
  X_dt[, ID := .I],
  id.vars = "ID",
  variable.name = "variable",
  value.name = "feature_value"
)

shap_long <- merge(shap_long, X_long, by = c("ID", "variable"))


summary_dt <- shap_long[, .(
  mean_abs_shap = mean(abs(shap_value)),
  sem_shap      = sd(shap_value) / sqrt(.N),    # standard error → narrow error bars
  mean_shap     = mean(shap_value),             # signed mean (optional)
  n             = .N
), by = variable][order(-mean_abs_shap)]

# Keep top 20 / 30 / 50
top_features <- summary_dt[1:50]   # adjust number here

# Nice ordering for plots
top_features[, variable := fct_reorder(variable, mean_abs_shap)]




plot <- ggplot(top_features, aes(x = mean_abs_shap, y = variable)) +
  geom_col(fill = "#193a71", width = 0.7)  +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.6) +
  labs(
    title = "SHAP Feature Importance (Top 50)",
    x = "Mean absolute SHAP value",
    y = NULL
  ) +
  theme_minimal() +
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
        axis.text.y = element_text(size = 10, hjust = 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 

plot 

ggsave(file="xgboost.svg", plot=plot, width=9, height=8)
