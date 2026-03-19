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


# Individual Spearman correlations, 2 FOG flags, All MRIs

mri_cols <- names(FOG_df)[9:544]   


library(dplyr)
library(purrr)

corr_results <- map_dfr(mri_cols, function(col) {
  
  test <- cor.test(
    FOG_df[[col]],
    FOG_df$OFF_2.13,
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


spearman_corr_fog_2.13 <- corr_results



n_top <- 20

top_pos <- spearman_corr_fog_2.13 %>%
  arrange(desc(spearman_r)) %>%
  slice(1:n_top)

top_neg <- spearman_corr_fog_2.13 %>%
  arrange(spearman_r) %>%
  slice(1:n_top)

top_features_fog <- bind_rows(top_pos, top_neg) %>%
  mutate(feature = fct_reorder(feature, spearman_r))  # order by correlation

plot <- ggplot(top_features_fog, aes(x = feature, y = spearman_r, fill = spearman_r > 0)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#b5667b", "FALSE" = "#193a71"), 
                    labels = c("Negative correlation", "Positive correlation")) +
  labs(
    x = NULL,
    y = "\n Spearman correlation with FOG 2.13",
    fill = "Direction"
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
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


plot

ggsave(file="fog2.13_corrs.svg", plot=plot, width=10, height=7)







corr_results <- map_dfr(mri_cols, function(col) {
  
  test <- cor.test(
    FOG_df[[col]],
    FOG_df$OFF_3.11,
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


spearman_corr_fog_3.11 <- corr_results


n_top <- 20

top_pos <- spearman_corr_fog_3.11 %>%
  arrange(desc(spearman_r)) %>%
  slice(1:n_top)

top_neg <- spearman_corr_fog_3.11 %>%
  arrange(spearman_r) %>%
  slice(1:n_top)

top_features_fog <- bind_rows(top_pos, top_neg) %>%
  mutate(feature = fct_reorder(feature, spearman_r))  # order by correlation

plot <- ggplot(top_features_fog, aes(x = feature, y = spearman_r, fill = spearman_r > 0)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#b5667b", "FALSE" = "#193a71"), 
                    labels = c("Negative correlation", "Positive correlation")) +
  labs(
    x = NULL,
    y = "\n Spearman correlation with FOG 3.11",
    fill = "Direction"
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
        axis.text.y = element_text(size = 10, hjust= 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 


plot

ggsave(file="fog3.11_corrs.svg", plot=plot, width=10, height=7)








names(FOG_df)

FOG_df <- FOG_df %>%
  mutate(OFF_2.13 = ifelse(is.na(OFF_2.13), NA, ifelse(OFF_2.13==0,0,1))) %>%
    mutate(OFF_3.11 = ifelse(is.na(OFF_2.13), NA, ifelse(OFF_3.11==0,0,1))) 



# LASSO 2.13

library(glmnet)
library(pROC)
library(dplyr)
library(caret)
library(ggplot2)
library(ggrepel)


FOG_clean <- FOG_df %>%
  filter(!is.na(OFF_2.13))

# MRI features only (NO AGE)
X <- FOG_clean %>%
  select(`White Matter (WM) volume cm3`:`Lobules VIII-X volume %`) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0)) %>%
  as.matrix()

# Standardize predictors
X <- scale(X)

y <- FOG_clean$OFF_2.13

# Balanced folds
set.seed(42)
folds <- createFolds(y, k = 10, list = FALSE)

auc_fold <- numeric(10)
f1_fold  <- numeric(10)

selected_features <- vector("list", 10)
coef_list <- vector("list", 10)

for (k in 1:10) {

  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)

  X_train <- X[train_idx, ]
  y_train <- y[train_idx]

  X_test <- X[test_idx, ]
  y_test <- y[test_idx]

  cv_model <- cv.glmnet(
    X_train,
    y_train,
    family = "binomial",
    alpha = 1,
    nfolds = 5
  )

  coef_model <- coef(cv_model, s = "lambda.min")

  # remove intercept
  coef_vec <- as.vector(coef_model)[-1]
  names(coef_vec) <- rownames(coef_model)[-1]

  coef_list[[k]] <- coef_vec
  selected_features[[k]] <- names(coef_vec[coef_vec != 0])

  pred_prob <- predict(
    cv_model,
    newx = X_test,
    s = "lambda.min",
    type = "response"
  )

  roc_obj <- roc(y_test, as.numeric(pred_prob), quiet = TRUE)
  auc_fold[k] <- auc(roc_obj)

  pred_class <- ifelse(pred_prob > 0.5, 1, 0)

  conf_mat <- confusionMatrix(
    factor(pred_class, levels = c(0,1)),
    factor(y_test, levels = c(0,1)),
    positive = "1"
  )

  f1_fold[k] <- conf_mat$byClass["F1"]
}

# Model performance
mean_auc <- mean(auc_fold)
sd_auc   <- sd(auc_fold)

mean_f1  <- mean(f1_fold)
sd_f1    <- sd(f1_fold)


feature_counts <- table(unlist(selected_features)) %>%
  sort(decreasing = TRUE)

top_features <- as.data.frame(feature_counts) %>%
  rename(feature = Var1, n_folds = Freq)

top_features_2.13 <- top_features 



library(xgboost)

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# Train model
set.seed(42)
final_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 200,
  max_depth = 3,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)


shap_contrib <- predict(final_model, dtrain, predcontrib = TRUE)

# Remove bias term
shap_contrib <- shap_contrib[, -ncol(shap_contrib)]

library(data.table)

shap_dt <- as.data.table(shap_contrib)
setnames(shap_dt, colnames(X))

shap_long <- melt(
  shap_dt[, ID := .I],
  id.vars = "ID",
  variable.name = "variable",
  value.name = "shap_value"
)

# Summary
summary_dt <- shap_long[, .(
  mean_abs_shap = mean(abs(shap_value))
), by = variable][order(-mean_abs_shap)]

top_shap <- summary_dt[1:50]


library(ggplot2)
library(forcats)

top_shap[, variable := fct_reorder(variable, mean_abs_shap)]

fwrite(top_shap, "top_shap_2.13.csv")


plot <- ggplot(top_shap, aes(x = mean_abs_shap, y = variable)) +
  geom_col(fill = "#b5667b", width = 0.7)  +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.6) +
  labs(
    title = "FOG 2.13 SHAP Feature Importance (Top 50)",
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




# LASSO 3.11



library(glmnet)
library(pROC)
library(dplyr)
library(caret)
library(ggplot2)
library(ggrepel)

FOG_clean <- FOG_df %>%
  filter(!is.na(OFF_3.11))

# MRI features only (NO AGE)
X <- FOG_clean %>%
  select(`White Matter (WM) volume cm3`:`Lobules VIII-X volume %`) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0)) %>%
  as.matrix()

# Standardize predictors
X <- scale(X)

y <- FOG_clean$OFF_3.11

# Balanced folds
set.seed(42)
folds <- createFolds(y, k = 10, list = FALSE)

auc_fold <- numeric(10)
f1_fold  <- numeric(10)

selected_features <- vector("list", 10)
coef_list <- vector("list", 10)

for (k in 1:10) {

  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)

  X_train <- X[train_idx, ]
  y_train <- y[train_idx]

  X_test <- X[test_idx, ]
  y_test <- y[test_idx]

  cv_model <- cv.glmnet(
    X_train,
    y_train,
    family = "binomial",
    alpha = 1,
    nfolds = 5
  )

  coef_model <- coef(cv_model, s = "lambda.min")

  # remove intercept
  coef_vec <- as.vector(coef_model)[-1]
  names(coef_vec) <- rownames(coef_model)[-1]

  coef_list[[k]] <- coef_vec
  selected_features[[k]] <- names(coef_vec[coef_vec != 0])

  pred_prob <- predict(
    cv_model,
    newx = X_test,
    s = "lambda.min",
    type = "response"
  )

  roc_obj <- roc(y_test, as.numeric(pred_prob), quiet = TRUE)
  auc_fold[k] <- auc(roc_obj)

  pred_class <- ifelse(pred_prob > 0.5, 1, 0)

  conf_mat <- confusionMatrix(
    factor(pred_class, levels = c(0,1)),
    factor(y_test, levels = c(0,1)),
    positive = "1"
  )

  f1_fold[k] <- conf_mat$byClass["F1"]
}

# Model performance
mean_auc <- mean(auc_fold)
sd_auc   <- sd(auc_fold)

mean_f1  <- mean(f1_fold)
sd_f1    <- sd(f1_fold)


feature_counts <- table(unlist(selected_features)) %>%
  sort(decreasing = TRUE)

top_features <- as.data.frame(feature_counts) %>%
  rename(feature = Var1, n_folds = Freq)

top_features_3.11 <- top_features 






library(xgboost)

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# Train model
set.seed(42)
final_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 200,
  max_depth = 3,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)


shap_contrib <- predict(final_model, dtrain, predcontrib = TRUE)

# Remove bias term
shap_contrib <- shap_contrib[, -ncol(shap_contrib)]

library(data.table)

shap_dt <- as.data.table(shap_contrib)
setnames(shap_dt, colnames(X))

shap_long <- melt(
  shap_dt[, ID := .I],
  id.vars = "ID",
  variable.name = "variable",
  value.name = "shap_value"
)

# Summary
summary_dt <- shap_long[, .(
  mean_abs_shap = mean(abs(shap_value))
), by = variable][order(-mean_abs_shap)]

top_shap <- summary_dt[1:50]
