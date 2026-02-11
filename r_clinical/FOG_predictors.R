
library(readxl)
library(data.table)
library(tidyverse)

DEMOGRAPHIE <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
DEMOGRAPHIE <- DEMOGRAPHIE %>% mutate(D_SCREEN=as.numeric(str_sub(D_SCREEN, 7L, 10L)))

features_df <- DEMOGRAPHIE %>% select(SUBJID, AGE) %>% filter(AGE!="âge")

features_df <- features_df %>% full_join(DEMOGRAPHIE %>% mutate(sympt_dur=as.numeric(D_SCREEN)-as.numeric(D_1ER_SYMPT)) %>% select(SUBJID, sympt_dur))

UPDRSII_V0_2.13 <- fread("data/UPDRSII_V0_2.13.txt")

features_df <- features_df %>% full_join(UPDRSII_V0_2.13)

Item3.11_before_vs_after <- fread("data/Item3.11_before_vs_after.txt")

features_df <- features_df %>% full_join(Item3.11_before_vs_after %>% 
                                           select(SUBJID, OFF_Before, Min_ON_Before) %>%
                                           rename("V0_MDS3_11OFF"="OFF_Before", "V0_MDS3_11ON"="Min_ON_Before"))


UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

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

Axials <- Axials_V0_OFF %>% inner_join(Axials_V0_ON) 


features_df <- features_df %>% full_join(Axials)




features_df <- features_df %>% full_join(Axials %>% mutate(Axial_Drop_V0=100*(Axial_ON_v0 -Axial_OFF_v0 )/Axial_OFF_v0  ) %>%
  select(SUBJID, Axial_Drop_V0))






UPDRSIII_TOTAUX <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX[-1,]

UPDRSIII_TOTAUX <- UPDRSIII_TOTAUX %>% select(SUBJID, TOT_OFF_DRUG_V0, EVAL_MOT_BESTON_V0,
                                              OFF_TOTALCALC_V1, ONOFF_TOTALCALC_V1, OFFON_TOTALCALC_V1, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX <- data.frame(UPDRSIII_TOTAUX) %>% mutate_each(as.numeric, TOT_OFF_DRUG_V0:ON_TOTALCALC_V1)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% select(SUBJID, TOT_OFF_DRUG_V0, OFF_TOTALCALC_V1, EVAL_MOT_BESTON_V0, ON_TOTALCALC_V1)

UPDRSIII_TOTAUX_before_vs_after <- UPDRSIII_TOTAUX_before_vs_after %>% drop_na()

features_df <- features_df %>% full_join(UPDRSIII_TOTAUX_before_vs_after %>% 
                                           select(SUBJID, TOT_OFF_DRUG_V0 ,EVAL_MOT_BESTON_V0 ) %>%
                                           rename("V0_MDS3_Tot_OFF"="TOT_OFF_DRUG_V0", "V0_MDS3_Tot_ON"="EVAL_MOT_BESTON_V0"))



features_df <- features_df %>% full_join(
  UPDRSIII_TOTAUX_before_vs_after %>% 
    mutate(MDS3_Drop_V0=100*(EVAL_MOT_BESTON_V0-TOT_OFF_DRUG_V0)/TOT_OFF_DRUG_V0) %>%
    select(SUBJID, MDS3_Drop_V0)
) 




LEDD_asymmetry <- fread("data/LEDD_asymmetry.csv")
LEDD_asymmetry <- LEDD_asymmetry %>% filter(visit=="Screening") %>% drop_na() %>% select(SUBJID, LEDD)

features_df <- features_df %>% full_join(LEDD_asymmetry) 




MOCA <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "MOCA_V0_V3", skip=0, col_types = "text", trim_ws = TRUE)

MOCA <- MOCA %>% filter(TITRE=="Visite de screening") %>% select(SUBJID, MOCA_SCORE)


features_df <- features_df %>% full_join(MOCA) 





DATES_DE_VISITES  <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DATES_DE_VISITES ", skip=0, col_types = "text", trim_ws = TRUE)
DATES_DE_VISITES <- DATES_DE_VISITES %>% select(SUBJID, D_CHIR)

features_df <- features_df %>% full_join(DATES_DE_VISITES %>% inner_join(DEMOGRAPHIE %>% select(SUBJID, DDN)) %>% 
  mutate(D_CHIR=as.numeric(str_sub(D_CHIR, 7L, 10L))) %>%
  mutate(DDN=as.numeric(str_sub(DDN, 4L, 7))) %>% 
  mutate(age_dbs=D_CHIR-DDN) %>% select(SUBJID, age_dbs)
) 



Clox <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "Clox", skip=0, col_types = "text", trim_ws = TRUE)
Clox <- Clox  %>% filter(TITRE=="Visite de screening") %>% select(SUBJID, CLOX_DESSIN_CALC, CLOX_COPIE_CALC )

features_df <- features_df %>% full_join(Clox) 


Fluence_verbale <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "Fluence verbale", skip=0, col_types = "text", trim_ws = TRUE)
Fluence_verbale <- Fluence_verbale %>% filter(TITRE_VISITE =="Visite de screening") %>% select(SUBJID,FLUENCE_NBMOTCORRECT )

features_df <- features_df %>% full_join(Fluence_verbale) 






# HAMD
HAMD <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "HAMD", skip=0, col_types = "text", trim_ws = TRUE)
HAMD <- HAMD %>% filter(TITRE=="Visite de screening") %>% select(SUBJID, HAMD_SCORECALC )

features_df <- features_df %>% full_join(HAMD) 

# HAMA
HAMA <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "HAMA ", skip=0, col_types = "text", trim_ws = TRUE)
HAMA <- HAMA %>% filter(TITRE=="Visite de screening") %>% select(SUBJID, HAMA_SCORECALC )

features_df <- features_df %>% full_join(HAMA) 





# UPPS
UPPS <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "UPPS", skip=0, col_types = "text", trim_ws = TRUE)

UPPS <- UPPS %>%
  filter(SUBJID != "Subject Identifier for the Study")

upps_items <- paste0("UPPS", 1:20)

UPPS <- UPPS %>%
  mutate(across(all_of(upps_items), as.numeric))

rev_urgence        <- c("UPPS4", "UPPS7", "UPPS12", "UPPS17")
rev_urgence_pos    <- c("UPPS2", "UPPS10", "UPPS15", "UPPS20")
rev_sensation      <- c("UPPS3", "UPPS9", "UPPS14", "UPPS18")

premeditation      <- c("UPPS1", "UPPS6", "UPPS13", "UPPS19")
perseverance       <- c("UPPS5", "UPPS8", "UPPS11", "UPPS16")


UPPS <- UPPS %>%
  mutate(
    urgence = rowSums(
      across(all_of(rev_urgence), ~ 5 - .x),
      na.rm = TRUE
    ),

    urgence_positive = rowSums(
      across(all_of(rev_urgence_pos), ~ 5 - .x),
      na.rm = TRUE
    ),

    manque_premeditation = rowSums(
      across(all_of(premeditation)),
      na.rm = TRUE
    ),

    manque_perseverance = rowSums(
      across(all_of(perseverance)),
      na.rm = TRUE
    ),

    recherche_sensation = rowSums(
      across(all_of(rev_sensation), ~ 5 - .x),
      na.rm = TRUE
    )
  )

UPPS <- UPPS %>% filter(TITRE=="Visite de screening") %>%
  select(SUBJID, urgence, urgence_positive, manque_premeditation, manque_perseverance, recherche_sensation)


features_df <- features_df %>% full_join(UPPS) 



FOG <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "FOG", skip=0, col_types = "text", trim_ws = TRUE)

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

FOG <- FOG %>% filter(VISIT=="Visite de screening") %>% select(SUBJID, fog_tot)

features_df <- features_df %>% full_join(FOG %>% select(SUBJID, fog_tot)) 

features_df <- features_df %>% distinct()



UPDRSII_V1_2.13 <- fread("data/UPDRSII_V1_2.13.txt")
UPDRSII_V1_2.13 <- UPDRSII_V1_2.13 %>% select(SUBJID, V1_MDS2_13ON) %>%
  mutate(V1_MDS2_13ON=ifelse(V1_MDS2_13ON==0,0,1))


features_df <- UPDRSII_V1_2.13 %>% inner_join(features_df)

features_df$MOCA_SCORE <- as.numeric(features_df$MOCA_SCORE)
features_df$CLOX_DESSIN_CALC <- as.numeric(features_df$CLOX_DESSIN_CALC)
features_df$CLOX_COPIE_CALC <- as.numeric(features_df$CLOX_COPIE_CALC)
features_df$FLUENCE_NBMOTCORRECT <- as.numeric(features_df$FLUENCE_NBMOTCORRECT)
features_df$AGE <- as.numeric(features_df$AGE)
features_df$HAMD_SCORECALC <- as.numeric(features_df$HAMD_SCORECALC)
features_df$HAMA_SCORECALC <- as.numeric(features_df$HAMA_SCORECALC)



library(xgboost)
library(data.table)
library(Matrix)
library(SHAPforxgboost)
library(caret)

features_df <- features_df %>% select(-age_dbs)

names(features_df) <- c("SUBJID", "[V1] UPDRS II 2.13 ON", "Age",
                        "Disease duration", "[V0] UPDRS II 2.13 OFF", "[V0] UPDRS II 2.13 ON",
                        "[V0] UPDRS III 3.11 OFF", "[V0] UPDRS III 3.11 ON", "[V0] Axial OFF",
                        "[V0] Axial ON", "[V0] % Axial Drop", "[V0] UPDRS III Total OFF",
                        "[V0] UPDRS III Total ON", "[V0] % UPDRS III Drop", "LEDD",
                        "MOCA",  "CLOX Draw",
                        "CLOX Copy", "Verbal Fluency", 
                        "HAMD", "HAMA", "UPPS Neg Urgency",
                        "UPPS Pos Urgency", "(Lack of) Premeditation",
                        "(Lack of) Perseverance",  "Sensation Seeking" , "French FOG Quest")


# XGBOOST 

dt <- as.data.table(features_df)

# Outcome
y <- dt$`[V1] UPDRS II 2.13 ON`

# Predictors
X <- dt[, !"[V1] UPDRS II 2.13 ON"]

X[, SUBJID := NULL]

# Convert to numeric matrix
X_mat <- data.matrix(X)


set.seed(1)

train_idx <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X_mat[train_idx, ]
X_test  <- X_mat[-train_idx, ]

y_train <- y[train_idx]
y_test  <- y[-train_idx]


dtrain <- xgb.DMatrix(
  data = X_train,
  label = y_train,
  missing = NA
)

dtest <- xgb.DMatrix(
  data = X_test,
  label = y_test,
  missing = NA
)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,
  verbose = 1
)

pred_prob <- predict(xgb_model, dtest)

roc_obj <- pROC::roc(y_test, pred_prob)

pROC::auc(roc_obj)

library(pROC)

roc_obj <- roc(response = y_test,predictor = pred_prob,levels = c(0, 1),direction = "<")

plot(roc_obj,col = "black",lwd = 3,legacy.axes = TRUE, main = "ROC curve for XGBoost model")

text(0.2, 0.2,labels = paste0("AUC = ", round(auc(roc_obj), 3)), cex = 0.9)

shap_values <- shap.values(xgb_model = xgb_model,X_train = X_train)

shap_long <- shap.prep(shap_contrib = shap_values$shap_score,X_train = X_train)

plot <- shap.plot.summary(shap_long, 
                  min_color_bound = "#efb854",
                  max_color_bound = "#5ab0cb")   + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        legend.position = "none") +
  theme(panel.background = element_blank(), 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    strip.background = element_blank(), strip.text = element_blank(), 
    axis.line = element_blank(), axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 8, vjust = -0.5),
    axis.title.y = element_text(size = 8, vjust = -0.5), plot.margin = margin(5, 5, 5, 5, "pt"))

plot

ggsave(file="plot.svg", plot=plot, width=6, height=4)

plot <- shap.plot.dependence(
  shap_long,
  x = "[V0] % UPDRS III Drop",
  y = "[V0] % UPDRS III Drop", 
  add_hist =TRUE,
  colour="#5ab0cb"
) 

plot

ggsave(file="plot.svg", plot=plot, width=4, height=4)





# LASSO

X_imp <- copy(X)

for (j in colnames(X_imp)) {
  if (is.numeric(X_imp[[j]])) {
    X_imp[is.na(get(j)), (j) := median(X_imp[[j]], na.rm = TRUE)]
  }
}

X_imp_mat <- data.matrix(X_imp)
X_scaled  <- scale(X_imp_mat)


library(glmnet)

set.seed(123)

cv_lasso <- cv.glmnet( x = X_scaled, y = y,
  family = "binomial", alpha = 1,nfolds = 5, standardize = FALSE )


plot(cv_lasso,xlab = expression(log(lambda)), ylab = "Binomial deviance", 
     main = "Cross-validated LASSO (binomial) \n", cex.axis = 1.0, cex.lab = 1.0)

coef_lasso <- coef(cv_lasso, s = "lambda.1se")

lasso_df <- data.table(feature = rownames(coef_lasso),coef = as.numeric(coef_lasso))

lasso_df <- lasso_df[coef != 0 & feature != "(Intercept)"]

lasso_df

lasso_df[, direction := ifelse(coef > 0, "Higher → ON", "Higher → OFF")]



lasso_prob <- predict(cv_lasso, newx = X_scaled, s = "lambda.1se", type = "response")

pROC::auc(y, as.numeric(lasso_prob))

library(pROC)

roc_lasso <- roc(response = y, predictor = as.numeric(lasso_prob), levels = c(0, 1), direction = "<" )

plot( roc_lasso, col = "black",lwd = 3, legacy.axes = TRUE, main = "ROC curve – LASSO model")

text(0.4, 0.2, labels = paste0("AUC = ", round(auc(roc_lasso), 3)), cex = 0.9)


plot <- ggplot(lasso_df, aes(x = feature, y = coef, fill = coef > 0)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "black", "FALSE" = "black"),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "Standardized LASSO coefficient"
  ) +
   theme(
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) + 
  theme(text = element_text(face = "bold"))

ggsave(file="plot.svg", plot=plot, width=5, height=2)

