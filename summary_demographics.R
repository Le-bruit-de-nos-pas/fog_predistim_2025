pats_to_track <- fread("data/Item3.10_after.txt")
pats_to_track <- pats_to_track[,"SUBJID"]

SUBJID <- pats_to_track %>% select(SUBJID) # 520

# DEMOGRAPHICS

DEMOGRAPHIE <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)

DEMOGRAPHIE <- SUBJID %>% inner_join(DEMOGRAPHIE)

DEMOGRAPHIE <- DEMOGRAPHIE %>% mutate(D_SCREEN=as.numeric(str_sub(D_SCREEN, 7L, 10L)))

mean(as.numeric(DEMOGRAPHIE$AGE));  sd(as.numeric(DEMOGRAPHIE$AGE))  # 59.56 +- 7.55
median(as.numeric(DEMOGRAPHIE$AGE)); quantile(as.numeric(DEMOGRAPHIE$AGE), 0.25); quantile(as.numeric(DEMOGRAPHIE$AGE), 0.75)  # 61 55 66

DEMOGRAPHIE %>% group_by(SEXE) %>% count() # Femme 181 Homme 339
DEMOGRAPHIE %>% group_by(ETHNIE) %>% count() # 94% Caucasien européen, 4% Afrique, 2% autre

mean(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT), na.rm=T) 
sd(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT), na.rm=T)  
median(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT), na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT), 0.25, na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT), 0.75, na.rm=T)  


mean(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_DIAG), na.rm=T) 
sd(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_DIAG), na.rm=T)  
median(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_DIAG), na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_DIAG), 0.25, na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_DIAG), 0.75, na.rm=T)  


mean(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_LDOPA), na.rm=T) 
sd(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_LDOPA), na.rm=T)  
median(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_LDOPA), na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_LDOPA), 0.25, na.rm=T)  
quantile(DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_LDOPA), 0.75, na.rm=T)  



DEMOGRAPHIE %>% select(SUBJID, D_SCREEN, D_TTT_DOPAM) %>% filter(D_TTT_DOPAM!="003") %>% mutate(D_TTT_DOPAM=as.numeric(D_TTT_DOPAM)) %>%
  drop_na() %>% mutate(target=(D_SCREEN-D_TTT_DOPAM)) %>%
  summarise(mean=mean(target),
            sd=sd(target),
            median=median(target),
            q1=quantile(target, 0.25),
            q3=quantile(target, 0.75) 
            )



DEMOGRAPHIE %>% select(SUBJID, D_SCREEN, D_FLUCTU_MOTR) %>% filter(D_FLUCTU_MOTR>="19") %>% mutate(D_FLUCTU_MOTR=as.numeric(D_FLUCTU_MOTR)) %>%
  drop_na() %>% mutate(target=(D_SCREEN-D_FLUCTU_MOTR)) %>%
  summarise(mean=mean(target),
            sd=sd(target),
            median=median(target),
            q1=quantile(target, 0.25),
            q3=quantile(target, 0.75) 
  )


DEMOGRAPHIE %>% select(SUBJID, D_SCREEN, D_FLUCTU_NONMOTR) %>% 
  filter(D_FLUCTU_NONMOTR!="0"&D_FLUCTU_NONMOTR!="0000") %>% mutate(D_FLUCTU_NONMOTR=as.numeric(D_FLUCTU_NONMOTR)) %>%
  drop_na() %>% mutate(target=(D_SCREEN-D_FLUCTU_NONMOTR)) %>%
  summarise(mean=mean(target),
            sd=sd(target),
            median=median(target),
            q1=quantile(target, 0.25),
            q3=quantile(target, 0.75) 
  )





DEMOGRAPHIE %>% select(SUBJID, D_SCREEN, D_DYSKINESIE) %>% 
  filter(D_DYSKINESIE!="0") %>% mutate(D_DYSKINESIE=as.numeric(D_DYSKINESIE)) %>%
  drop_na() %>% mutate(target=(D_SCREEN-D_DYSKINESIE)) %>%
  summarise(mean=mean(target),
            sd=sd(target),
            median=median(target),
            q1=quantile(target, 0.25),
            q3=quantile(target, 0.75) 
  )








# Age at DBS

DATES_DE_VISITES  <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DATES_DE_VISITES ", skip=0, col_types = "text", trim_ws = TRUE)
DATES_DE_VISITES <- DATES_DE_VISITES %>% select(SUBJID, D_CHIR)

DATES_DE_VISITES %>% inner_join(DEMOGRAPHIE %>% select(SUBJID, DDN)) %>% 
  mutate(D_CHIR=as.numeric(str_sub(D_CHIR, 7L, 10L))) %>%
  mutate(DDN=as.numeric(str_sub(DDN, 4L, 7))) %>% 
  summarise(mean=mean(D_CHIR-DDN, na.rm=T), sd=sd(D_CHIR-DDN, na.rm=T),
            median=median(D_CHIR-DDN, na.rm=T),
            q1=quantile(D_CHIR-DDN, 0.25, na.rm=T),
            q3=quantile(D_CHIR-DDN, 0.75, na.rm=T))




# UPDRS III Total V0
UPDRSIII_TOTAUX <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- SUBJID %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)

mean(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T) ;  sd(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T)  
median(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T) ;  
quantile(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), 0.25, na.rm=T)
quantile(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), 0.75, na.rm=T)  

mean(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), na.rm=T) ;  sd(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), na.rm=T)  
median(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), na.rm=T) ;  
quantile(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), 0.25, na.rm=T)  
quantile(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), 0.75, na.rm=T)  



mean((as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0))/as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T) 
sd((as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0))/as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T) 
median((as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0))/as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T) 
quantile((as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0))/as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), 0.25 ,na.rm=T) 
quantile((as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0))/as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), 0.75 ,na.rm=T) 


# UPDRS III Total V1
UPDRSIII_TOTAUX <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- SUBJID %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)

mean(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T) ;  sd(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T)  
median(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T) ;  
quantile(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), 0.25, na.rm=T)
quantile(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), 0.75, na.rm=T)  

mean(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), na.rm=T) ;  sd(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), na.rm=T)  
median(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), na.rm=T) ;  
quantile(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), 0.25, na.rm=T)  
quantile(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), 0.75, na.rm=T)  



mean((as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1))/as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T) 
sd((as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1))/as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T) 
median((as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1))/as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T) 
quantile((as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1))/as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), 0.25 ,na.rm=T) 
quantile((as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1) - as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1))/as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), 0.75 ,na.rm=T) 








# UPDRS II V0

UPDRSI_II <- fread("data/UPDRSI_II.txt")
UPDRSI_II <- UPDRSI_II %>% inner_join(SUBJID)
UPDRSI_II <- UPDRSI_II %>% filter(VISIT==0)

names(UPDRSI_II)

UPDRSI_II <- UPDRSI_II %>% 
  mutate(`Pre_OP_[OFF]`= MDS2_2OFF + MDS2_3OFF + MDS2_4OFF + MDS2_5OFF + MDS2_6OFF + MDS2_7OFF + MDS2_8OFF + MDS2_9OFF + MDS2_10OFF + MDS2_11OFF + MDS2_12OFF + MDS2_13OFF ) %>%
  mutate(`Pre_OP_[ON]`= MDS2_2ON + MDS2_3ON + MDS_2_4ON + MDS2_5ON + MDS2_6ON + MDS2_7ON + MDS2_8ON + MDS2_9ON + MDS2_10ON + MDS2_11ON + MDS2_12ON + MDS2_13ON )

mean(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T) 
sd(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T) 
median(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T)
quantile(UPDRSI_II$`Pre_OP_[OFF]`, 0.25, na.rm=T) 
quantile(UPDRSI_II$`Pre_OP_[OFF]`, 0.75, na.rm=T) 

mean(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T) 
sd(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T) 
median(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T)
quantile(UPDRSI_II$`Pre_OP_[ON]`, 0.25, na.rm=T) 
quantile(UPDRSI_II$`Pre_OP_[ON]`, 0.75, na.rm=T) 




# UPDRS II V1

UPDRSI_II <- fread("data/UPDRSI_II.txt")
UPDRSI_II <- UPDRSI_II %>% inner_join(SUBJID)
UPDRSI_II <- UPDRSI_II %>% filter(VISIT==1)

names(UPDRSI_II)

UPDRSI_II <- UPDRSI_II %>% 
  mutate(`Pre_OP_[OFF]`= MDS2_2OFF + MDS2_3OFF + MDS2_4OFF + MDS2_5OFF + MDS2_6OFF + MDS2_7OFF + MDS2_8OFF + MDS2_9OFF + MDS2_10OFF + MDS2_11OFF + MDS2_12OFF + MDS2_13OFF ) %>%
  mutate(`Pre_OP_[ON]`= MDS2_2ON + MDS2_3ON + MDS_2_4ON + MDS2_5ON + MDS2_6ON + MDS2_7ON + MDS2_8ON + MDS2_9ON + MDS2_10ON + MDS2_11ON + MDS2_12ON + MDS2_13ON )

mean(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T) 
sd(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T) 
median(UPDRSI_II$`Pre_OP_[OFF]`, na.rm=T)
quantile(UPDRSI_II$`Pre_OP_[OFF]`, 0.25, na.rm=T) 
quantile(UPDRSI_II$`Pre_OP_[OFF]`, 0.75, na.rm=T) 

mean(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T) 
sd(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T) 
median(UPDRSI_II$`Pre_OP_[ON]`, na.rm=T)
quantile(UPDRSI_II$`Pre_OP_[ON]`, 0.25, na.rm=T) 
quantile(UPDRSI_II$`Pre_OP_[ON]`, 0.75, na.rm=T) 






# PDQ39 V0
PDQ39 <- fread("data/PDQ39.txt")

PDQ39 <- PDQ39 %>% inner_join(SUBJID) %>% filter(VISIT==0)
PDQ39 <- PDQ39 %>% filter(item=="PDQ39_SCORE")

unique(PDQ39$item)

mean(PDQ39$value, na.rm=T) 
sd(PDQ39$value, na.rm=T) 
median(PDQ39$value, na.rm=T)
quantile(PDQ39$value, 0.25, na.rm=T) 
quantile(PDQ39$value, 0.75, na.rm=T) 


# PDQ39 V1
PDQ39 <- fread("data/PDQ39.txt")

PDQ39 <- PDQ39 %>% inner_join(SUBJID) %>% filter(VISIT==1)
PDQ39 <- PDQ39 %>% filter(item=="PDQ39_SCORE")

unique(PDQ39$item)

mean(PDQ39$value, na.rm=T) 
sd(PDQ39$value, na.rm=T) 
median(PDQ39$value, na.rm=T)
quantile(PDQ39$value, 0.25, na.rm=T) 
quantile(PDQ39$value, 0.75, na.rm=T) 




# S&E
Hoehn_YarhS_E <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "Hoehn&Yarh-S&E", skip=0, col_types = "text", trim_ws = TRUE)
Hoehn_YarhS_E <- SUBJID %>% inner_join(Hoehn_YarhS_E)
names(Hoehn_YarhS_E)
# S&E OFF
Hoehn_YarhS_E %>% select(SUBJID, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`),
            median=median(`Visite Bilan à 1 an - V1`), q1=quantile(`Visite Bilan à 1 an - V1`,0.25),
            q3=quantile(`Visite Bilan à 1 an - V1`,0.75))

Hoehn_YarhS_E %>% select(SUBJID, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`),
            median=median(`Visite de screening`), q1=quantile(`Visite de screening`,0.25),
            q3=quantile(`Visite de screening`,0.75))

# S&E ON
Hoehn_YarhS_E %>% select(SUBJID, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`),
            median=median(`Visite Bilan à 1 an - V1`), q1=quantile(`Visite Bilan à 1 an - V1`,0.25),
            q3=quantile(`Visite Bilan à 1 an - V1`,0.75))

Hoehn_YarhS_E %>% select(SUBJID, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`),
            median=median(`Visite de screening`), q1=quantile(`Visite de screening`,0.25),
            q3=quantile(`Visite de screening`,0.75))




# H&Y
Hoehn_YarhS_E <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "Hoehn&Yarh-S&E", skip=0, col_types = "text", trim_ws = TRUE)
# H&Y OFF
Hoehn_YarhS_E %>% select(SUBJID, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>% 
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  filter(!is.na(`Visite de screening`)) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`),
            median=median(`Visite de screening`),
            q1=quantile(`Visite de screening`,0.25),
            q3=quantile(`Visite de screening`,0.75))


Hoehn_YarhS_E %>% select(SUBJID, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  filter(!is.na(`Visite Bilan à 1 an - V1`)) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`),
            median=median(`Visite Bilan à 1 an - V1`),
            q1=quantile(`Visite Bilan à 1 an - V1`,0.25),
            q3=quantile(`Visite Bilan à 1 an - V1`,0.75))



# H&Y ON
Hoehn_YarhS_E %>% select(SUBJID, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  filter(!is.na(`Visite de screening`)) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`),
            median=median(`Visite de screening`),
            q1=quantile(`Visite de screening`,0.25),
            q3=quantile(`Visite de screening`,0.75))

Hoehn_YarhS_E %>% select(SUBJID, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  filter(!is.na(`Visite Bilan à 1 an - V1`)) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`),
            median=median(`Visite Bilan à 1 an - V1`),
            q1=quantile(`Visite Bilan à 1 an - V1`,0.25),
            q3=quantile(`Visite Bilan à 1 an - V1`,0.75))






# MoCA V0
MoCA_V0 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "MoCA V0", skip=0, col_types = "text", trim_ws = TRUE)
MoCA_V0 <- SUBJID %>% inner_join(MoCA_V0) %>% select(SUBJID, MOCA_SCORE)
mean(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
sd(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
median(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
quantile(as.numeric(MoCA_V0$MOCA_SCORE), 0.25, na.rm=T)
quantile(as.numeric(MoCA_V0$MOCA_SCORE), 0.75, na.rm=T)

# MoCA V1
MoCA_V1 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "MoCA V1", skip=0, col_types = "text", trim_ws = TRUE)
MoCA_V1 <- SUBJID %>% inner_join(MoCA_V1) %>% select(SUBJID, MOCA_SCORE)
mean(as.numeric(MoCA_V1$MOCA_SCORE), na.rm=T)
sd(as.numeric(MoCA_V1$MOCA_SCORE), na.rm=T)
median(as.numeric(MoCA_V1$MOCA_SCORE), na.rm=T)
quantile(as.numeric(MoCA_V1$MOCA_SCORE), 0.25, na.rm=T)
quantile(as.numeric(MoCA_V1$MOCA_SCORE), 0.75, na.rm=T)






# LEDD V0
LEDD_asymmetry <- fread("data/LEDD_asymmetry.csv")
LEDD_asymmetry <- LEDD_asymmetry %>% filter(visit=="Screening") %>% drop_na() %>% inner_join(SUBJID)
mean(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
sd(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
median(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.25, na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.75, na.rm=T)

# LEDD V1
LEDD_asymmetry <- fread("data/LEDD_asymmetry.csv")
LEDD_asymmetry <- LEDD_asymmetry %>% filter(visit=="V1") %>% drop_na() %>% inner_join(SUBJID)
mean(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
sd(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
median(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.25, na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.75, na.rm=T)





# Boston Fluence
Fluence_verbale <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "Fluence verbale", skip=0, col_types = "text", trim_ws = TRUE)
Fluence_verbale <- SUBJID %>% inner_join(Fluence_verbale) %>% select(SUBJID, TITRE_VISITE, FLUENCE_NBMOTCORRECT )

Fluence_verbale %>% group_by(TITRE_VISITE) %>%
  mutate(FLUENCE_NBMOTCORRECT=as.numeric(FLUENCE_NBMOTCORRECT)) %>%
  summarise(mean=mean(FLUENCE_NBMOTCORRECT, na.rm=T),
            sd=sd(FLUENCE_NBMOTCORRECT, na.rm=T),
            median=median(FLUENCE_NBMOTCORRECT, na.rm=T),
            q1=quantile(FLUENCE_NBMOTCORRECT, 0.25, na.rm=T),
            q3=quantile(FLUENCE_NBMOTCORRECT, 0.75, na.rm=T))


# Clox
Clox <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "Clox", skip=0, col_types = "text", trim_ws = TRUE)
Clox <- SUBJID %>% inner_join(Clox) %>% select(SUBJID, TITRE, CLOX_DESSIN_CALC, CLOX_COPIE_CALC )

Clox %>% group_by(TITRE) %>%
  mutate(CLOX_DESSIN_CALC=as.numeric(CLOX_DESSIN_CALC)) %>%
  summarise(mean=mean(CLOX_DESSIN_CALC, na.rm=T),
            sd=sd(CLOX_DESSIN_CALC, na.rm=T),
            median=median(CLOX_DESSIN_CALC, na.rm=T),
            q1=quantile(CLOX_DESSIN_CALC, 0.25, na.rm=T),
            q3=quantile(CLOX_DESSIN_CALC, 0.75, na.rm=T))

Clox %>% group_by(TITRE) %>%
  mutate(CLOX_COPIE_CALC=as.numeric(CLOX_COPIE_CALC)) %>%
  summarise(mean=mean(CLOX_COPIE_CALC, na.rm=T),
            sd=sd(CLOX_COPIE_CALC, na.rm=T),
            median=median(CLOX_COPIE_CALC, na.rm=T),
            q1=quantile(CLOX_COPIE_CALC, 0.25, na.rm=T),
            q3=quantile(CLOX_COPIE_CALC, 0.75, na.rm=T))




# HAMD
HAMD <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "HAMD", skip=0, col_types = "text", trim_ws = TRUE)
HAMD <- SUBJID %>% inner_join(HAMD) %>% select(SUBJID, TITRE, HAMD_SCORECALC )

HAMD %>% group_by(TITRE) %>%
  mutate(HAMD_SCORECALC=as.numeric(HAMD_SCORECALC)) %>%
  summarise(mean=mean(HAMD_SCORECALC, na.rm=T),
            sd=sd(HAMD_SCORECALC, na.rm=T),
            median=median(HAMD_SCORECALC, na.rm=T),
            q1=quantile(HAMD_SCORECALC, 0.25, na.rm=T),
            q3=quantile(HAMD_SCORECALC, 0.75, na.rm=T))

# HAMA
HAMA <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "HAMA ", skip=0, col_types = "text", trim_ws = TRUE)
HAMA <- SUBJID %>% inner_join(HAMA) %>% select(SUBJID, TITRE, HAMA_SCORECALC )

HAMA %>% group_by(TITRE) %>%
  mutate(HAMA_SCORECALC=as.numeric(HAMA_SCORECALC)) %>%
  summarise(mean=mean(HAMA_SCORECALC, na.rm=T),
            sd=sd(HAMA_SCORECALC, na.rm=T),
            median=median(HAMA_SCORECALC, na.rm=T),
            q1=quantile(HAMA_SCORECALC, 0.25, na.rm=T),
            q3=quantile(HAMA_SCORECALC, 0.75, na.rm=T))



# FOG Questionnaire

FOG <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "FOG", skip=0, col_types = "text", trim_ws = TRUE)

length(unique(FOG$SUBJID))

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




FOG %>% group_by(VISIT ) %>%
  mutate(fog_tot =as.numeric(fog_tot )) %>%
  summarise(mean=mean(fog_tot , na.rm=T),
            sd=sd(fog_tot , na.rm=T),
            median=median(fog_tot , na.rm=T),
            q1=quantile(fog_tot , 0.25, na.rm=T),
            q3=quantile(fog_tot , 0.75, na.rm=T))




# PDQ39
PDQ39 <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "PDQ39", skip=0, col_types = "text", trim_ws = TRUE)

PDQ39 <- PDQ39 %>% inner_join(SUBJID)

PDQ39 <- PDQ39 %>% select(SUBJID, VISIT , PDQ39_SCORE)

PDQ39 %>% group_by(VISIT) %>%
  mutate(PDQ39_SCORE =as.numeric(PDQ39_SCORE )) %>%
  summarise(mean=mean(PDQ39_SCORE , na.rm=T),
            sd=sd(PDQ39_SCORE , na.rm=T),
            median=median(PDQ39_SCORE , na.rm=T),
            q1=quantile(PDQ39_SCORE , 0.25, na.rm=T),
            q3=quantile(PDQ39_SCORE , 0.75, na.rm=T))







# MOCA
MOCA <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "MOCA_V0_V3", skip=0, col_types = "text", trim_ws = TRUE)

MOCA <- MOCA %>% inner_join(SUBJID)

MOCA <- MOCA %>% select(SUBJID, TITRE , MOCA_SCORE)

MOCA %>% group_by(TITRE) %>%
  mutate(MOCA_SCORE =as.numeric(MOCA_SCORE )) %>%
  summarise(mean=mean(MOCA_SCORE , na.rm=T),
            sd=sd(MOCA_SCORE , na.rm=T),
            median=median(MOCA_SCORE , na.rm=T),
            q1=quantile(MOCA_SCORE , 0.25, na.rm=T),
            q3=quantile(MOCA_SCORE , 0.75, na.rm=T))

MOCA <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "MOCA_V1_V5", skip=0, col_types = "text", trim_ws = TRUE)

MOCA <- MOCA %>% inner_join(SUBJID)

MOCA <- MOCA %>% select(SUBJID, TITRE , MOCA_SCORE)

MOCA %>% group_by(TITRE) %>%
  mutate(MOCA_SCORE =as.numeric(MOCA_SCORE )) %>%
  summarise(mean=mean(MOCA_SCORE , na.rm=T),
            sd=sd(MOCA_SCORE , na.rm=T),
            median=median(MOCA_SCORE , na.rm=T),
            q1=quantile(MOCA_SCORE , 0.25, na.rm=T),
            q3=quantile(MOCA_SCORE , 0.75, na.rm=T))








# UPPS
UPPS <- read_xlsx(path="data/Raquel_FOG_Dec2025.xlsx",sheet = "UPPS", skip=0, col_types = "text", trim_ws = TRUE)
UPPS <- SUBJID %>% inner_join(UPPS) 

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



UPPS %>% group_by(TITRE) %>%
  mutate(urgence =as.numeric(urgence )) %>%
  summarise(mean=mean(urgence , na.rm=T),
            sd=sd(urgence , na.rm=T),
            median=median(urgence , na.rm=T),
            q1=quantile(urgence , 0.25, na.rm=T),
            q3=quantile(urgence , 0.75, na.rm=T))


UPPS %>% group_by(TITRE) %>%
  mutate(urgence_positive =as.numeric(urgence_positive )) %>%
  summarise(mean=mean(urgence_positive , na.rm=T),
            sd=sd(urgence_positive , na.rm=T),
            median=median(urgence_positive , na.rm=T),
            q1=quantile(urgence_positive , 0.25, na.rm=T),
            q3=quantile(urgence_positive , 0.75, na.rm=T))


UPPS %>% group_by(TITRE) %>%
  mutate(manque_premeditation =as.numeric(manque_premeditation )) %>%
  summarise(mean=mean(manque_premeditation , na.rm=T),
            sd=sd(manque_premeditation , na.rm=T),
            median=median(manque_premeditation , na.rm=T),
            q1=quantile(manque_premeditation , 0.25, na.rm=T),
            q3=quantile(manque_premeditation , 0.75, na.rm=T))


UPPS %>% group_by(TITRE) %>%
  mutate(manque_perseverance =as.numeric(manque_perseverance )) %>%
  summarise(mean=mean(manque_perseverance , na.rm=T),
            sd=sd(manque_perseverance , na.rm=T),
            median=median(manque_perseverance , na.rm=T),
            q1=quantile(manque_perseverance , 0.25, na.rm=T),
            q3=quantile(manque_perseverance , 0.75, na.rm=T))


UPPS %>% group_by(TITRE) %>%
  mutate(recherche_sensation =as.numeric(recherche_sensation )) %>%
  summarise(mean=mean(recherche_sensation , na.rm=T),
            sd=sd(recherche_sensation , na.rm=T),
            median=median(recherche_sensation , na.rm=T),
            q1=quantile(recherche_sensation , 0.25, na.rm=T),
            q3=quantile(recherche_sensation , 0.75, na.rm=T))



# STIM PARAMS
STIM_PARAMS <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "Frequence V1_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

STIM_PARAMS <- STIM_PARAMS %>% select(SUBJID, VISITE , AMPLITUDEG1, 	DUREEG1,	FREQUENCEG1, AMPLITUDED1, 	DUREED1,	FREQUENCED1)

STIM_PARAMS <- STIM_PARAMS %>% inner_join(SUBJID)

STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(AMPLITUDEG1 =as.numeric(AMPLITUDEG1 )) %>%
  summarise(mean=mean(AMPLITUDEG1 , na.rm=T),
            sd=sd(AMPLITUDEG1 , na.rm=T),
            median=median(AMPLITUDEG1 , na.rm=T),
            q1=quantile(AMPLITUDEG1 , 0.25, na.rm=T),
            q3=quantile(AMPLITUDEG1 , 0.75, na.rm=T))

STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(DUREEG1 =as.numeric(DUREEG1 )) %>%
  summarise(mean=mean(DUREEG1 , na.rm=T),
            sd=sd(DUREEG1 , na.rm=T),
            median=median(DUREEG1 , na.rm=T),
            q1=quantile(DUREEG1 , 0.25, na.rm=T),
            q3=quantile(DUREEG1 , 0.75, na.rm=T))

STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(FREQUENCEG1 =as.numeric(FREQUENCEG1 )) %>%
  summarise(mean=mean(FREQUENCEG1 , na.rm=T),
            sd=sd(FREQUENCEG1 , na.rm=T),
            median=median(FREQUENCEG1 , na.rm=T),
            q1=quantile(FREQUENCEG1 , 0.25, na.rm=T),
            q3=quantile(FREQUENCEG1 , 0.75, na.rm=T))


STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(AMPLITUDED1 =as.numeric(AMPLITUDED1 )) %>%
  summarise(mean=mean(AMPLITUDED1 , na.rm=T),
            sd=sd(AMPLITUDED1 , na.rm=T),
            median=median(AMPLITUDED1 , na.rm=T),
            q1=quantile(AMPLITUDED1 , 0.25, na.rm=T),
            q3=quantile(AMPLITUDED1 , 0.75, na.rm=T))

STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(DUREED1 =as.numeric(DUREED1 )) %>%
  summarise(mean=mean(DUREED1 , na.rm=T),
            sd=sd(DUREED1 , na.rm=T),
            median=median(DUREED1 , na.rm=T),
            q1=quantile(DUREED1 , 0.25, na.rm=T),
            q3=quantile(DUREED1 , 0.75, na.rm=T))

STIM_PARAMS %>% group_by(VISITE) %>%
  mutate(FREQUENCED1 =as.numeric(FREQUENCED1 )) %>%
  summarise(mean=mean(FREQUENCED1 , na.rm=T),
            sd=sd(FREQUENCED1 , na.rm=T),
            median=median(FREQUENCED1 , na.rm=T),
            q1=quantile(FREQUENCED1 , 0.25, na.rm=T),
            q3=quantile(FREQUENCED1 , 0.75, na.rm=T))



# UPDRS III ON
UPDRSIII_V3_V5 <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSIII complet V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSIII_V3_V5 <- UPDRSIII_V3_V5 %>% inner_join(SUBJID)

UPDRSIII_V3_V5 <- UPDRSIII_V3_V5 %>% select(SUBJID, VISIT , ON_TOTAL)

UPDRSIII_V3_V5 %>% group_by(VISIT) %>%
  mutate(ON_TOTAL =as.numeric(ON_TOTAL )) %>%
  summarise(mean=mean(ON_TOTAL , na.rm=T),
            sd=sd(ON_TOTAL , na.rm=T),
            median=median(ON_TOTAL , na.rm=T),
            q1=quantile(ON_TOTAL , 0.25, na.rm=T),
            q3=quantile(ON_TOTAL , 0.75, na.rm=T))


# UPDRS II 
UPDRSII_V3_V5 <- read_xlsx(path="data/Raquel FOG_Nov 2025.xlsx",sheet = "UPDRSII complet V0_V1_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

UPDRSII_V3_V5 <- UPDRSII_V3_V5 %>% inner_join(SUBJID)

UPDRSII_V3_V5 <- UPDRSII_V3_V5 %>% select(SUBJID, VISIT , MDSUPDRSPARTIE2_ON, MDSUPDRSPARTIE2_OFF)

UPDRSII_V3_V5 %>% group_by(VISIT) %>%
  mutate(MDSUPDRSPARTIE2_ON =as.numeric(MDSUPDRSPARTIE2_ON )) %>%
  summarise(mean=mean(MDSUPDRSPARTIE2_ON , na.rm=T),
            sd=sd(MDSUPDRSPARTIE2_ON , na.rm=T),
            median=median(MDSUPDRSPARTIE2_ON , na.rm=T),
            q1=quantile(MDSUPDRSPARTIE2_ON , 0.25, na.rm=T),
            q3=quantile(MDSUPDRSPARTIE2_ON , 0.75, na.rm=T))


UPDRSII_V3_V5 %>% group_by(VISIT) %>%
  mutate(MDSUPDRSPARTIE2_OFF =as.numeric(MDSUPDRSPARTIE2_OFF )) %>%
  summarise(mean=mean(MDSUPDRSPARTIE2_OFF , na.rm=T),
            sd=sd(MDSUPDRSPARTIE2_OFF , na.rm=T),
            median=median(MDSUPDRSPARTIE2_OFF , na.rm=T),
            q1=quantile(MDSUPDRSPARTIE2_OFF , 0.25, na.rm=T),
            q3=quantile(MDSUPDRSPARTIE2_OFF , 0.75, na.rm=T))




# S&E  H&Y V3 V5
SaE_v3_V5 <- read_xlsx(path="data/Raquel_Margherita_Juil 24.xlsx",sheet = "Hoehn&Yahr-S&E_V3_V5", skip=0, col_types = "text", trim_ws = TRUE)

SaE_v3_V5 <- SaE_v3_V5 %>% inner_join(SUBJID)

SaE_v3_V5 <- SaE_v3_V5 %>% select(SUBJID, TITRE, HOEHN_YAHR_ON,	HOEHN_YAHR_OFF,	SCHWAB_ON,	SCHWAB_OFF)

SaE_v3_V5 %>% group_by(TITRE) %>%
  mutate(HOEHN_YAHR_ON =as.numeric(HOEHN_YAHR_ON )) %>%
  summarise(mean=mean(HOEHN_YAHR_ON , na.rm=T),
            sd=sd(HOEHN_YAHR_ON , na.rm=T),
            median=median(HOEHN_YAHR_ON , na.rm=T),
            q1=quantile(HOEHN_YAHR_ON , 0.25, na.rm=T),
            q3=quantile(HOEHN_YAHR_ON , 0.75, na.rm=T))

SaE_v3_V5 %>% group_by(TITRE) %>%
  mutate(HOEHN_YAHR_OFF =as.numeric(HOEHN_YAHR_OFF )) %>%
  summarise(mean=mean(HOEHN_YAHR_OFF , na.rm=T),
            sd=sd(HOEHN_YAHR_OFF , na.rm=T),
            median=median(HOEHN_YAHR_OFF , na.rm=T),
            q1=quantile(HOEHN_YAHR_OFF , 0.25, na.rm=T),
            q3=quantile(HOEHN_YAHR_OFF , 0.75, na.rm=T))

SaE_v3_V5 %>% group_by(TITRE) %>%
  mutate(SCHWAB_ON =as.numeric(SCHWAB_ON )) %>%
  summarise(mean=mean(SCHWAB_ON , na.rm=T),
            sd=sd(SCHWAB_ON , na.rm=T),
            median=median(SCHWAB_ON , na.rm=T),
            q1=quantile(SCHWAB_ON , 0.25, na.rm=T),
            q3=quantile(SCHWAB_ON , 0.75, na.rm=T))

SaE_v3_V5 %>% group_by(TITRE) %>%
  mutate(SCHWAB_OFF =as.numeric(SCHWAB_OFF )) %>%
  summarise(mean=mean(SCHWAB_OFF , na.rm=T),
            sd=sd(SCHWAB_OFF , na.rm=T),
            median=median(SCHWAB_OFF , na.rm=T),
            q1=quantile(SCHWAB_OFF , 0.25, na.rm=T),
            q3=quantile(SCHWAB_OFF , 0.75, na.rm=T))
