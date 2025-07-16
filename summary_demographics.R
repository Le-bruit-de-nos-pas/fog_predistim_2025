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





# PDQ39 

# PDQ29 baseline -> Pre_OP   51.9 

# UPDRS III Total
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




# UPDRS II

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

PDQ39 <- fread("data/PDQ39.txt")

PDQ39 <- PDQ39 %>% inner_join(SUBJID) %>% filter(VISIT==0)
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


Hoehn_YarhS_E %>% select(SUBJID, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`),
            median=median(`Visite de screening`), q1=quantile(`Visite de screening`,0.25),
            q3=quantile(`Visite de screening`,0.75))





Hoehn_YarhS_E <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "Hoehn&Yarh-S&E", skip=0, col_types = "text", trim_ws = TRUE)


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




# MoCA

MoCA_V0 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "MoCA V0", skip=0, col_types = "text", trim_ws = TRUE)
MoCA_V0 <- SUBJID %>% inner_join(MoCA_V0) %>% select(SUBJID, MOCA_SCORE)

mean(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
sd(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
median(as.numeric(MoCA_V0$MOCA_SCORE), na.rm=T)
quantile(as.numeric(MoCA_V0$MOCA_SCORE), 0.25, na.rm=T)
quantile(as.numeric(MoCA_V0$MOCA_SCORE), 0.75, na.rm=T)



LEDD_asymmetry <- fread("data/LEDD_asymmetry.csv")

LEDD_asymmetry <- LEDD_asymmetry %>% filter(visit=="Screening") %>% drop_na() %>% inner_join(SUBJID)


mean(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
sd(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
median(as.numeric(LEDD_asymmetry$LEDD), na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.25, na.rm=T)
quantile(as.numeric(LEDD_asymmetry$LEDD), 0.75, na.rm=T)
