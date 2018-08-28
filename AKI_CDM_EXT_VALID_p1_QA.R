rm(list=ls())
gc()

#source utility functions
source("./R/helper_functions.R")
source("./R/extract_cohort.R")

#load libraries
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "ROracle",
                    "DBI"))


#establish the connection between r-studio and CDM server
config_file_path<-"../config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-dbConnect(Oracle(),
                config_file$username,
                config_file$password,
                config_file$access)

#extract cohort --Table1
cohort<-extract_cohort(conn,
                       cdm_db_schema=config_file$cdm_db_schema,
                       oracle_temp_schema=cdm_db_schema,
                       start_date="2010-01-01",
                       end_date="2018-12-31")

#save Table1
Table1<-cohort$aki_enc
save(Table1,file="./data/Table1.Rdata")

#print out attrition table
consort_tbl<-cohort$attrition
save(consort_tbl,file="./data/consort_tbl.Rdata")
print(cohort$attrition)
#TODO: consort diagram

#clean up
rm(cohort,consort_tbl); gc()


#collectand summarize variables
load("./data/Table1.Rdata")
# auxilliary summaries and tables
enc_tot<-length(unique(Table1$ENCOUNTERID))
# critical dates of AKI encounters
aki_stage_ind<-Table1 %>%
  dplyr::select(PATID, ENCOUNTERID, ADMIT_DATE, DISCHARGE_DATE,
                NONAKI_ANCHOR, AKI1_ONSET,AKI2_ONSET,AKI3_ONSET) %>%
  gather(chk_pt, critical_date,-PATID,-ENCOUNTERID) %>%
  filter(!is.na(critical_date)) %>%
  mutate(chk_pt=gsub("_.*","",chk_pt)) %>%
  group_by(chk_pt) %>%
  dplyr::mutate(stg_tot_cnt=n()) %>%
  ungroup %>%
  arrange(PATID, ENCOUNTERID, chk_pt, critical_date, stg_tot_cnt)

## demographic
demo<-dbGetQuery(conn,
                 parse_sql("./inst/collect_demo.sql",
                           cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  mutate(AGE_GRP=case_when(AGE<= 25 ~ "18-25",
                           AGE >= 26 & AGE <= 35 ~ "26-35",
                           AGE >= 36 & AGE <= 45 ~ "36-45",
                           AGE >= 46 & AGE <= 55 ~ "46-55",
                           AGE >= 56 & AGE <= 65 ~ "56-65",
                           AGE >= 66 ~ "66<=")) %>%
  dplyr::select(PATID,ENCOUNTERID,
                AGE,AGE_GRP,SEX,RACE,HISPANIC,DDAYS_SINCE_ENC) %>%
  replace_na(list(AGE="NI",
                  AGE_GRP="NI",
                  SEX="NI",
                  RACE="NI",
                  HISPANIC="NI")) %>%
  gather(key,value,-PATID,-ENCOUNTERID) %>%
  unique

#save
save(demo,file="./data/AKI_demo.Rdata")


#summaries
demo_summ<-aki_stage_ind %>% 
  filter(!chk_pt %in% c("DISCHARGE")) %>%
  dplyr::select(-critical_date) %>%
  left_join(demo %>% 
              filter(!(key %in% c("AGE","DDAYS_SINCE_ENC"))), 
            by="ENCOUNTERID") %>%
  group_by(chk_pt,stg_tot_cnt,key,value) %>%
  dplyr::summarize(enc_cnt = n(),
                   enc_prop = round(n()/stg_tot_cnt[1],2)) %>%
  ungroup %>% dplyr::select(-stg_tot_cnt) %>%
  gather(summ,summ_val,-chk_pt,-key,-value) %>%
  # # decode demo_val
  # left_join(meta %>% dplyr::select(COLUMN_NAME,VAR_CODE,VAR_NAME),
  #           by=c("demo_type"="COLUMN_NAME","demo_val"="VAR_CODE")) %>%
  # dplyr::mutate(demo_val = ifelse(!is.na(VAR_NAME),VAR_NAME,demo_val)) %>%
  # dplyr::select(-VAR_NAME) %>%
  # unite("demo_type_cat",c("demo_type","demo_val")) %>%
  # attach totals at bottom
  bind_rows(aki_stage_ind %>%
              filter(!chk_pt %in% c("DISCHARGE")) %>%
              dplyr::select(chk_pt,stg_tot_cnt) %>% 
              unique %>% 
              dplyr::rename(enc_cnt=stg_tot_cnt) %>%
              mutate(enc_prop=round(enc_cnt/enc_tot,2),
                     key="TOTAL",
                     value="(%/overall)") %>%
              gather(summ,summ_val,-chk_pt,-key,-value) %>%
              dplyr::select(key,value,chk_pt,summ,summ_val)) %>%
  unite("stg_summ",c("chk_pt","summ")) %>%
  unique %>% spread(stg_summ,summ_val) %>%
  replace(.,is.na(.),0)

save(demo_summ,file="./data/demo_summ.Rdata")
#clean up
rm(demo,demo_summ); gc()

## vital
#vital: HT,WT,BMI,BP
vital<-dbGetQuery(conn,
                  parse_sql("./inst/collect_vital.sql",
                            cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  mutate(BMI_GRP = case_when(ORIGINAL_BMI <= 25 ~ "BMI <= 25",
                             ORIGINAL_BMI > 25 &  ORIGINAL_BMI <= 30 ~ "BMI 26-30",
                             ORIGINAL_BMI >=31  ~ "BMI >= 31")) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(dsa=round(as.numeric(difftime(MEASURE_DATE_TIME,critical_date,units="days")),2)) %>%
  dplyr::select(-MEASURE_DATE_TIME,-critical_date,-chk_pt,-stg_tot_cnt) %>%
  gather(key,value,-PATID,-ENCOUNTERID,-dsa) %>%
  filter(!is.na(key) & !is.na(value)) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,dsa) %>%
  mutate(key=recode(key,
                    ORIGINAL_BMI="BMI",
                    SYSTOLIC="BP_SYSTOLIC",
                    DIASTOLIC="BP_DIASTOLIC")) %>%
  unique

#save
save(vital,file="./data/AKI_vital.Rdata")


# collect summaries
vital_summ<-vital %>%
  dplyr::select(ENCOUNTERID, key, value, dsa) %>%
  filter(key %in% c("HT","WT","BMI","BP_DIASTOLIC","BP_SYSTOLIC")) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(param_low=case_when(key=="HT" ~ 0,
                             key=="WT" ~ 0,
                             key=="BMI" ~ 0,
                             key %in% c("BP_DIASTOLIC",
                                        "BP_SYSTOLIC") ~ 40),
         param_high=case_when(key=="HT" ~ 94.99,
                              key=="WT" ~ 350,
                              key=="BMI" ~ 50,
                              key=="BP_DIASTOLIC"~120,
                              key=="BP_SYSTOLIC" ~ 210)) %>%
  mutate(dsa_grp=case_when(dsa < 0 ~ "[-7,0)",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 ~ "3<")) %>%
  group_by(key,dsa_grp) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   low_cnt=sum((value<param_low)),
                   high_cnt=sum((value>param_high)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T)),
                   sd=round(sd(value,na.rm=T)),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T)) %>%
  mutate(cov=round(sd/mean,1)) %>%
  ungroup %>%
  gather(summ,summ_val,-key,-dsa_grp) %>%
  spread(dsa_grp,summ_val) %>%
  mutate(summ=recode(summ,
                     enc_cnt="1.encounters#",
                     record_cnt="2.records#",
                     low_cnt="3.low_records#",
                     high_cnt="4.high_records#",
                     min="5a.min",
                     median="5b.median",
                     mean="5c.mean",
                     sd="5d.sd",
                     cov="5e.cov",
                     max="5f.max")) %>%
  arrange(key,summ)

#save
save(vital_summ,file="./data/vital_summ.Rdata")
rm(vital,vital_summ); gc()

##TODO: scalability issue
# # personalized outlier identification - SBP, DBP
# vital_summ2<-vital %>%
#   dplyr::select(ENCOUNTERID, key, value, dsa) %>%
#   filter(key %in% c("BP_DIASTOLIC","BP_SYSTOLIC")) %>%
#   group_by(ENCOUNTERID,key) %>%
#   dplyr::mutate(freq=n())
#   
# freq_filter<-quantile(vital_summ2$freq,probs=0.05)  
# 
# vital_summ2 %<>%
#   filter(freq >= freq_filter) %>%
#   dplyr::mutate(loess_fit=predict(loess(value~dsa),se=T)$fit,
#                 loess_se=predict(loess(value~dsa),se=T)$se) %>%
#   dplyr::mutate(low_bd=loess_fit-2.33*sqrt(freq)*loess_se,
#                 up_bd=loess_fit+2.33*sqrt(freq)*loess_se) %>%
#   ungroup %>%
#   dplyr::mutate(outlier_ind = ifelse(value > up_bd | value < low_bd, 1,0)) %>%
#   group_by(ENCOUNTERID) %>%
#   dplyr::mutate(outlier_prop=mean(outlier_ind)) %>%
#   ungroup %>%
#   group_by(key) %>%
#   dplyr::summarize(low_bd_mean=mean(low_bd,na.rm=T),
#                    low_bd_sd=sd(low_bd,na.rm=T),
#                    up_bd_mean=mean(up_bd,na.rm=T),
#                    up_bd_sd=sd(up_bd,na.rm=T),
#                    outliers_prop_mean=mean(outlier_prop))
# 
# save(vital_summ2,file="./data/vital_summ2.Rdata")  


## labs
lab<-dbGetQuery(conn,
                parse_sql("./inst/collect_lab.sql",
                          cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,LAB_LOINC,RESULT_NUM,RESULT_UNIT,SPECIMEN_DATE_TIME) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(dsa=round(as.numeric(difftime(SPECIMEN_DATE_TIME,critical_date,units="days")),2)) %>%
  dplyr::rename(key=LAB_LOINC,value=RESULT_NUM,unit=RESULT_UNIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,unit,dsa) %>%
  filter(!is.na(key) & !is.na(value)) %>%
  unique

#save
save(lab,file="./data/AKI_lab.Rdata")

#collect summaries
lab_summ<-lab %>% 
  mutate(dsa_grp=case_when(dsa < 0 ~ "[-7,0)",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 ~ "3<")) %>%
  group_by(key,dsa_grp) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T)),
                   sd=round(sd(value,na.rm=T)),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T)) %>%
  mutate(cov=round(sd/mean,1)) %>%
  ungroup %>%
  gather(summ,summ_val,-key,-dsa_grp) %>%
  spread(dsa_grp,summ_val) %>%
  mutate(summ=recode(summ,
                     enc_cnt="1.encounters#",
                     record_cnt="2.records#",
                     min="3a.min",
                     median="3b.median",
                     mean="3c.mean",
                     sd="3d.sd",
                     cov="3e.cov",
                     max="3f.max")) %>%
  arrange(key,summ) 

lab_summ %<>%
  mutate(at_admission=ifelse(is.na(`1`),0,1),
         within_3d=ifelse(is.na(coalesce(`1`,`2`,`3`)),0,1),
         daily_moniter=ifelse(!is.na(`1`)&!is.na(`2`)&!is.na(`3`),1,0))
  
save(lab_summ,file="./data/lab_summ.Rdata")

#clean up
rm(lab,lab_summ); gc()


## medication
med<-dbGetQuery(conn,
                parse_sql("./inst/collect_med.sql",
                          cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  dplyr::mutate(RX_EXPOS=pmin(RX_END_DATE_ADJ,RX_END_DATE,na.rm=T)-RX_START_DATE) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_EXPOS) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(dsa=round(as.numeric(difftime(RX_START_DATE,critical_date,units="days")))) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_EXPOS,RX_QUANTITY_DAILY,dsa) %>%
  unique

#expand table with daily exposure 
med_expand<-med[rep(rownames(med),each=med$RX_EXPOS),] %>%
  mutate(RX_EXPOS=1)

#collect summaries



#save
save(med_expand,file="./data/AKI_med.Rdata")


## admission DRG
admit_DRG<-dbGetQuery(conn,
                      parse_sql("./inst/collect_enc.sql",
                                cdm_db_schema=cdm_db_schema)$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,DRG,ADMITTING_SOURCE) %>%
  gather(key1,key2,-PATID,-ENCOUNTERID) %>%
  unite("key",c("key1","key2"),sep=":") %>% 
  mutate(value=1) %>% unique %>%
  dplyr::select(PATID, ENCOUNTERID, key, value)
#save
save(admit_DRG,file="./data/AKI_admit_DRG.Rdata")


## diagnosis
load("./data/ccs_icd_cw.Rdata")
dx<-dbGetQuery(conn,
               parse_sql("./inst/collect_dx.sql",
                         cdm_db_schema=cdm_db_schema)$statement) %>%
  #attach CCS diagnosis grouping
  dplyr::mutate(DX_ICD=paste0("ICD",DX_TYPE,":",DX)) %>%
  left_join(ccs_icd %>% select(-ccs_name),by=c("DX_ICD"="icd_w_type")) %>%
  unique %>% filter(!is.na(ccs_code)) %>%
  dplyr::rename(DX_CCS=ccs_code) %>%
  dplyr::select(PATID,ENCOUNTERID,DX_ICD,DX_CCS,DAYS_SINCE_ADMIT) %>%
  unique
#save
save(dx,file="./data/AKI_dx.Rdata")  


## procedure
px<-dbGetQuery(conn,
               parse_sql("./inst/collect_px.sql",
                         cdm_db_schema=cdm_db_schema)$statement) %>%
  dplyr::mutate(PX=paste0(PX_TYPE,":",PX)) %>%
  dplyr::select(PATID,ENCOUNTERID,PX,DAYS_SINCE_ADMIT) %>%
  unique
#save
save(px,file="./data/AKI_px.Rdata")  



  
#illogical dates (aki onsets before birth or after death)
aki_bad_dates<-aki_stage_ind<-tbl1 %>%
  dplyr::select(ENCOUNTERID,
                AKI1_ONSET,AKI2_ONSET,AKI3_ONSET) %>%
  gather(chk_pt, onset, -ENCOUNTERID) %>%
  filter(!is.na(onset)) %>%
  dplyr::select(ENCOUNTERID, chk_pt)%>%
  mutate(chk_pt=gsub("_.*","",chk_pt))
