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


## vital
#vital: HT,WT,BMI
vital<-dbGetQuery(conn,
                  parse_sql("./inst/collect_vital.sql",
                            cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  mutate(BMI_GRP = case_when(ORIGINAL_BMI <= 25 ~ "BMI <= 25",
                             ORIGINAL_BMI > 25 &  ORIGINAL_BMI <= 30 ~ "BMI 26-30",
                             ORIGINAL_BMI >=31  ~ "BMI >= 31")) %>%
  gather(key,value,-PATID,-ENCOUNTERID,-VITALID,-MEASURE_DATE_TIME) %>%
  filter(!is.na(key)) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by="ENCOUNTERID") %>%
  dplyr::mutate(dsa=MEASURE_DATE_TIME-critical_date) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,dsa)
  
#save
save(vital,file="./data/AKI_vital.Rdata")


# collect summaries
ht_wt_bmi_summ<-ht_wt_bmi %>%
  dplyr::select(PATID, HT, WT) %>%
  dplyr::filter(!is.na(HT) | !is.na(WT)) %>%
  left_join(aki_stage_idx, by="PATID") %>%
  dplyr::summarize(HT_records=sum(!is.na(HT)),
                   HT_mean=mean(HT,na.rm=T),
                   HT_median=median(HT,na.rm=T))

ht_wt_bmi<-vital %>% 
  dplyr::select(PATID, VITALID, MEASURE_DATE_TIME,
                HT,WT,ORIGINAL_BMI) %>%
  dplyr::filter(!is.na(HT) & !is.na(WT)) %>%
  left_join(demo %>% dplyr::select(PATID,AGE_GRP,SEX) %>% unique,
            by="PATID") %>%
  gather(key,val,-PATID,-age_grp,-SEX,-VITALID,-MEASURE_DATE_TIME) %>%
  group_by(age_grp,SEX,key) %>%
  dplyr::mutate(low_bd=ifelse(key=="ORIGINAL_BMI",
                              pmax(10,median(val,na.rm=T)-3*IQR(val,na.rm=T,type=8)),
                              pmax(50,median(val,na.rm=T)-3*IQR(val,na.rm=T,type=8))),   
                up_bd=pmin(500,median(val,na.rm=T)+4.5*IQR(val,na.rm=T,type=8))) %>% 
  ungroup %>%
  dplyr::mutate(outlier_ind = ifelse(val > up_bd | val < low_bd, 1,0))

#outliers summaries
ht_wt_bmi %>% filter(outlier_ind==1) %>%
  group_by(age_grp,SEX,key,low_bd,up_bd) %>%
  dplyr::summarize(pat_cnt=length(unique(PATID))) %>% 
  ungroup %>% unique %>% arrange(age_grp,SEX,key) %>%
  View

#vital: SBP, DBP
#detect outliers - SYSTOLIC, DIASTOLIC, at encounter level
# AKI_VITAL_test<-AKI_VITAL %>% dplyr::filter(PATID < 100)
# scalability issue!
bp<-AKI_VITAL %>% 
  dplyr::select(ENCOUNTERID,VITALID, MEASURE_DATE_TIME,
                SYSTOLIC, DIASTOLIC) %>%
  dplyr::filter(!is.na(SYSTOLIC) & !is.na(DIASTOLIC)) %>%
  gather(key,val,-ENCOUNTERID,-VITALID, -MEASURE_DATE_TIME) %>%
  group_by(key,ENCOUNTERID) %>%
  dplyr::mutate(measure_time=rank(MEASURE_DATE_TIME),
                freq=n()) %>%
  dplyr::mutate(loess_fit=predict(loess(val~measure_time),se=T)$fit,
                loess_se=predict(loess(val~measure_time),se=T)$se) %>%
  dplyr::mutate(low_bd=loess_fit-1.5*sqrt(freq)*loess_se,
                up_bd=loess_fit+1.5*sqrt(freq)*loess_se) %>%
  ungroup %>%
  dplyr::mutate(outlier_ind = ifelse(val > up_bd | val < low_bd, 1,0))


#outliers summaries
bp %>% filter(outlier_ind==1) %>%
  group_by(key) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTERID)),
                   low_bd=median(low_bd),
                   low_bd_max=max(low_bd),
                   up_bd=median(up_bd),
                   up_bd=min(up_bd)) %>%
  View  


## labs
lab<-dbGetQuery(conn,
                parse_sql("./inst/collect_lab.sql",
                          cdm_db_schema=cdm_db_schema)$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,VITALID,LAB_LOINC,RESULT_NUM,RESULT_UNIT,SPECIMEN_DATE_TIME) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by="ENCOUNTERID") %>%
  dplyr::mutate(dsa=SPECIMEN_DATE_TIME-critical_date) %>%
  dplyr::select(PATID,ENCOUNTERID,LAB_LOINC,RESULT_NUM,RESULT_UNIT,dsa) %>%
  unique
#save
save(lab,file="./data/AKI_lab.Rdata")


## medication
med<-dbGetQuery(conn,
                parse_sql("./inst/collect_med.sql",
                          cdm_db_schema=cdm_db_schema)$statement) %>%
  dplyr::mutate(RX_EXPOS=pmin(RX_END_DATE_ADJ,RX_END_DATE)-RX_START_DATE) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_EXPOS) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by="ENCOUNTERID") %>%
  dplyr::mutate(dsa=RX_START_DATE-critical_date) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_EXPOS,RX_QUANTITY_DAILY,dsa) %>%
  unique

med_expand<-med[rep(rownames(med),each=med$RX_EXPOS),] %>%
  mutate(RX_EXPOS=1)

#save
save(med_expand,file="./data/AKI_med.Rdata")


## admission DRG
admit_DRG<-dbGetQuery(conn,
                      parse_sql("./inst/collect_enc.sql",
                                cdm_db_schema=cdm_db_schema)$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,DRG,ADMITTING_SOURCE) %>%
  unique
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
