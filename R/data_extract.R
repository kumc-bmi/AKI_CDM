##############################
#### AKI - Data Extraction####
##############################

rm(list=ls())
gc()

source("./R/util.R")

#load libraries
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr",
                    "kableExtra",
                    "ggplot2",
                    "ggrepel",
                    "RCurl",
                    "XML",
                    "openxlsx"))

params<-list(DBMS_type="Oracle",
             driver_type="JDBC",
             start_date="2010-01-01",
             end_date="2018-12-31")

#establish the connection between r-studio and CDM server (Oracle)
config_file<-read.csv("./config/config.csv",stringsAsFactors = F)
conn<-connect_to_db(DBMS_type=params$DBMS_type,
                    driver_type=params$driver_type,
                    config_file=config_file)


####======extract cohort --Table1========####
# by default, we assume cdm schema is on the same server as current schema,
# wrapper function to extract AKI cohort (intermediate tables will be created)
cohort<-extract_cohort(conn,
                       cdm_db_name=config_file$cdm_db_name,
                       cdm_db_schema=config_file$cdm_db_schema,
                       start_date=params$start_date,
                       end_date=params$end_date,
                       verb=F)

Table1<-cohort$aki_enc
consort_tbl<-cohort$attrition

# collect summaries
enc_tot<-length(unique(Table1$ENCOUNTERID))

tbl1_dsa<-Table1 %>% 
  dplyr::select(PATID,ENCOUNTERID,
                NONAKI_SINCE_ADMIT, 
                AKI1_SINCE_ADMIT,
                AKI2_SINCE_ADMIT,
                AKI3_SINCE_ADMIT) %>%
  gather(stage, days_since_admit,-PATID,-ENCOUNTERID) %>%
  mutate(stage=gsub("_.*","",stage)) %>% 
  filter(!is.na(days_since_admit)) 

tbl1_summ<-tbl1_dsa %>%
  group_by(stage) %>%
  dplyr::summarize(pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min_time=min(days_since_admit,na.rm=T),
                   q1_time=quantile(days_since_admit,probs=0.25,na.rm=T),
                   median_time=median(days_since_admit,na.rm=T),
                   mean_time=round(mean(days_since_admit,na.rm=T),1),
                   q3_time=quantile(days_since_admit,probs=0.75,na.rm=T),
                   max_time=max(days_since_admit,na.rm=T),
                   sd_time=round(sd(days_since_admit,na.rm=T),2),
                   .groups="drop") %>%
  mutate(semi_IQR_time=0.5*(q3_time-q1_time)) %>%
  #HIPAA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",as.character(pat_cnt)),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt)))

#save results
saveRDS(Table1,file="./data/raw/Table1.rda")
# saveRDS(consort_tbl,file="./data/consort_tbl.rda")

#clean up
rm(cohort); gc()


#consort diagram
pdf(file="./figure/consort_diagram.pdf",
    width=8,
    height=8)
consort_diag(consort_tbl)
dev.off()


# auxilliary summaries and tables
enc_tot<-length(unique(Table1$ENCOUNTERID))
# critical dates of AKI encounters
aki_stage_ind<-Table1 %>%
  dplyr::select(ENCOUNTERID, ADMIT_DATE, DISCHARGE_DATE,
                NONAKI_ANCHOR, AKI1_ONSET,AKI2_ONSET,AKI3_ONSET) %>%
  gather(chk_pt, critical_date,-ENCOUNTERID) %>%
  filter(!is.na(critical_date)) %>%
  mutate(chk_pt=gsub("_.*","",chk_pt)) %>%
  group_by(chk_pt) %>%
  dplyr::mutate(stg_tot_cnt=n()) %>%
  ungroup %>%
  arrange(ENCOUNTERID, chk_pt, critical_date, stg_tot_cnt)


## demographic
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_demo.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

demo<-execute_single_sql(conn,
                         statement=sql$statement,
                         write=(sql$action=="write")) %>%
  mutate(AGE_GRP=case_when(AGE<= 25 ~ "18-25",
                           AGE >= 26 & AGE <= 35 ~ "26-35",
                           AGE >= 36 & AGE <= 45 ~ "36-45",
                           AGE >= 46 & AGE <= 55 ~ "46-55",
                           AGE >= 56 & AGE <= 65 ~ "56-65",
                           AGE >= 66 ~ "66<=")) %>%
  dplyr::select(ENCOUNTERID,
                AGE,AGE_GRP,SEX,RACE,HISPANIC,DDAYS_SINCE_ENC) %>%
  replace_na(list(AGE="NI",
                  AGE_GRP="NI",
                  SEX="NI",
                  RACE="NI",
                  HISPANIC="NI")) %>%
  gather(key,value,-ENCOUNTERID) %>%
  unique


#collect summaries
demo_summ<-aki_stage_ind %>% 
  dplyr::filter(!chk_pt %in% c("DISCHARGE")) %>%
  dplyr::select(-critical_date) %>%
  left_join(demo %>% 
              dplyr::filter(!(key %in% c("AGE","DDAYS_SINCE_ENC"))), 
            by="ENCOUNTERID") %>%
  group_by(chk_pt,stg_tot_cnt,key,value) %>%
  #HIPAA compliance, low count masking
  dplyr::summarize(enc_cnt = ifelse(n()<11,11,n()),.groups="drop") %>%
  mutate(enc_prop = ifelse(enc_cnt>11,round(enc_cnt/stg_tot_cnt[1],3),11)) %>%
  ungroup %>%
  dplyr::select(-stg_tot_cnt) %>%
  gather(summ,summ_val,-chk_pt,-key,-value) %>%
  bind_rows(aki_stage_ind %>%
              dplyr::filter(!chk_pt %in% c("DISCHARGE")) %>%
              dplyr::select(chk_pt,stg_tot_cnt) %>% 
              unique %>%
              #HIPAA compliance, low count masking
              dplyr::rename(enc_cnt=stg_tot_cnt) %>%
              mutate(enc_cnt=ifelse(enc_cnt<11,11,enc_cnt)) %>%
              mutate(enc_prop=ifelse(enc_cnt>11,round(enc_cnt/enc_tot,3),11),
                     key="TOTAL",
                     value="(%/overall)") %>%
              gather(summ,summ_val,-chk_pt,-key,-value) %>%
              dplyr::select(key,value,chk_pt,summ,summ_val)) %>%
  unite("stg_summ",c("chk_pt","summ")) %>%
  unique %>% spread(stg_summ,summ_val) %>%
  replace(.,is.na(.),0)

#save results
saveRDS(demo,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(demo_summ,file="./data/demo_summ.rda")

#clean up
rm(demo); gc()

####======vital======####
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_vital.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

vital<-execute_single_sql(conn,
                          statement=sql$statement,
                          write=(sql$action=="write")) %>%
  mutate(BMI_GRP = case_when(ORIGINAL_BMI <= 25 ~ "BMI <= 25",
                             ORIGINAL_BMI > 25 &  ORIGINAL_BMI <= 30 ~ "BMI 26-30",
                             ORIGINAL_BMI >=31  ~ "BMI >= 31")) %>%
  dplyr::rename(dsa=DAYS_SINCE_ADMIT,
                timestamp=MEASURE_DATE_TIME) %>%
  gather(key,value,-PATID,-ENCOUNTERID,-dsa,-timestamp) %>%
  dplyr::filter(!is.na(key) & !is.na(value)) %>%
  mutate(key=recode(key,
                    ORIGINAL_BMI="BMI",
                    SYSTOLIC="BP_SYSTOLIC",
                    DIASTOLIC="BP_DIASTOLIC")) %>%
  unique


vital1<-vital %>%
  dplyr::select(ENCOUNTERID, key, value, dsa) %>%
  dplyr::filter(key %in% c("HT","WT","BMI","BP_DIASTOLIC","BP_SYSTOLIC")) %>%
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
  mutate(dsa_grp=case_when(dsa < 0 ~ "0>",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 & dsa < 4 ~ "4",
                           dsa >=4 & dsa < 5 ~ "5",
                           dsa >=5 & dsa < 6 ~ "6",
                           dsa >=6 & dsa < 7 ~ "7",
                           dsa >=7 ~ "7<"))


# collect summaries
vital_summ<-vital1 %>%
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   low_cnt=sum((value<param_low)),
                   high_cnt=sum((value>param_high)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T)),
                   sd=round(sd(value,na.rm=T)),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T),
                   .groups="drop") %>%
  mutate(cov=round(sd/mean,1)) %>%
  #HIPAA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt),
         low_cnt=ifelse(as.numeric(low_cnt)<11,"<11",as.character(low_cnt)),
         high_cnt=ifelse(as.numeric(high_cnt)<11,"<11",as.character(high_cnt))) %>%
  gather(summ,overall,-key) %>%
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
  left_join(
    vital1 %>%
      group_by(key,dsa_grp) %>%
      dplyr::summarize(record_cnt=n(),
                       enc_cnt=length(unique(ENCOUNTERID)),
                       low_cnt=sum((value<param_low)),
                       high_cnt=sum((value>param_high)),
                       min=min(value,na.rm=T),
                       mean=round(mean(value,na.rm=T)),
                       sd=round(sd(value,na.rm=T)),
                       median=round(median(value,na.rm=T)),
                       max=max(value,na.rm=T),
                       .groups="drop") %>%
      mutate(cov=round(sd/mean,1)) %>%
      #HIPAA, low counts masking
      mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
             record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt),
             low_cnt=ifelse(as.numeric(low_cnt)<11,"<11",as.character(low_cnt)),
             high_cnt=ifelse(as.numeric(high_cnt)<11,"<11",as.character(high_cnt))) %>%
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
                         max="5f.max")),
    by=c("key","summ")
  ) %>%
  arrange(key,summ) %>%
  gather(days_from_admit,summ_val,-key,-summ) %>% 
  spread(summ,summ_val)

vital_smoke_summ<-vital %>%
  dplyr::select(PATID,ENCOUNTERID, key, value) %>%
  dplyr::filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
  unique %>%
  group_by(PATID,ENCOUNTERID, key) %>%
  dplyr::mutate(value=paste(value[order(value)],collapse = ",")) %>% 
  ungroup %>% unique %>%
  spread(key,value) %>%
  right_join(Table1 %>% dplyr::select(PATID,ENCOUNTERID),
             by=c("PATID","ENCOUNTERID")) %>%
  replace_na(list(SMOKING="NI",
                  TOBACCO="NI",
                  TOBACCO_TYPE="NI")) %>%
  gather(key,value,-PATID,-ENCOUNTERID) %>%
  mutate(key2=key) %>%
  unite("key_cat",c("key2","value")) %>%
  group_by(key,key_cat) %>%
  dplyr::summarize(pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   enc_prop=length(unique(ENCOUNTERID))/enc_tot) %>%
  arrange(desc(pat_cnt)) %>%
  ungroup %>%
  #HIPAA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",as.character(pat_cnt)),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt))) %>%
  mutate(enc_prop=ifelse(enc_cnt!="<11",paste0(round(enc_prop,3)*100,"%"),"<11")) %>%
  gather(summ,summ_val,-key_cat,-key) %>%
  mutate(summ=recode(summ,
                     pat_cnt="1.patients#",
                     enc_cnt="2.encounters#",
                     enc_prop="3.encounters%")) %>%
  spread(summ,summ_val)

#save
saveRDS(vital,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(vital_summ,file="./data/vital_summ.rda")
# saveRDS(vital_smoke_summ,file="./data/vital_smoke_summ.rda")

#clean up
rm(vital,vital1); gc()

  
####======Labs==========#####
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_lab.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

lab<-execute_single_sql(conn,
                        statement=sql$statement,
                        write=(sql$action=="write")) %>%
  dplyr::rename(key=LAB_LOINC,value=RESULT_NUM,unit=RESULT_UNIT,
                dsa=DAYS_SINCE_ADMIT,timestamp=SPECIMEN_DATE_TIME) %>%
  dplyr::select(ENCOUNTERID,key,value,unit,dsa,timestamp) %>%
  dplyr::filter(!is.na(key) & !is.na(value)) %>%
  unique %>%
  mutate(dsa_grp=case_when(dsa < 0 ~ "0>",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 & dsa < 4 ~ "4",
                           dsa >=4 & dsa < 5 ~ "5",
                           dsa >=5 & dsa < 6 ~ "6",
                           dsa >=6 & dsa < 7 ~ "7",
                           dsa >=7 ~ "7<"))

#collect summaries
lab_summ<-lab %>% 
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T),2),
                   sd=round(sd(value,na.rm=T),3),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T),
                   .groups="drop") %>%
  ungroup %>%
  mutate(cov=round(sd/mean,3)) %>%
  mutate(freq_rk=rank(-enc_cnt,ties.method="first")) %>%
  #HIPAA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt))) %>%
  gather(summ,overall,-key,-freq_rk) %>%
  left_join(
    lab %>%
      group_by(key,dsa_grp) %>%
      dplyr::summarize(record_cnt=n(),
                       enc_cnt=length(unique(ENCOUNTERID)),
                       min=min(value,na.rm=T),
                       mean=round(mean(value,na.rm=T),2),
                       sd=round(sd(value,na.rm=T),3),
                       median=round(median(value,na.rm=T)),
                       max=max(value,na.rm=T),
                       .groups="drop") %>%
      ungroup %>%
      mutate(cov=round(sd/mean,3)) %>%
      #HIPAA, low counts masking
      mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
             record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt)),
             sd=ifelse(is.nan(sd),0,sd)) %>%
      gather(summ,summ_val,-key,-dsa_grp) %>%
      spread(dsa_grp,summ_val),
    by=c("key","summ")
  ) %>%
  arrange(freq_rk,summ) %>%
  #additional 
  mutate(at_admission=ifelse(is.na(`1`),0,1))

#save
saveRDS(lab,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(lab_summ,file="./data/lab_summ.rda")

#clean up
rm(lab); gc()

  
####======Diagnosis=======####
## historical diagnosis
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_dx.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

dx<-execute_single_sql(conn,
                       statement=sql$statement,
                       write=(sql$action=="write")) %>%
  #attach CCS diagnosis grouping
  dplyr::mutate(DX_ICD=paste0("ICD",DX_TYPE,":",DX)) %>%
  left_join(readRDS("./ref/ccs_icd_cw.rda") %>% 
              select(-ccs_name),by=c("DX_ICD"="icd_w_type")) %>%
  unique %>% filter(!is.na(ccs_code)) %>%
  dplyr::rename(key=ccs_code, dsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,dsa) %>%
  unique

#collect summaries
dx_summ<-dx %>%
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min_history=min(dsa,na.rm=T),
                   mean_history=round(mean(dsa,na.rm=T)),
                   sd_history=round(sd(dsa,na.rm=T)),
                   median_history=round(median(dsa,na.rm=T)),
                   max_history=max(dsa,na.rm=T),
                   .groups="drop") %>%
  #HIPAA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",pat_cnt),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt)) %>%
  arrange(key)

#save
saveRDS(dx,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(dx_summ,file="./data/dx_summ.rda")

#clean up
rm(dx); gc()


####======Procedure=========####
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_px.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

px<-execute_single_sql(conn,
                       statement=sql$statement,
                       write=(sql$action=="write")) %>%
  dplyr::mutate(PX=paste0(PX_TYPE,":",PX)) %>%
  dplyr::rename(key=PX,dsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,dsa) %>%
  unique %>%
  mutate(dsa_grp=case_when(dsa < 0 ~ "0>",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 & dsa < 4 ~ "4",
                           dsa >=4 & dsa < 5 ~ "5",
                           dsa >=5 & dsa < 6 ~ "6",
                           dsa >=6 & dsa < 7 ~ "7",
                           dsa >=7 ~ "7<"))

px_summ<-px %>%
  group_by(key,dsa_grp) %>%
  dplyr::summarize(record_cnt=n(),
                   pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   .groups="drop") %>%
  #HIPAA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",pat_cnt),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt)) %>%
  arrange(key,dsa_grp)

#save
saveRDS(px,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(px_summ,file="./data/px_summ.rda")

#clean up
rm(px); gc()


####======Medication=======####
## medication (med_admin)
sql<-parse_sql(paste0("./src/",params$DBMS_type,"/collect_med_admin.sql"),
               cdm_db_schema=config_file$cdm_db_schema)

med<-execute_single_sql(conn,
                        statement=sql$statement,
                        write=(sql$action=="write")) %>%
  dplyr::mutate(RX_EXPOS=round(pmax(as.numeric(difftime(MEDADMIN_START_DATE_TIME,MEDADMIN_STOP_DATE_TIME,units="days")),1))) %>%
  dplyr::rename(sdsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,MEDADMIN_CODE,MEDADMIN_TYPE,MEDADMIN_ROUTE,RX_EXPOS,sdsa) %>%
  mutate(RX_QUANTITY_DAILY=1) %>%
  unite("key",c("MEDADMIN_CODE","MEDADMIN_TYPE","MEDADMIN_ROUTE"),sep=":")


#re-calculate medication exposure
chunk_num<-20
enc_chunk<-med %>% dplyr::select(ENCOUNTERID) %>% unique %>%
  mutate(chunk_id=sample(1:chunk_num,n(),replace=T))

med2<-c()
for(i in 1:chunk_num){
  start_i<-Sys.time()
  
  #--subset ith chunk
  med_sub<-med %>%
    semi_join(enc_chunk %>% filter(chunk_id==i),by="ENCOUNTERID")
  
  #--collect single-day exposure
  med_sub2<-med_sub %>% filter(RX_EXPOS<=1) %>%
    dplyr::mutate(dsa=sdsa,value=RX_QUANTITY_DAILY) %>%
    dplyr::select(ENCOUNTERID,key,value,dsa)
  
  #--for multi-day exposed med, converted to daily exposure
  med_expand<-med_sub[rep(row.names(med_sub),(med_sub$RX_EXPOS+1)),] %>%
    group_by(ENCOUNTERID,key,RX_QUANTITY_DAILY,sdsa) %>%
    dplyr::mutate(expos_daily=1:n()-1) %>%
    dplyr::summarize(dsa=paste0(sdsa+expos_daily,collapse=","),.groups="drop") %>%
    dplyr::rename(value=RX_QUANTITY_DAILY) %>%
    dplyr::select(ENCOUNTERID,key,value,dsa) %>%
    mutate(dsa=strsplit(dsa,",")) %>%
    unnest(dsa) %>%
    mutate(dsa=as.numeric(dsa))
  
  #--merge overlapped precribing intervals (pick the higher exposure)
  med_sub2 %<>% bind_rows(med_expand) %>%
    group_by(ENCOUNTERID,key,dsa) %>%
    dplyr::summarize(value=max(value),.groups="drop")
  
  #--identify non-overlapped exposure episodes and determines the real sdsa
  med_sub2 %<>%
    group_by(ENCOUNTERID,key) %>%
    dplyr::mutate(dsa_lag=lag(dsa,n=1L)) %>%
    ungroup %>%
    mutate(sdsa=ifelse(is.na(dsa_lag)|dsa > dsa_lag+1,dsa,NA)) %>%
    fill(sdsa,.direction="down")
  
  med_sub2 %<>%
    group_by(ENCOUNTERID,key,sdsa) %>%
    dplyr::summarize(RX_EXPOS=pmax(1,sum(value,na.rm=T)),
                     value=paste0(value,collapse=","), #expanded daily exposure
                     dsa=paste0(dsa,collapse=","),
                     .groups="drop")  #expanded dsa for daily exposure
  
  med2 %<>% bind_rows(med_sub2)
}

med<-med2

#collect summaries
med_summ<-med %>% 
  dplyr::select(ENCOUNTERID,key,sdsa,RX_EXPOS) %>%
  mutate(dsa_grp=case_when(sdsa < 0 ~ "0>",
                           sdsa >=0 & sdsa < 1 ~ "1",
                           sdsa >=1 & sdsa < 2 ~ "2",
                           sdsa >=2 & sdsa < 3 ~ "3",
                           sdsa >=3 & sdsa < 4 ~ "4",
                           sdsa >=4 & sdsa < 5 ~ "5",
                           sdsa >=5 & sdsa < 6 ~ "6",
                           sdsa >=6 & sdsa < 7 ~ "7",
                           sdsa >=7 ~ "7<")) %>%
  group_by(key,dsa_grp) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min_expos=min(RX_EXPOS,na.rm=T),
                   mean_expos=round(mean(RX_EXPOS,na.rm=T)),
                   sd_expos=round(sd(RX_EXPOS,na.rm=T)),
                   median_expos=round(median(RX_EXPOS,na.rm=T)),
                   max_expos=max(RX_EXPOS,na.rm=T),
                   .groups="drop") %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",as.character(record_cnt)),
         sd_expos=ifelse(is.na(sd_expos),0,sd_expos)) %>%
  dplyr::mutate(cov_expos=round(sd_expos/mean_expos,1)) %>%
  gather(summ,summ_val,-key,-dsa_grp) %>%
  spread(dsa_grp,summ_val) %>%
  arrange(key,summ)

med_density<-length(unique(med$ENCOUNTERID))

#save
saveRDS(med,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))
# saveRDS(med_summ,file="./data/med_summ.rda")

#clean up
rm(med); gc()


####======results wrap-up==========####
final_out<-list(Table1=consort_tbl,
                Table2=tbl1_summ,
                Table3=demo_nice_tbl,
                Table4=vital_summ,
                Table5=vital_smoke_summ,
                Table6=lab_summ,
                Table7=dx_summ,
                Table8=px_summ,
                Table9=med_summ)
write.xlsx(final_out,file="./output/AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx")


# ggplot sometimes create some unwanted empty .pdf file and want to clean it up
if(file.exists("./Rplots.pdf")){
  file.remove("./Rplots.pdf")
}

rm(list=ls())
gc()

