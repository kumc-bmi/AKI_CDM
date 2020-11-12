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

#save results
saveRDS(Table1,file="./data/raw/Table1.rda")
saveRDS(consort_tbl,file="./data/raw/consort_tbl.rda")

#clean up
rm(cohort,Table1,consort_tbl); gc()


####======collect data   --Demographic========####
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

#save results
saveRDS(demo,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))

#clean up
rm(demo); gc()

####======collect data   --Vital========####
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


#save
saveRDS(vital,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))

#clean up
rm(vital,vital1); gc()

  
####======collect data   --Lab==========#####
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

  
####======collect data   --Diagnosis=======####
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

#save
saveRDS(dx,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))

#clean up
rm(dx); gc()


####======collect data   --Procedures=========####
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

#save
saveRDS(px,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))

#clean up
rm(px); gc()


####======collect data   --Medication=======####
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

#save
saveRDS(med,file=paste0("./data/raw/",toupper(sql$tbl_out),".rda"))

#clean up
rm(med); gc()



####======collect metadata ======
metadata<-data.frame(var=unique(readRDS("./data/raw/AKI_DEMO.rda")$key),
                     type="demo",stringsAsFactors = F) %>%
  bind_rows(data.frame(var=unique(readRDS("./data/raw/AKI_VITAL.rda")$key),
                       type="vital",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=paste0(rep(c("SYSTOPIC","DIASTOLIC"),2),rep(c("_min","_slope"),each=2)),
                       type="vital",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=unique(readRDS("./data/raw/AKI_LAB.rda")$key),
                       type="lab",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=paste0(unique(readRDS("./data/raw/AKI_LAB.rda")$key),"_change"),
                       type="lab",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var="BUN_SCR",
                       type="lab",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=as.character(unique(readRDS("./data/raw/AKI_DX.rda")$key)),
                       type="dx",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=unique(readRDS("./data/raw/AKI_PX.rda")$key),
                       type="px",stringsAsFactors = F)) %>%
  bind_rows(data.frame(var=unique(readRDS("./data/raw/AKI_MED.rda")$key),
                       type="med",stringsAsFactors = F))
