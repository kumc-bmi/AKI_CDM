rm(list=ls())
gc()

#holders for final results
final_out<-list()
tbl_cnt<-1

############################# set up #############################
#source utility functions
source("./R/util.R")
source("./R/extract_cohort.R")

#load libraries
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "ROracle",
                    "DBI",
                    "openxlsx"))


#establish the connection between r-studio and a writable schema where
# - intermediate tables can be created and
# - can talk to CDM server
config_file_path<-"../config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-dbConnect(ROracle::Oracle(),
                config_file$username,
                config_file$password,
                config_file$access)


############################# extract cohort --Table1 #############################
# by default, we assume cdm schema is on the same server as current schema,
# if not, set same_server=F and cdm_db_server=...(server name)
cohort<-extract_cohort(conn,
                       oracle_temp_schema=config_file$oracle_temp_schema,
                       cdm_db_schema=config_file$cdm_db_schema,
                       start_date="2010-01-01",
                       end_date="2018-12-31")
Table1<-cohort$aki_enc
consort_tbl<-cohort$attrition

#---------------add to result list----------------#
final_out[[paste0("Table",tbl_cnt)]]<-consort_tbl
tbl_cnt<-tbl_cnt+1
#---------------add to result list----------------#


############################# summarize Table1 ##################################
# collect summaries
enc_tot<-length(unique(Table1$ENCOUNTERID))

tbl1_dsa<-Table1 %>% 
  dplyr::select(PATID, ENCOUNTERID,
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
                   sd_time=round(sd(days_since_admit,na.rm=T),2)) %>%
  mutate(semi_IQR_time=0.5*(q3_time-q1_time)) %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",as.character(pat_cnt)),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt)))

#---------------add to result list----------------#
final_out[[paste0("Table",tbl_cnt)]]<-tbl1_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list----------------#

# tbl1_summ2<-tbl1_dsa %>%
#   mutate(dsa_bin=case_when(days_since_admit <10 ~ paste0("0",days_since_admit," days"),
#                            days_since_admit >=10 & days_since_admit < 31 ~ paste(days_since_admit,"days"),
#                            days_since_admit >=31 ~ '31 days(1mth) <')) %>%
#   group_by(stage,dsa_bin) %>%
#   dplyr::summarize(enc_cnt=length(unique(ENCOUNTERID))) %>%
#   spread(stage,enc_cnt,fill=0) %>%
#   mutate(AKI1_cum=cumsum(AKI1),
#          AKI2_cum=cumsum(AKI2),
#          AKI3_cum=cumsum(AKI3),
#          NONAKI_cum=cumsum(NONAKI)) %>%
#   arrange(desc(dsa_bin)) %>%
#   mutate(NONAKI=cumsum(NONAKI)) %>%
#   arrange(dsa_bin)
# 
# #---------------add to result list----------------#
# final_out[[paste0("Table",tbl_cnt)]]<-tbl1_summ2
# tbl_cnt<-tbl_cnt+1
# #---------------add to result list----------------#

#save on disk?
# save(Table1,file="./data/Table1.Rdata")
# save(consort_tbl,file="./data/consort_tbl.Rdata")
# save(tbl1_summ,file="./data/tbl1_summ.Rdata")
# save(tbl1_summ2,file="./data/tbl1_summ2.Rdata")
#clean up
rm(cohort,consort_tbl,tbl1_summ); gc()

#collect and summarize variables
# auxilliary summaries and tables
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


############################# demographic #######################################
demo<-dbGetQuery(conn,
                 parse_sql("./inst/collect_demo.sql",
                           cdm_db_schema=config_file$cdm_db_schema,
                           cdm_db_server=" ")$statement) %>%
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

#collect summaries
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

#---------------add to result list----------------#
final_out[[paste0("Table",tbl_cnt)]]<-demo_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list----------------#

#save results
# save(demo,file="./data/AKI_demo.Rdata")
# save(demo_summ,file="./data/demo_summ.Rdata")
#clean up
rm(demo,demo_summ); gc()

############################# vital ################################################
vital<-dbGetQuery(conn,
                  parse_sql("./inst/collect_vital.sql",
                            cdm_db_schema=config_file$cdm_db_schema,
                            cdm_db_server=" ")$statement) %>%
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

vital1<-vital %>%
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
                   max=max(value,na.rm=T)) %>%
  ungroup %>%
  mutate(cov=round(sd/mean,1)) %>%
  #HIPPA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",record_cnt),
         low_cnt=ifelse(as.numeric(low_cnt)<11 & as.numeric(low_cnt)>0,"<11",as.character(low_cnt)),
         high_cnt=ifelse(as.numeric(high_cnt)<11 & as.numeric(high_cnt)>0,"<11",as.character(high_cnt))) %>%
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
                       max=max(value,na.rm=T)) %>%
      ungroup %>%
      mutate(cov=round(sd/mean,1)) %>%
      #HIPPA, low counts masking
      mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",enc_cnt),
             record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",record_cnt),
             low_cnt=ifelse(as.numeric(low_cnt)<11 & as.numeric(low_cnt)>0,"<11",as.character(low_cnt)),
             high_cnt=ifelse(as.numeric(high_cnt)<11 & as.numeric(high_cnt)>0,"<11",as.character(high_cnt))) %>%
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
  arrange(key,summ)


#---------------add to result list----------------#
final_out[[paste0("Table",tbl_cnt)]]<-vital_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list----------------#


vital_smoke_summ<-vital %>%
  dplyr::select(PATID,ENCOUNTERID, key, value) %>%
  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
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
  unite("key_cat",c("key","value")) %>%
  group_by(key_cat) %>%
  dplyr::summarize(pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   record_cnt=n()) %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11 & as.numeric(pat_cnt)>0,"<11",as.character(pat_cnt)),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt))) %>%
  gather(summ,summ_val,-key_cat) %>%
  mutate(summ=recode(summ,
                     pat_cnt="1.patients#",
                     enc_cnt="2.encounters#",
                     record_cnt="3.records#")) %>%
  spread(summ,summ_val)

#---------------add to result list-------------------#
final_out[[paste0("Table",tbl_cnt)]]<-vital_smoke_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list-------------------#

#save
# save(vital,file="./data/AKI_vital.Rdata")
# save(vital_summ,file="./data/vital_summ.Rdata")
# save(vital_smoke_summ,file="./data/vital_smoke_summ.Rdata")
#clean up
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


############################# labs ###########################################
lab<-dbGetQuery(conn,
                parse_sql("./inst/collect_lab.sql",
                          cdm_db_schema=config_file$cdm_db_schema,
                          cdm_db_server=" ")$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,LAB_LOINC,RESULT_NUM,RESULT_UNIT,SPECIMEN_DATE_TIME) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(dsa=round(as.numeric(difftime(SPECIMEN_DATE_TIME,critical_date,units="days")),2)) %>%
  dplyr::rename(key=LAB_LOINC,value=RESULT_NUM,unit=RESULT_UNIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,unit,dsa) %>%
  filter(!is.na(key) & !is.na(value)) %>%
  unique

#collect summaries
lab %<>%
  mutate(dsa_grp=case_when(dsa < 0 ~ "0>",
                           dsa >=0 & dsa < 1 ~ "1",
                           dsa >=1 & dsa < 2 ~ "2",
                           dsa >=2 & dsa < 3 ~ "3",
                           dsa >=3 & dsa < 4 ~ "4",
                           dsa >=4 & dsa < 5 ~ "5",
                           dsa >=5 & dsa < 6 ~ "6",
                           dsa >=6 & dsa < 7 ~ "7",
                           dsa >=7 ~ "7<"))

lab_summ<-lab %>% 
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T)),
                   sd=round(sd(value,na.rm=T)),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T)) %>%
  ungroup %>%
  mutate(cov=round(sd/(mean+1e-2),1)) %>%
  mutate(freq_rk=rank(-enc_cnt,ties.method="first")) %>%
  #HIPPA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt))) %>%
  gather(summ,overall,-key,-freq_rk) %>%
  mutate(summ=recode(summ,
                     enc_cnt="1.encounters#",
                     record_cnt="2.records#",
                     min="3a.min",
                     median="3b.median",
                     mean="3c.mean",
                     sd="3d.sd",
                     cov="3e.cov",
                     max="3f.max")) %>%
  left_join(
    lab %>%
      group_by(key,dsa_grp) %>%
      dplyr::summarize(record_cnt=n(),
                       enc_cnt=length(unique(ENCOUNTERID)),
                       min=min(value,na.rm=T),
                       mean=round(mean(value,na.rm=T)),
                       sd=round(sd(value,na.rm=T)),
                       median=round(median(value,na.rm=T)),
                       max=max(value,na.rm=T)) %>%
      ungroup %>%
      mutate(cov=round(sd/(mean+1e-2),2)) %>%
      #HIPPA, low counts masking
      mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
             record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt)),
             sd=ifelse(is.nan(sd),0,sd)) %>%
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
                         max="3f.max")),
    by=c("key","summ")
  ) %>%
  arrange(freq_rk,summ) %>%
  #additional 
  mutate(at_admission=ifelse(is.na(`1`),0,1),
         within_3d=ifelse(is.na(coalesce(`1`,`2`,`3`)),0,1))

#---------------add to result list-------------------#
final_out[[paste0("Table",tbl_cnt)]]<-lab_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list-------------------#

#save
# save(lab,file="./data/AKI_lab.Rdata")
# save(lab_summ,file="./data/lab_summ.Rdata")

#clean up
rm(lab,lab_summ); gc()


############################# admission DRG ####################################
uhc_DRG<-dbGetQuery(conn,
                parse_sql("./inst/collect_DRG.sql",
                          cdm_db_schema=config_file$cdm_db_schema,
                          cdm_db_server=" ")$statement) %>%
  dplyr::select(PATID,ENCOUNTERID,DRG_TYPE,DRG,DRG_DATE) %>%
  filter(!is.na(DRG)) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(dsa=round(as.numeric(difftime(DRG_DATE,critical_date,units="days")))) %>%
  dplyr::rename(key1=DRG_TYPE,key2=DRG) %>% 
  dplyr::select(PATID,ENCOUNTERID,key1,key2,dsa) %>%
  unique

#collect summaries
DRG_summ<-uhc_DRG %>%
  group_by(key1,key2) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   pat_cnt=length(unique(PATID)),
                   min_history=min(dsa,na.rm=T),
                   mean_history=round(mean(dsa,na.rm=T)),
                   sd_history=round(sd(dsa,na.rm=T)),
                   median_history=round(median(dsa,na.rm=T)),
                   max_history=max(dsa,na.rm=T)) %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",as.character(pat_cnt)),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",as.character(record_cnt))) %>%
  gather(summ,summ_val,-key1,-key2) %>%
  dplyr::mutate(summ=recode(summ,
                            pat_cnt="1.patients#",
                            enc_cnt="2.encounters#",
                            record_cnt="3.records#",
                            max_history="4a.min_history",
                            median_history="4b.median_history",
                            mean_history="4c.mean_history",
                            sd_history="4d.sd_history",
                            min_history="4f.max_history")) %>%
  unite("DRG",c("key2","key2"),sep="_") %>%
  spread(summ,summ_val) %>%
  arrange(DRG)

#---------------add to result list-------------------#
final_out[[paste0("Table",tbl_cnt)]]<-DRG_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list-------------------#

#save
# save(uhc_DRG,file="./data/AKI_DRG.Rdata")
# save(DRG_summ,file="./data/DRG_summ.Rdata")

#clean up
rm(uhc_DRG,DRG_summ); gc()


############################# diagnosis (CCS) #######################################################
load("./data/ccs_icd_cw.Rdata")
dx<-dbGetQuery(conn,
               parse_sql("./inst/collect_dx.sql",
                         cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  #attach CCS diagnosis grouping
  dplyr::mutate(DX_ICD=paste0("ICD",DX_TYPE,":",DX)) %>%
  left_join(ccs_icd %>% select(-ccs_name),by=c("DX_ICD"="icd_w_type")) %>%
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
                   max_history=max(dsa,na.rm=T)) %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",pat_cnt),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt)) %>%
  gather(summ,summ_val,-key) %>%
  dplyr::mutate(summ=recode(summ,
                            pat_cnt="1.patient#",
                            enc_cnt="2.encounter#",
                            record_cnt="3.records#",
                            max_history="4a.min_history",
                            median_history="4b.median_history",
                            mean_history="4c.mean_history",
                            sd_history="4d.sd_history",
                            min_history="4f.max_history")) %>%
  spread(summ,summ_val) %>%
  arrange(key)

#---------------add to result list-------------------#
final_out[[paste0("Table",tbl_cnt)]]<-dx_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list-------------------#

#save
# save(dx,file="./data/AKI_dx.Rdata")  
# save(dx_summ,file="./data/dx_summ.Rdata")

#clean up
rm(dx,dx_summ); gc()


############################# procedure ####################################
px<-dbGetQuery(conn,
               parse_sql("./inst/collect_px.sql",
                         cdm_db_schema=config_file$cdm_db_schema,
                         cdm_db_server=" ")$statement) %>%
  dplyr::mutate(PX=paste0(PX_TYPE,":",PX)) %>%
  dplyr::select(PATID,ENCOUNTERID,PX,DAYS_SINCE_ADMIT) %>%
  dplyr::rename(key=PX, dsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,dsa) %>%
  unique

px_summ<-px %>%
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min_history=min(dsa,na.rm=T),
                   mean_history=round(mean(dsa,na.rm=T)),
                   sd_history=round(sd(dsa,na.rm=T)),
                   median_history=round(median(dsa,na.rm=T)),
                   max_history=max(dsa,na.rm=T)) %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",pat_cnt),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt),
         sd_history=ifelse(is.na(sd_history),0,sd_history)) %>%
  gather(summ,summ_val,-key) %>%
  dplyr::mutate(summ=recode(summ,
                            pat_cnt="1.patient#",
                            enc_cnt="2.encounter#",
                            record_cnt="3.records#",
                            max_history="4a.min_history",
                            median_history="4b.median_history",
                            mean_history="4c.mean_history",
                            sd_history="4d.sd_history",
                            min_history="4f.max_history")) %>%
  spread(summ,summ_val) %>%
  arrange(key)

#---------------add to result list-------------------#
final_out[[paste0("Table",tbl_cnt)]]<-px_summ
tbl_cnt<-tbl_cnt+1
#---------------add to result list-------------------#

#save
# save(px,file="./data/AKI_px.Rdata")  
# save(px_summ,file="./data/px_summ.Rdata")

#clean up
rm(px,px_summ); gc()


############################# medication ####################################
med<-dbGetQuery(conn,
                parse_sql("./inst/collect_med.sql",
                          cdm_db_schema=config_file$cdm_db_schema,
                          cdm_db_server=" ")$statement) %>%
  dplyr::mutate(RX_EXPOS=round(pmin(pmax(as.numeric(difftime(RX_END_DATE,RX_START_DATE,units="days")),1),
                                    pmax(RX_DAYS_SUPPLY,1),na.rm=T))) %>%
  replace_na(list(RX_QUANTITY_DAILY=1)) %>%
  group_by(PATID,ENCOUNTERID,RXNORM_CUI,RX_BASIS) %>%
  dplyr::summarize(RX_START_DATE=min(RX_START_DATE),
                   RX_END_DATE=max(RX_END_DATE),
                   RX_QUANTITY_DAILY=max(RX_QUANTITY_DAILY,na.rm=T),
                   RX_EXPOS=max(RX_EXPOS,na.rm=T)) %>%
  ungroup %>%
  dplyr::mutate(RX_EXPOS=pmax(as.numeric(difftime(RX_END_DATE,RX_START_DATE,units="days")),
                              RX_EXPOS,na.rm=T)) %>%
  left_join(aki_stage_ind %>% filter(chk_pt=="ADMIT"),
            by=c("PATID","ENCOUNTERID")) %>%
  dplyr::mutate(sdsa=round(as.numeric(difftime(RX_START_DATE,critical_date,units="days")))) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_BASIS,RX_EXPOS,RX_QUANTITY_DAILY,sdsa) %>%
  unite("key",c("RXNORM_CUI","RX_BASIS"),sep=":")

#estimate daily exposure
batch<-20
expos_quant<-c(1,unique(quantile(med[med$RX_EXPOS>1,]$RX_EXPOS,probs=0:batch/batch)))
med2<-med %>% filter(RX_EXPOS<=1) %>% 
  dplyr::mutate(dsa=as.character(sdsa),edsa=sdsa,value=RX_QUANTITY_DAILY) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,sdsa,edsa,dsa)

for(i in seq_len(length(expos_quant)-1)){
  start_i<-Sys.time()
  
  med_sub<-med %>% filter(RX_EXPOS > expos_quant[i] & RX_EXPOS <= expos_quant[i+1])
  med_expand<-med_sub[rep(row.names(med_sub),(med_sub$RX_EXPOS+1)),] %>%
    group_by(PATID,ENCOUNTERID,key,RX_QUANTITY_DAILY,sdsa) %>%
    dplyr::mutate(expos_daily=1:n()-1) %>% 
    dplyr::summarize(edsa=max(sdsa+expos_daily),
                     dsa=paste0(sdsa+expos_daily,collapse=",")) %>%
    ungroup %>% dplyr::rename(value=RX_QUANTITY_DAILY) %>%
    dplyr::select(PATID,ENCOUNTERID,key,value,sdsa,edsa,dsa)
  med2 %<>% bind_rows(med_expand)

  lapse_i<-Sys.time()-start_i
  cat("batch",i,"of exposures between",expos_quant[i],"and",expos_quant[i+1],
      "days are collected in",lapse_i,units(lapse_i),".\n")
  
  gc()
}

#collect summaries
med_summ<-med %>% 
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
                   max_expos=max(RX_EXPOS,na.rm=T)) %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",as.character(enc_cnt)),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",as.character(record_cnt)),
         sd_expos=ifelse(is.na(sd_expos),0,sd_expos)) %>%
  dplyr::mutate(cov_expos=round(sd_expos/mean_expos,1)) %>%
  gather(summ,summ_val,-key,-dsa_grp) %>%
  spread(dsa_grp,summ_val) %>%
  dplyr::mutate(summ=recode(summ,
                            enc_cnt="1.encounters#",
                            record_cnt="2.records#",
                            min_expos="3a.min_expos",
                            median_expos="3b.median_expos",
                            mean_expos="3c.mean_expos",
                            sd_expos="3d.sd_expos",
                            cov_expos="3e.cov_expos",
                            max_expos="3f.max_expos")) %>%
  arrange(key,summ) 

#---------------add to result list-------------------#
#last table
final_out[[paste0("Table",tbl_cnt)]]<-med_summ
#---------------add to result list-------------------#

#save
# save(med2,file="./data/AKI_med.Rdata")
# save(med_summ,file="./data/med_summ.Rdata")

#clean up
rm(med,med2,med_summ); gc()

############################# write final workbook ##########################
write.xlsx(final_out,file="./output/AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx")
#Note: if .xlsx cannot opened directly, try open it by read.xlsx in R
