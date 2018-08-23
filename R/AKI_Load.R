############ Preprocessing ##################
rm(list=ls())
gc()

setwd("~/aki")

source("helper_functions.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "xgboost",
                    "ROracle",
                    "DBI"))

## Connect to Oracle database
#set up connection with Oracle db
# heronb2_config<-read.csv('../heronb2_config.csv')
# c_connect<-dbConnect(Oracle(),heronb2_config$username,heronb2_config$password,heronb2_config$access)

herona1_config<-read.csv('../herona1_config.csv')
c_connect<-dbConnect(Oracle(),herona1_config$username,herona1_config$password,herona1_config$access)

#### Table1 ####
Table1<-dbGetQuery(c_connect,"select * from AKI_onsets")
write.csv(Table1,file="./data/Table1.csv",row.names=F)
rm(Table1); gc()


#### AKI_DEMO ####
AKI_DEMO<-dbGetQuery(c_connect,"select * from AKI_DEMO")
write.csv(AKI_DEMO,file="./data/AKI_DEMO.csv",row.names=F)


#### AKI_VITAL ####
AKI_VITAL<-dbGetQuery(c_connect,"select * from AKI_VITAL")

#detect outliers - HT, WT, BMI
ht_wt_bmi<-AKI_VITAL %>% 
  dplyr::select(PATID, VITALID, MEASURE_DATE_TIME,
                HT,WT,ORIGINAL_BMI) %>%
  dplyr::filter(!is.na(HT) & !is.na(WT)) %>%
  left_join(AKI_DEMO %>% dplyr::select(PATID,AGE,SEX) %>% unique,
            by="PATID") %>%
  dplyr::mutate(age_grp= case_when(AGE < 30 ~ 1,
                                   AGE >= 30 & AGE < 40 ~ 2,
                                   AGE >= 40 & AGE < 50 ~ 3,
                                   AGE >= 50 & AGE < 60 ~ 4,
                                   AGE >= 60 & AGE < 70 ~ 5,
                                   AGE >= 70 ~ 6)) %>%
  dplyr::select(-AGE) %>%
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
  
  
vital_out<-ht_wt_bmi %>% filter(outlier_ind==1) %>% 
  dplyr::select(VITALID) %>% unique
  # bind_rows(bp %>% filter(outlier_ind==1) %>% 
  #             dplyr::select(VITALID) %>% unique)

before<-length(unique(AKI_VITAL$VITALID))
AKI_VITAL %<>%
  anti_join(vital_out,by="VITALID")
after<-length(unique(AKI_VITAL$VITALID))
before-after

write.csv(AKI_VITAL,file="./data/AKI_VITAL.csv",row.names = F)
rm(ht_wt_bmi,bp,AKI_VITAL,AKI_DEMO); gc()

#### AKI_ENC ####
AKI_ENC<-dbGetQuery(c_connect,"select * from AKI_ENC")
write.csv(AKI_ENC,file="./data/AKI_ENC.csv",row.names=F)
rm(AKI_ENC); gc()

#### AKI_MED ####
AKI_MED<-dbGetQuery(c_connect,"select * from AKI_MED")
write.csv(AKI_MED,file="./data/AKI_MED.csv",row.names=F)
rm(AKI_MED); gc()

#### AKI_LAB ####
AKI_LAB<-dbGetQuery(c_connect,"select * from AKI_LAB") %>%
  dplyr::select(-LAB_PX,-LAB_PX_TYPE,-RESULT_QUAL)
write.csv(AKI_LAB,file="./data/AKI_LAB.csv",row.names=F)
rm(AKI_LAB); gc()

#### AKI_DX ####
load("./data/ccs_icd_cw.Rdata")

#format icd codes
ccs_icd %<>% 
  dplyr::mutate(icd_dot=ifelse(grepl("^E",icd) & icd_type=="9",
                               paste0(substr(icd,1,4),".",substring(icd,5)),
                               paste0(substr(icd,1,3),".",substring(icd,4)))) %>%
  dplyr::mutate(icd_w_type=paste0("ICD",str_pad(icd_type,2,"left","0"),":",icd_dot))

AKI_DX<-dbGetQuery(c_connect,"select * from AKI_DX") %>%
  dplyr::mutate(icd_w_type=paste0("ICD",DX_TYPE,":",DX)) %>%
  left_join(ccs_icd %>% filter(type=="dx") %>% 
              dplyr::select(icd_w_type,ccs_code),
            by="icd_w_type") %>%
  dplyr::rename(CCS_CODE = ccs_code) %>% 
  dplyr::select(-icd_w_type,-ENCOUNTERID) %>%
  unique %>% filter(!is.na(CCS_CODE))

write.csv(AKI_DX,file="./data/AKI_DX.csv",row.names=F)
rm(AKI_DX); gc()


#### AKI_PX ####
AKI_PX<-dbGetQuery(c_connect,"select * from AKI_PX")
write.csv(AKI_PX,file="./data/AKI_PX.csv",row.names=F)
rm(AKI_PX); gc()


#### AKI_metadata ####
AKI_metadata<-dbGetQuery(c_connect,"select * from AKI_metadata") %>%
  bind_rows(ccs_icd %>% dplyr::select(ccs_code, ccs_name, type) %>%
              unique %>%
              dplyr::mutate(COLUMN_NAME="CCS_CODE",        
                            TABLE_NAME=ifelse(type=="dx","AKI_DX","AKI_PX"),
                            VAR_CODE = as.character(ccs_code)) %>%
              dplyr::rename(VAR_NAME = ccs_name) %>%
              dplyr::select(COLUMN_NAME, TABLE_NAME, VAR_CODE, VAR_NAME))
write.csv(AKI_metadata,file="./data/AKI_metadata.csv",row.names=F)
rm(AKI_metadata); gc()

