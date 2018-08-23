####################################################
############ Exploratory Analysis ##################
####################################################

#### set up ####
#clean the slate
rm(list=ls())
gc()

#point to project folder
setwd("~/aki") 

#load utility functions
source("./R/helper_functions.R") 

#install and load required R packages
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr"
                    ))

# load metadata for decoding variable names
meta<-read.csv("./data/AKI_metadata.csv",stringsAsFactors = F)


#### Table1 ####
# read Table 1
# date conversions
tbl1<-read.csv("./data/Table1.csv",header=T) %>%
  dplyr::mutate(ADMIT_DATE=as.Date(as.character(ADMIT_DATE),"%Y-%m-%d"),
                DISCHARGE_DATE=as.Date(as.character(DISCHARGE_DATE),"%Y-%m-%d"),
                AKI1_ONSET=as.Date(as.character(AKI1_ONSET),"%Y-%m-%d"),
                AKI2_ONSET=as.Date(as.character(AKI2_ONSET),"%Y-%m-%d"),
                AKI3_ONSET=as.Date(as.character(AKI3_ONSET),"%Y-%m-%d"))

enc_tot<-length(unique(tbl1$ENCOUNTERID)) # 117453

# get AKI stage index
aki_stage_ind<-tbl1 %>%
  dplyr::select(ENCOUNTERID,
                AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT) %>%
  gather(aki_stg, days_since, -ENCOUNTERID) %>%
  filter(!is.na(days_since)) %>%
  dplyr::select(ENCOUNTERID, aki_stg)%>%
  mutate(aki_stg=gsub("_.*","",aki_stg)) %>%
  group_by(aki_stg) %>%
  dplyr::mutate(stg_tot_cnt=n()) %>%
  ungroup %>%
  arrange(ENCOUNTERID, aki_stg, stg_tot_cnt)

#### AKI_DEMO ####
demo<-read.csv("./data/AKI_DEMO.csv",header=T,stringsAsFactors = F) %>%
  mutate(AGE_GRP=case_when(AGE<= 25 ~ "18-25",
                           AGE >= 26 & AGE <= 35 ~ "26-35",
                           AGE >= 36 & AGE <= 45 ~ "36-45",
                           AGE >= 46 & AGE <= 55 ~ "46-55",
                           AGE >= 56 & AGE <= 65 ~ "56-65",
                           AGE >= 66 ~ "66<="))

# get basic demographic distributions
demo_summ<-aki_stage_ind %>%
  left_join(demo %>%
              dplyr::select(-AGE) %>%
              gather(demo_type,demo_val,-PATID, -ENCOUNTERID), 
            by="ENCOUNTERID") %>%
  group_by(aki_stg,stg_tot_cnt,demo_type,demo_val) %>%
  dplyr::summarize(enc_cnt = n(),
                   enc_prop = round(n()/stg_tot_cnt[1],2)) %>%
  ungroup %>% dplyr::select(-stg_tot_cnt) %>%
  gather(summ,summ_val,-aki_stg,-demo_type,-demo_val) %>%
  # decode demo_val
  left_join(meta %>% dplyr::select(COLUMN_NAME,VAR_CODE,VAR_NAME),
            by=c("demo_type"="COLUMN_NAME","demo_val"="VAR_CODE")) %>%
  dplyr::mutate(demo_val = ifelse(!is.na(VAR_NAME),VAR_NAME,demo_val)) %>%
  dplyr::select(-VAR_NAME) %>%
  unite("demo_type_cat",c("demo_type","demo_val")) %>%
  # attach totals at bottom
  bind_rows(aki_stage_ind %>%
              dplyr::select(aki_stg,stg_tot_cnt) %>% 
              unique %>% 
              dplyr::rename(enc_cnt=stg_tot_cnt) %>%
              mutate(enc_prop=round(enc_cnt/117453,2),
                     demo_type_cat="TOTAL(%/overall)") %>%
              gather(summ,summ_val,-aki_stg,-demo_type_cat) %>%
              dplyr::select(aki_stg,demo_type_cat,summ,summ_val)) %>%
  unite("stg_summ",c("aki_stg","summ")) %>%
  unique %>% spread(stg_summ,summ_val) %>%
  replace(.,is.na(.),0)


#### AKI_VITAL ####
vital<-read.csv("./data/AKI_VITAL.csv",stringsAsFactors = F)


#### AKI_ENC ####
enc<-read.csv("./data/AKI_ENC.csv",stringsAsFactors = F)


#### AKI_MED ####
med<-read.csv("./data/AKI_MED.csv",stringsAsFactors = F)


#### AKI_LAB ####
lab<-read.csv("./data/AKI_LAB.csv",stringsAsFactors = F)


#### AKI_DX ####
dx<-read.csv("./data/AKI_DX.csv",stringsAsFactors = F)


#### AKI_PX ####
dx<-read.csv("./data/AKI_PX.csv",stringsAsFactors = F)


