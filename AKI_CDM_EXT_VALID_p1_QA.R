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
                       cdm_db_schema,
                       oracle_temp_schema=cdm_db_schema,
                       start_date="2010-01-01",
                       end_date="2018-12-31")

#save Table1
Table1<-cohort$aki_enc
save(Table1,file="./data/Table1.Rdata")

#print out attrition table
print(cohort$attrition)
##pass tests!



#collect variables
load("./data/Table1.Rdata")
Table1 %<>%
  dplyr::mutate(ADMIT_DATE=as.Date(as.character(ADMIT_DATE),"%Y-%m-%d"),
                DISCHARGE_DATE=as.Date(as.character(DISCHARGE_DATE),"%Y-%m-%d"),
                AKI1_ONSET=as.Date(as.character(AKI1_ONSET),"%Y-%m-%d"),
                AKI2_ONSET=as.Date(as.character(AKI2_ONSET),"%Y-%m-%d"),
                AKI3_ONSET=as.Date(as.character(AKI3_ONSET),"%Y-%m-%d"))

enc_tot<-length(unique(Table1$ENCOUNTERID))


#demographic
demo<-dbGetQuery(conn,
                 parse_sql("./inst/collect_demo.sql",
                           cdm_db_schema=cdm_db_schema)$statement) %>%
  mutate(AGE_GRP=case_when(AGE<= 25 ~ "18-25",
                           AGE >= 26 & AGE <= 35 ~ "26-35",
                           AGE >= 36 & AGE <= 45 ~ "36-45",
                           AGE >= 46 & AGE <= 55 ~ "46-55",
                           AGE >= 56 & AGE <= 65 ~ "56-65",
                           AGE >= 66 ~ "66<="))

#save
save(demo,file="./data/AKI_demo.Rdata")

#illogical dates (aki onsets before birth or after death)
aki_bad_dates<-aki_stage_ind<-tbl1 %>%
  dplyr::select(ENCOUNTERID,
                AKI1_ONSET,AKI2_ONSET,AKI3_ONSET) %>%
  gather(aki_stg, onset, -ENCOUNTERID) %>%
  filter(!is.na(onset)) %>%
  dplyr::select(ENCOUNTERID, aki_stg)%>%
  mutate(aki_stg=gsub("_.*","",aki_stg)) %>%
  



#summaries
demo_summ<-aki_stage_ind<-tbl1 %>%
  dplyr::select(ENCOUNTERID,
                AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT) %>%
  gather(aki_stg, days_since, -ENCOUNTERID) %>%
  filter(!is.na(days_since)) %>%
  dplyr::select(ENCOUNTERID, aki_stg)%>%
  mutate(aki_stg=gsub("_.*","",aki_stg)) %>%
  group_by(aki_stg) %>%
  dplyr::mutate(stg_tot_cnt=n()) %>%
  ungroup %>%
  arrange(ENCOUNTERID, aki_stg, stg_tot_cnt) %>%
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





