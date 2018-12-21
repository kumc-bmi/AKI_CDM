#### test: vital collections ####
source("./R/util.R")
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))
params<-list(  DBMS_type="Oracle",
               driver_type="OCI",
               remote_CDM=FALSE)


config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,params$driver_type,config_file)
DBMS_type<-attr(conn,"DBMS_type")
driver_type<-attr(conn,"driver_type")


#set up parameters
remote_CDM=params$remote_CDM
cdm_db_link=config_file$cdm_db_link
cdm_db_name=config_file$cdm_db_name
cdm_db_schema=config_file$cdm_db_schema
start_date="2010-01-01"
end_date="2018-12-31"
verb=F

# auxilliary summaries and tables
Table1<-readRDS("./data_local/data_raw/Table1.rda")
enc_tot<-length(unique(Table1$ENCOUNTERID))

#statements to be tested
sql<-parse_sql(paste0("./inst/",params$DBMS_type,"/collect_vital.sql"),
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema)

#collect vital
vital<-execute_single_sql(conn,
                          statement=sql$statement,
                          write=(sql$action=="write")) %>%
  mutate(BMI_GRP = case_when(ORIGINAL_BMI <= 25 ~ "BMI <= 25",
                             ORIGINAL_BMI > 25 &  ORIGINAL_BMI <= 30 ~ "BMI 26-30",
                             ORIGINAL_BMI >=31  ~ "BMI >= 31")) %>%
  dplyr::rename(dsa=DAYS_SINCE_ADMIT,
                timestamp=MEASURE_DATE_TIME) %>%
  gather(key,value,-PATID,-ENCOUNTERID,-dsa,-timestamp) %>%
  filter(!is.na(key) & !is.na(value)) %>%
  mutate(key=recode(key,
                    ORIGINAL_BMI="BMI",
                    SYSTOLIC="BP_SYSTOLIC",
                    DIASTOLIC="BP_DIASTOLIC")) %>%
  unique

vital<-readRDS("./data_local/data_raw/AKI_VITAL.rda")


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
#passed!


#collect summaries
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
                       max=max(value,na.rm=T)) %>%
      ungroup %>%
      mutate(cov=round(sd/mean,1)) %>%
      #HIPPA, low counts masking
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



