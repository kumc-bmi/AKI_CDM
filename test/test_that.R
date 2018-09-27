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


#test format_data()
#demo
rm(list=ls());gc()
type<-"demo"
dat<-readRDS("./data/AKI_demo.rda")
dat_out<-dat%>% dplyr::select(-PATID) %>%
  filter(key %in% c("AGE","SEX","RACE","HISPANIC")) %>%
  group_by(ENCOUNTERID,key) %>%
  top_n(n=1L,wt=value) %>% #randomly pick one if multiple entries exist
  ungroup

#vital
rm(list=ls());gc()
type<-"vital"
dat<-readRDS("./data/AKI_vital.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
  group_by(ENCOUNTERID,key) %>%
  arrange(value) %>% slice(1:1) %>%
  ungroup

dat_out<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("HT","WT","BMI")) %>%
  group_by(ENCOUNTERID,key) %>%
  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
  ungroup

bp<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("BP_DIASTOLIC","BP_SYSTOLIC")) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(value=ifelse((key=="BP_DIASTOLIC" & (value>120 | value<40))|
                        (key=="BP_SYSTOLIC" & (value>210 | value<40)),NA,value),
         dsa_int=round(dsa)) %>%
  group_by(ENCOUNTERID,key,dsa_int) %>%
  dplyr::mutate(value_imp=median(value,na.rm=T)) %>%
  ungroup 

bp %>% group_by(key) %>% 
  dplyr::summarize(rec_cnt=n(),na_cnt=sum((is.na(value)))) %>%
  ungroup %>% View

bp %<>%
  filter(!is.na(value_imp)) %>%
  mutate(value=ifelse(is.na(value),value_imp,value)) %>%
  dplyr::select(-value_imp)

bp_min<-bp %>%
  group_by(ENCOUNTERID,key,dsa_int) %>%
  dplyr::summarize(value_lowest=min(value,na.rm=T)) %>%
  ungroup %>%
  mutate(key=paste0(key,"_min"))

bp_slp_obj<-bp %>%
  group_by(ENCOUNTERID,key,dsa_int) %>%
  mutate(dsa=1:n()) %>%
  do(fit_val=lm(value ~ dsa,data=.))

bp_slp<-tidy(bp_slp_obj,fit_val) %>%
  filter(term=="dsa") %>%
  dplyr::rename(value=estimate) %>%
  mutate(key=paste0(key,"_slope"))





#
source("./R/extract_cohort.R")
source("./R/viz.R")
params<-list(  DBMS_type="Oracle",
               remote_CDM=FALSE)

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,config_file)
DBMS_type<-attr(conn,"DBMS_type")


#### test sql parsor ####
sql<-parse_sql(file_path="./inst/Oracle/cohort_initial.sql",
               remote_CDM=F,
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")

sql<-parse_sql(file_path="./inst/tSQL/cohort_initial.sql",
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")

sql<-parse_sql(file_path="./inst/Oracle/collect_demo.sql",
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema)

sql<-parse_sql(file_path="./inst/Oracle/collect_demo.sql",
               cdm_db_link="remote_link",
               cdm_db_name=NULL,
               cdm_db_schema=cdm_db_schema)

sql<-parse_sql(file_path="./inst/tSQL/collect_demo.sql",
               cdm_db_name=db_params$cdm_db_name,
               cdm_db_schema=db_params$cdm_db_schema)


#### test extract_cohort ####
remote_CDM=params$remote_CDM
cdm_db_link=config_file$cdm_db_link
cdm_db_name=config_file$cdm_db_name
cdm_db_schema=config_file$cdm_db_schema
start_date="2010-01-01"
end_date="2018-12-31"
verb=F

statements<-paste0(
  paste0("./inst/",DBMS_type),
  c("/cohort_initial.sql",
    "/cohort_all_SCr.sql",
    "/cohort_enc_SCr.sql",
    "/cohort_baseline_SCr.sql",
    "/cohort_exclude.sql",
    "/cohort_eligib.sql",
    "/cohort_AKI_staging.sql",
    "/cohort_final.sql")
)

sql<-parse_sql(file_path=statements[5],
               cdm_db_link=cdm_db_link,
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema,
               start_date=start_date,
               end_date=end_date)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))


# test medication extraction and summary
med<-dbGetQuery(conn,
                parse_sql(paste0("./inst/",params$DBMS_type,"/collect_med.sql"),
                          cdm_db_link=config_file$cdm_db_link,
                          cdm_db_name=config_file$cdm_db_name,
                          cdm_db_schema=config_file$cdm_db_schema)$statement) %>%
  dplyr::mutate(RX_EXPOS=round(pmin(pmax(as.numeric(difftime(RX_END_DATE,RX_START_DATE,units="days")),1),
                                    pmax(RX_DAYS_SUPPLY,1),na.rm=T))) %>%
  replace_na(list(RX_QUANTITY_DAILY=1)) %>%
  dplyr::mutate(sdsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,RXNORM_CUI,RX_BASIS,RX_EXPOS,RX_QUANTITY_DAILY,sdsa) %>%
  unite("key",c("RXNORM_CUI","RX_BASIS"),sep=":")

#converted to daily exposure
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

#merge overlapped precribing intervals
med2 %<>% 
  group_by(PATID,ENCOUNTERID,key,sdsa,edsa,dsa) %>%
  dplyr::summarize(value=value[1]) %>%
  ungroup

med<-med2 %>%
  group_by(PATID,ENCOUNTERID,key,sdsa) %>%
  dplyr::summarize(RX_EXPOS=sum(value,na.rm=T)) %>%
  ungroup


