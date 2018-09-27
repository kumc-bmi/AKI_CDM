#### test: dx collection ####
source("./R/util.R")
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))
load("./data/ccs_icd_cw.Rdata")
params<-list(  DBMS_type="Oracle",
               remote_CDM=FALSE)


config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,config_file)
DBMS_type<-attr(conn,"DBMS_type")


#set up parameters
remote_CDM=params$remote_CDM
cdm_db_link=config_file$cdm_db_link
cdm_db_name=config_file$cdm_db_name
cdm_db_schema=config_file$cdm_db_schema
start_date="2010-01-01"
end_date="2018-12-31"
verb=F

#statements to be tested
sql<-parse_sql(paste0("./inst/",DBMS_type,"/collect_dx.sql"),
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema)

#collect dx
dx<-execute_single_sql(conn,
                       statement=sql$statement,
                       write=(sql$action=="write")) %>%
  #attach CCS diagnosis grouping
  dplyr::mutate(DX_ICD=paste0("ICD",DX_TYPE,":",DX)) %>%
  left_join(ccs_icd %>% select(-ccs_name),by=c("DX_ICD"="icd_w_type")) %>%
  unique %>% filter(!is.na(ccs_code)) %>%
  dplyr::rename(key=ccs_code, dsa=DAYS_SINCE_ADMIT) %>%
  dplyr::select(PATID,ENCOUNTERID,key,dsa) %>%
  unique
#passed!


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
  arrange(key)
#passed!


#data for plotting
dx_temp<-dx_summ %>% 
  dplyr::select(key,enc_cnt,mean_history) %>%
  filter(enc_cnt!="<11") %>%
  mutate(enc_cnt=as.numeric(enc_cnt),
         mean_history=as.numeric(mean_history)) %>%
  filter(enc_cnt > 0) %>%
  mutate(label=ifelse(dense_rank(-enc_cnt)<=10,key,""))

p1<-ggplot(dx_temp,aes(x=mean_history,y=enc_cnt,label=label))+
  geom_point()+geom_text_repel()+
  scale_y_continuous(sec.axis = sec_axis(trans= ~./enc_tot,
                                         name = 'Percentage'))+
  labs(x="Mean History of Diagnoses (Days)",
       y="Encounter Counts",
       title="Figure 2 - Data Density vs. Recency (CCS)")
#passed!

#find links
load("./data/ccs_ref.Rdata")
dx_report<-dx_temp %>% 
  arrange(desc(enc_cnt)) %>%
  slice(1:6) %>%
  dplyr::select(key) %>%
  left_join(ccs_ref %>% filter(type=="dx"),
            by=c("key"="ccs_code")) %>%
  dplyr::select(key,ccs_name)
#passed!

