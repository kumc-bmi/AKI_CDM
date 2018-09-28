#### test execute_..._sql() ####
source("./R/util.R")
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))

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

# auxilliary summaries and tables
Table1<-readRDS("./data/Table1.rda")
enc_tot<-length(unique(Table1$ENCOUNTERID))

#statements to be tested
sql<-parse_sql(paste0("./inst/",DBMS_type,"/collect_px.sql"),
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema)

#collect px
px<-execute_single_sql(conn,
                       statement=sql$statement,
                       write=(sql$action=="write")) 
#passed!


#preprocessing
px %<>%
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
#passed!


#summarizing
px_summ<-px %>%
  group_by(key,dsa_grp) %>%
  dplyr::summarize(record_cnt=n(),
                   pat_cnt=length(unique(PATID)),
                   enc_cnt=length(unique(ENCOUNTERID))) %>%
  ungroup %>%
  #HIPPA, low counts masking
  mutate(pat_cnt=ifelse(as.numeric(pat_cnt)<11,"<11",pat_cnt),
         enc_cnt=ifelse(as.numeric(enc_cnt)<11,"<11",enc_cnt),
         record_cnt=ifelse(as.numeric(record_cnt)<11,"<11",record_cnt)) %>%
  arrange(key,dsa_grp)
#passed!

#data for plotting
px_temp<-px_summ %>% 
  dplyr::select(key,dsa_grp,enc_cnt) %>% 
  filter(enc_cnt!="<11") %>%
  mutate(enc_cnt=as.numeric(enc_cnt)) %>%
  filter(enc_cnt > 0) %>%
  arrange(desc(enc_cnt)) %>%
  mutate(label=ifelse(dense_rank(-enc_cnt)<=10,key,""))

p1<-ggplot(px_temp,aes(x=dsa_grp,y=enc_cnt,label=label))+
  geom_point()+geom_text_repel()+
  scale_y_continuous(sec.axis = sec_axis(trans= ~./enc_tot,
                                         name = 'Percentage'))+
  labs(x="Days since Admission",
       y="Encounter Counts",
       title="Figure 3 - Procedure Density over Time")
#passed!

#collect links
px_report<-px_temp %>%
  arrange(desc(enc_cnt)) %>%
  dplyr::select(key) %>%
  unique %>% slice(1:5) %>%
  mutate(link=lapply(key,google_code))
#passed

