#### test: lab collections ####
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
sql<-parse_sql(paste0("./inst/",params$DBMS_type,"/collect_lab.sql"),
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema)

#collect lab
lab<-execute_single_sql(conn,
                        statement=sql$statement,
                        write=(sql$action=="write")) %>%
  dplyr::rename(key=LAB_LOINC,value=RESULT_NUM,unit=RESULT_UNIT,
                dsa=DAYS_SINCE_ADMIT,timestamp=SPECIMEN_DATE_TIME) %>%
  dplyr::select(PATID,ENCOUNTERID,key,value,unit,dsa,timestamp) %>%
  filter(!is.na(key) & !is.na(value)) %>%
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


#collect summaries
lab_summ<-lab %>% 
  group_by(key) %>%
  dplyr::summarize(record_cnt=n(),
                   enc_cnt=length(unique(ENCOUNTERID)),
                   min=min(value,na.rm=T),
                   mean=round(mean(value,na.rm=T),2),
                   sd=round(sd(value,na.rm=T),3),
                   median=round(median(value,na.rm=T)),
                   max=max(value,na.rm=T)) %>%
  ungroup %>%
  mutate(cov=round(sd/mean,3)) %>%
  mutate(freq_rk=rank(-enc_cnt,ties.method="first")) %>%
  #HIPPA, low counts masking
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
                       max=max(value,na.rm=T)) %>%
      ungroup %>%
      mutate(cov=round(sd/mean,3)) %>%
      #HIPPA, low counts masking
      mutate(enc_cnt=ifelse(as.numeric(enc_cnt)<11 & as.numeric(enc_cnt)>0,"<11",as.character(enc_cnt)),
             record_cnt=ifelse(as.numeric(record_cnt)<11 & as.numeric(record_cnt)>0,"<11",as.character(record_cnt)),
             sd=ifelse(is.nan(sd),0,sd)) %>%
      gather(summ,summ_val,-key,-dsa_grp) %>%
      spread(dsa_grp,summ_val),
    by=c("key","summ")
  ) %>%
  arrange(freq_rk,summ) %>%
  #additional 
  mutate(at_admission=ifelse(is.na(`1`),0,1),
         within_3d=ifelse(is.na(coalesce(`1`,`2`,`3`)),0,1))
#passed!

#data for plotting
lab_temp<-lab_summ %>%
  filter(summ %in% c("enc_cnt","record_cnt")) %>%
  dplyr::select(key,summ,overall) %>% unique %>%
  filter(overall!="<11") %>%
  mutate(overall=as.numeric(overall)) %>%
  spread(summ,overall,fill=0) %>%
  filter(enc_cnt > 0) %>%
  mutate(record_intensity=round(record_cnt/enc_cnt,2)) %>%
  mutate(label=ifelse(dense_rank(-enc_cnt)<=10 | dense_rank(-record_intensity)<=10,key,""))

p1<-ggplot(lab_temp,aes(x=record_intensity,y=enc_cnt,label=label))+
  geom_point()+ geom_text_repel(segment.alpha=0.5,segment.color="grey")+
  scale_y_continuous(sec.axis = sec_axis(trans= ~./enc_tot,
                                         name = 'Percentage'))+
  labs(x="Average Records per Encounter",
       y="Encounter Counts",
       title="Figure 1 - Data Density vs. Records Intensity")
#passed!

#find links
lab_report<-lab_temp %>%
  dplyr::filter(key != "NI") %>%
  arrange(desc(enc_cnt)) %>% 
  dplyr::select(key) %>%
  unique %>% slice(1:5) %>%
  bind_rows(
    lab_temp %>% 
      dplyr::filter(key != "NI") %>%
      arrange(desc(record_intensity)) %>% 
      dplyr::select(key) %>%
      unique %>% slice(1:2)
  ) %>%
  mutate(link=lapply(key,get_loinc_ref))
#passed!

