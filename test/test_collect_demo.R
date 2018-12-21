#### test: demo collections ####
source("./R/util.R")
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr",
                    "kableExtra"
                    ))
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
Table1<-readRDS("./data_local/data_raw/Table1.rda")
enc_tot<-length(unique(Table1$ENCOUNTERID))
# critical dates of AKI encounters
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


#statements to be tested
sql<-parse_sql(paste0("./inst/",params$DBMS_type,"/collect_demo.sql"),
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
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
  dplyr::select(PATID,ENCOUNTERID,
                AGE,AGE_GRP,SEX,RACE,HISPANIC,DDAYS_SINCE_ENC) %>%
  replace_na(list(AGE="NI",
                  AGE_GRP="NI",
                  SEX="NI",
                  RACE="NI",
                  HISPANIC="NI")) %>%
  gather(key,value,-PATID,-ENCOUNTERID) %>%
  unique
#passed!
demo<-readRDS("./data_local/data_raw/AKI_DEMO.rda")


#collect summaries
demo_summ<-aki_stage_ind %>% 
  filter(!chk_pt %in% c("DISCHARGE")) %>%
  dplyr::select(-critical_date) %>%
  left_join(demo %>% 
              filter(!(key %in% c("AGE","DDAYS_SINCE_ENC"))), 
            by="ENCOUNTERID") %>%
  group_by(chk_pt,stg_tot_cnt,key,value) %>%
  #HIPAA compliance, low count masking
  dplyr::summarize(enc_cnt = ifelse(n()<11,11,n())) %>%
  mutate(enc_prop = ifelse(enc_cnt>11,round(enc_cnt/stg_tot_cnt[1],3),11)) %>%
  ungroup %>%
  dplyr::select(-stg_tot_cnt) %>%
  gather(summ,summ_val,-chk_pt,-key,-value) %>%
  bind_rows(aki_stage_ind %>%
              filter(!chk_pt %in% c("DISCHARGE")) %>%
              dplyr::select(chk_pt,stg_tot_cnt) %>% 
              unique %>%
              #HIPAA compliance, low count masking
              dplyr::rename(enc_cnt=stg_tot_cnt) %>%
              mutate(enc_cnt=ifelse(enc_cnt<11,11,enc_cnt)) %>%
              mutate(enc_prop=ifelse(enc_cnt>11,round(enc_cnt/enc_tot,3),11),
                     key="TOTAL",
                     value="(%/overall)") %>%
              gather(summ,summ_val,-chk_pt,-key,-value) %>%
              dplyr::select(key,value,chk_pt,summ,summ_val)) %>%
  unite("stg_summ",c("chk_pt","summ")) %>%
  unique %>% spread(stg_summ,summ_val) %>%
  replace(.,is.na(.),0)

#passed!


demo_nice_tbl<-demo_summ %>%
  gather(summ,summ_val,-key,-value) %>%
  mutate(summ_val=ifelse(grepl("_prop",summ),summ_val*100,summ_val)) %>%
  mutate(summ_val=as.character(summ_val)) %>%
  mutate(summ_val=ifelse(grepl("_enc",summ) & summ_val=="11","<11",summ_val)) %>%
  mutate(summ_val=ifelse(grepl("_prop",summ) & summ_val=="1100","<11",
                         ifelse(grepl("_prop",summ) & summ_val!="1100",paste0(summ_val,"%"),summ_val))) %>%
  spread(summ,summ_val) %>%
  unite("ADMIT",paste0("ADMIT_",c("enc_cnt","enc_prop")),sep=", ") %>%
  unite("AKI1",paste0("AKI1_",c("enc_cnt","enc_prop")),sep=", ") %>%
  unite("AKI2",paste0("AKI2_",c("enc_cnt","enc_prop")),sep=", ") %>%
  unite("AKI3",paste0("AKI3_",c("enc_cnt","enc_prop")),sep=", ") %>%
  unite("NONAKI",paste0("NONAKI_",c("enc_cnt","enc_prop")),sep=", ") %>%
  arrange(key,value)

row_grp_pos<-demo_nice_tbl %>% 
  mutate(rn=1:n()) %>%
  group_by(key) %>%
  dplyr::summarize(begin=rn[1],
                   end=rn[n()]) %>%
  ungroup

kable(demo_nice_tbl,
      caption="Table1 - Demographic Summaries at AKI1, AKI2, AKI3") %>%
  kable_styling("striped", full_width = F) %>%
  group_rows("Age Group", row_grp_pos$begin[1],row_grp_pos$end[1]) %>%
  group_rows("Hispanic", row_grp_pos$begin[2],row_grp_pos$end[2]) %>%
  group_rows("Race", row_grp_pos$begin[3],row_grp_pos$end[3]) %>%
  group_rows("Sex", row_grp_pos$begin[4],row_grp_pos$end[4]) %>%  
  group_rows("Total",row_grp_pos$begin[5],row_grp_pos$end[5])