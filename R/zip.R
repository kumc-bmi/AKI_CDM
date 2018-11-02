#######################
# compress data table #
#######################
#clean up the slate
rm(list=ls())
gc()

#load libraries
source("./R/util.R")
require_libraries(c("DBI",
                    "ROracle",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))

# #load in metadata table
# config_file_path<-"./config.csv"
# config_file<-read.csv(config_file_path,stringsAsFactors = F)
# conn<-connect_to_db("Oracle",config_file)
# 
# meta<-dbGetQuery(conn,"select * from CDM_metadata")
# saveRDS(meta,"./data/ft_metadata.rda")


#source utility functions
meta<-read.csv("./data/meta_cdm.csv",stringsAsFactors = F) %>%
  bind_rows(readRDS("./data/ft_metadata.rda"))

type_vec<-c("DEMO","VITAL","LAB","DX","PX","MED")
ft_zip<-c()
ft_idx<-c()
for(i in seq_along(type_vec)){
  start_i<-Sys.time()
  
  dat<-readRDS(paste0("./data/AKI_",type_vec[i],".rda"))
  dat_zip<-compress_df(dat,tbl=type_vec[i])
  
  #stack data
  ft_zip %<>% bind_rows(dat_zip$tbl_zip %>% mutate(pos=i))
  
  #stack metadata
  idx_map<-dat_zip$idx_map %>% mutate(pos=i)
  if(type_vec[i] == "DEMO"){
    idx_map %<>% mutate(FIELD_NAME=key) %>%
      left_join(meta, by="FIELD_NAME") %>%
      arrange(idx) %>%
      mutate(TABLE_NAME=ifelse(key=="AGE","DEMOGRAPHIC",TABLE_NAME),
             FIELD_NAME=ifelse(key=="AGE",key,FIELD_NAME),
             VALUESET_ITEM=ifelse(key=="AGE",key,VALUESET_ITEM),
             VALUESET_ITEM_DESCRIPTOR=ifelse(key=="AGE","Age at admission",VALUESET_ITEM_DESCRIPTOR))
  }else if(type_vec[i] == "VITAL"){
    idx_map %<>% mutate(key=substr(key,2,99)) %>%
      left_join(meta, by=c("key"="FIELD_NAME")) %>%
      mutate(TABLE_NAME=ifelse(is.na(TABLE_NAME),"VITAL",TABLE_NAME),
             FIELD_NAME=key,
             VALUESET_ITEM=ifelse(is.na(VALUESET_ITEM),key,VALUESET_ITEM),
             VALUESET_ITEM_DESCRIPTOR=case_when(key=="HT" ~ "height",
                                                key=="WT" ~ "weight",
                                                key=="BMI" ~ "body mass index",
                                                key=="BP_SYSTOLIC" ~ "systolic blood pressure",
                                                key=="BP_DIASTOLIC" ~ "diastolic blood pressure",
                                                key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE") ~ VALUESET_ITEM_DESCRIPTOR))
  }else if(type_vec[i] == "LAB"){
    idx_map %<>% mutate(VALUESET_ITEM=key) %>%
      inner_join(meta %>% filter(TABLE_NAME=="LAB_RESULT_CM"),
                 by="VALUESET_ITEM") %>%
      group_by(key,idx,pos,TABLE_NAME,FIELD_NAME) %>%
      arrange(desc(VALUESET_ITEM_DESCRIPTOR)) %>% 
      dplyr::slice(1:1) %>% ungroup %>%
      filter(key!="NI")
  }else if(type_vec[i] == "DRG"){
    idx_map %<>% mutate(VALUESET_ITEM=key) %>%
      inner_join(meta %>% filter(TABLE_NAME=="ENCOUNTER" & VAR_TYPE=="DRG") %>%
                   mutate(VALUESET_ITEM=gsub("CMSDRG:","",VALUESET_ITEM)),
                 by="VALUESET_ITEM") %>%
      group_by(key,idx,pos,TABLE_NAME,FIELD_NAME,VALUESET_ITEM) %>%
      arrange(desc(VALUESET_ITEM_DESCRIPTOR)) %>% 
      dplyr::slice(1:1) %>% ungroup
  }else if(type_vec[i] == "DX"){
    load("./data/ccs_icd_cw.Rdata")
    idx_map %<>% mutate(VALUESET_ITEM=key) %>%
      inner_join(ccs_icd %>% 
                   dplyr::rename(VALUESET_ITEM_DESCRIPTOR=ccs_name) %>%
                   mutate(VALUESET_ITEM=ccs_code,
                          TABLE_NAME="DIAGNOSIS",
                          FIELD_NAME="DX_CCS") %>%
                   dplyr::select(TABLE_NAME,FIELD_NAME,
                                 VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR),
                 by="VALUESET_ITEM") %>%
      unique %>% 
      mutate(key=as.character(key),
             VALUESET_ITEM=as.character(VALUESET_ITEM)) %>%
      group_by(key,idx,pos,TABLE_NAME,FIELD_NAME,VALUESET_ITEM) %>%
      arrange(desc(VALUESET_ITEM_DESCRIPTOR)) %>% 
      dplyr::slice(1:1) %>% ungroup
  }else if(type_vec[i] == "PX"){
    idx_map %<>% mutate(VALUESET_ITEM=case_when(grepl("^09",key) ~ gsub("^09","ICD9",key),
                                                grepl("^10",key) ~ gsub("^10","ICD10",key),
                                                grepl("^CH",key) ~ gsub("CH:","",key))) 
    idx_map %<>%
      left_join(meta %>% filter(TABLE_NAME=="PROCEDURE" & VALUESET_ITEM != "NI") %>%
                   mutate(VALUESET_ITEM=ifelse(grepl("^(CPT|HCPCS)+",VALUESET_ITEM),
                                               gsub(".*\\:","",VALUESET_ITEM),
                                               VALUESET_ITEM)),
                 by="VALUESET_ITEM") %>%
      unique %>%
      group_by(key,idx,pos,TABLE_NAME,FIELD_NAME,VALUESET_ITEM) %>%
      arrange(desc(VALUESET_ITEM_DESCRIPTOR)) %>% 
      dplyr::slice(1:1) %>% ungroup
  }else if(type_vec[i] == "MED"){
    idx_map %<>% mutate(VALUESET_ITEM=gsub("\\:.*","",key)) %>%
      inner_join(meta %>% filter(TABLE_NAME=="PRESCRIBING" & VALUESET_ITEM != "NI"),
                 by="VALUESET_ITEM") %>%
      unique
  }else{
    warning("features are not mappable!")
  }
  
  ft_idx %<>% bind_rows(idx_map)
  
  lapse_i<-Sys.time()-start_i
  cat("finish compressing table",type_vec[i],"in",lapse_i,units(lapse_i),".\n")
}

dat<-readRDS("./data/Table1.rda") %>%
  dplyr::select(ENCOUNTERID,
                AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT,
                NONAKI_SINCE_ADMIT) %>%
  gather(label, date,-ENCOUNTERID) %>%
  filter(!is.na(date)) %>%
  mutate(label=gsub("_.*","",label)) %>%
  mutate(label=recode(label,
                      AKI1=1,
                      AKI2=2,
                      AKI3=3,
                      NONAKI=0)) %>%
  arrange(label) %>%
  unite("lable_dt",c("label","date"),sep=",") %>%
  group_by(ENCOUNTERID) %>%
  dplyr::summarize(fstr=paste(lable_dt,collapse="_")) %>%
  ungroup %>%  mutate(pos=length(type_vec)+1)
  
ft_zip %<>% bind_rows(dat) 

ft_zip %<>%
  spread(pos,fstr,fill=0) %>%
  unite("feature_string",c("1","2","3","4","5","6","7"),sep="|") %>%
  unique

ft_idx %<>% 
  mutate(VALUESET_ITEM=paste0("'",VALUESET_ITEM,"'")) %>%
  dplyr::rename(VAR_IDX=idx,VAR_POS=pos) %>%
  dplyr::select(-key,-I2B2_HLEVEL,-ITEM_TYPE)

#save data
saveRDS(ft_zip,file="./data/ft_zip.rda")
saveRDS(ft_idx,file="./data/ft_idx.rda")
write.csv(ft_idx,file="./data/feature_dict.csv",row.names = F)

# #example inspection
ft_zip_ex<-readRDS("./data/ft_zip.rda") %>%
  dplyr::slice(1:10)

#break down into calendar years
#if break, reload
# ft_zip<-readRDS("./data/ft_zip.rda")
enc_yr<-readRDS("./data/Table1.rda") %>%
  dplyr::select(PATID,ENCOUNTERID,ADMIT_DATE) %>%
  mutate(yr=as.numeric(format(ADMIT_DATE,"%Y"))) %>%
  dplyr::select(-ADMIT_DATE)

yr_vec<-unique(enc_yr$yr)
yr_vec<-yr_vec[order(yr_vec)][-1]
for(i in yr_vec){
  ft_zip_yr<-ft_zip %>% 
    semi_join(enc_yr %>% filter(yr==i),
              by="ENCOUNTERID")
  
  write.table(ft_zip_yr,file=paste0("./data/ft_zip",i,".txt"),
              col.names=F,quote=T,row.names = F)
}

