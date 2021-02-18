################################################
#### transform data into compressed format #####
################################################

## this script is to transfor multi-center CDMs into a compressed format
## ticket: https://bmi-work.kumc.edu/work/ticket/6428

rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr"))


#-----CDM table list
tbl_lst<-c("demo","vital","lab","dx","px","med")


#-----compress data------
#---initialization with demo table
tbl_out<-compress_df(dat=readRDS("./data/raw/AKI_DEMO.rda"),
                     tbl=tbl_lst[1])

tbl_zip<- tbl_out$tbl_zip %>%
  setNames(c("ENCOUNTERID",tbl_lst[1]))

meta<-tbl_out$idx_map %>%
  mutate(table_name=tbl_lst[1])

#---compress other components
for(tbl_in in tbl_lst[-1]){
  tbl_out<-compress_df(dat=readRDS(paste0("./data/raw/AKI_",toupper(tbl_in),".rda")),
                       tbl=tbl_in)
  
  tbl_zip %<>%
    left_join(tbl_out$tbl_zip %>%
                setNames(c("ENCOUNTERID",tbl_in)),
              by="ENCOUNTERID") %>%
    replace(is.na(.),"0")
  
  meta %<>%
    bind_rows(tbl_out$idx_map %>%
                mutate(key=as.character(key),
                       table_name=tbl_in))
}

#---attach AKI stage info
aki<-readRDS("./data/raw/Table1.rda") %>%
  select(ENCOUNTERID,ADMIT_DATE,AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT,NONAKI_SINCE_ADMIT) %>%
  gather(aki_stg,dsa,-ENCOUNTERID,-ADMIT_DATE) %>%
  filter(!is.na(dsa)) %>%
  mutate(aki_stg=gsub("_.*","",aki_stg)) %>%
  mutate(aki_stg=recode(aki_stg,
                        "AKI1"="1",
                        "AKI2"="2",
                        "AKI3"="3",
                        "NONAKI"="0")) %>%
  unite("fstr",c("aki_stg","dsa"),sep=",") %>%
  group_by(ENCOUNTERID,ADMIT_DATE) %>%
  arrange(fstr) %>%
  dplyr::summarise(AKI_label=paste(fstr,collapse="_")) %>%
  ungroup %>%
  mutate(admit_yr=as.numeric(format(ADMIT_DATE,"%Y"))) %>%
  select(-ADMIT_DATE)


#---
tbl_zip %<>%
  left_join(aki %>% select(-admit_yr),by="ENCOUNTERID") %>%
  unite("fstr",c(tbl_lst,"AKI_label"),sep="|")



##save results
#save data by year
for (yr in unique(aki$admit_yr)){
  tbl_zip_yr<-tbl_zip %>%
    semi_join(aki %>% filter(admit_yr == yr),by="ENCOUNTERID")
  
  write.table(tbl_zip_yr, file=paste0("./data/compressed/",params$site,"_data_",yr,".txt"),
              append = F,row.names = F, col.names = F)
}












#-----metadata table from KUMC
# metadata<-readRDS("./data/meta/metadata.rda")


##complete metadata table
meta %<>%
  select(idx,key,table_name) %>%
  mutate(key2=case_when(table_name=="med"~gsub(":.*","",key),
                        TRUE ~ key)) %>%
  left_join(metadata %>% 
              mutate(key2=case_when(TABLE_NAME %in% c("MED_ADMIN","DISPENSING") ~ VALUESET_ITEM,
                                    TRUE ~ key)) %>%
              select(key2,VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR),
            by=c("key2")) %>%
  select(-key2) %>% unique %>%
  dplyr::rename(VAR_IDX=idx,VAR_CD=key,TABLE_NAME=table_name)

##save results
#save data by year
for (yr in unique(aki$admit_yr)){
  tbl_zip_yr<-tbl_zip %>%
    semi_join(aki %>% filter(admit_yr == yr),by="ENCOUNTERID")
  
  write.table(tbl_zip_yr, file=paste0("./data/compressed/",params$site,"_data_",yr,".txt"),
              append = F,row.names = F, col.names = F)
}


write.csv(meta,file=paste0("./data/compressed/",params$site,"_metadata.csv"),
          row.names = F)

