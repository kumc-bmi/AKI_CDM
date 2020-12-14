##########################
#### collect metadata ####
##########################
source("./R/util.R")
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "openxlsx"))

config_file<-read.csv("./config/config.csv",stringsAsFactors = F)
conn<-connect_to_db("Oracle","JDBC",config_file)

#====collect all exising variables=======
pred_in_d_opt<-1
pred_task_lst<-c("stg02up","stg01","stg12up")

varlst<-c()
for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    
    dat_ds<-readRDS(paste0("./data/preproc/data_mrv_",pred_in_d,"d_",pred_task,".rda"))
    
    varlst<-unique(c(varlst,dat_ds[[2]][["X_proc"]]$key))
    
  }
}


#====metadata - source I: PCORNET CDM website====
cdm_metadata<-read.xlsx("https://pcornet.org/wp-content/uploads/2020/06/2020-06-17-PCORnet-Common-Data-Model-v5dot1-parseable.xlsx", ##may be updated
                        sheet = 6) %>%
  mutate(key=paste0(FIELD_NAME,"_",VALUESET_ITEM),
         descriptor=paste0(FIELD_NAME,",",VALUESET_ITEM_DESCRIPTOR)) %>%
  bind_rows(data.frame(TABLE_NAME=c("DEMOGRAPHIC",rep("VITAL",5)),
                       FIELD_NAME=c("DEMO",rep("VITAL",5)),
                       key=c("AGE",
                             "BP_DIASTOLIC",
                             "BP_SYSTOLIC",
                             "BMI","HT","WT"),
                       descriptor=c("age",
                                    "diastolic blood pressure",
                                    "systolic blood pressure",
                                    "body mass index",
                                    "height",
                                    "weight"),
                       stringsAsFactors=F)) %>%
  select(TABLE_NAME,FIELD_NAME,key,descriptor)


#====metadata - source II: use pcornet metadata tables====
#--TODO: use UMLS as the master data dictionary
meta_sql<-parse_sql("../AKI_CDM/src/Oracle/metadata.sql",
                    cdm_meta_schema=config_file$cdm_meta_schema,
                    i2b2_meta_schema=config_file$i2b2_meta_schema)

execute_single_sql(conn,
                   statement=meta_sql$statement,
                   write=(meta_sql$action=="write"),
                   table_name=toupper(meta_sql$tbl_out))

#chunk_load due to size of the table
metadata<-chunk_load(conn=conn,dataset=meta_sql$tbl_out,chunk_size=5000)

metadata %<>%
  mutate(key=case_when(ITEM_TYPE=="09" ~ paste0("09:",VALUESET_ITEM),
                       ITEM_TYPE=="10" ~ paste0("10:",VALUESET_ITEM),
                       ITEM_TYPE %in% c("C4","HC","CPT")~ paste0("CH:",VALUESET_ITEM),
                       TRUE ~ VALUESET_ITEM),
         descriptor=VALUESET_ITEM_DESCRIPTOR) %>%
  select(TABLE_NAME,FIELD_NAME,key,descriptor) %>%
  bind_rows(pcori_metadata %>% filter(ITEM_TYPE=="RXNORM") %>%
              mutate(TABLE_NAME="MED_ADMIN",
                     FIELD_NAME="MEDADMIN_CODE",
                     key=paste0(VALUESET_ITEM,":RX"),
                     descriptor=paste0(VALUESET_ITEM_DESCRIPTOR,",administered")) %>%
              select(TABLE_NAME,FIELD_NAME,key,descriptor)) %>%
  bind_rows(pcori_metadata %>% filter(ITEM_TYPE=="NDC") %>%
              mutate(TABLE_NAME="DISPENSING",
                     FIELD_NAME="NDC",
                     key=paste0(VALUESET_ITEM,":ND"),
                     descriptor=paste0(VALUESET_ITEM_DESCRIPTOR,",dispensed")) %>%
              select(TABLE_NAME,FIELD_NAME,key,descriptor)) %>%
  bind_rows(pcori_metadata %>% filter(ITEM_TYPE=="RXNORM") %>%
              mutate(TABLE_NAME="PRESCRIBING",
                     FIELD_NAME="RXNORM_CUI",
                     key=paste0(VALUESET_ITEM,":01"),
                     descriptor=paste0(VALUESET_ITEM_DESCRIPTOR,",order to dispense")) %>%
              select(TABLE_NAME,FIELD_NAME,key,descriptor)) %>%
  bind_rows(pcori_metadata %>% filter(ITEM_TYPE=="NDC") %>%
              mutate(TABLE_NAME="PRESCRIBING",
                     FIELD_NAME="RXNORM_CUI",
                     key=paste0(VALUESET_ITEM,":02"),
                     descriptor=paste0(VALUESET_ITEM_DESCRIPTOR,",order to administer")) %>%
              select(TABLE_NAME,FIELD_NAME,key,descriptor))


#====metadata - source III: CCS category====
ccs_ref<-readRDS("../AKI_CDM/ref/ccs_ref.rda") %>%
  dplyr::rename(descriptor=ccs_name) %>%
  dplyr::mutate(key=as.character(ccs_code),
                FIELD_NAME="CCS",TABLE_NAME="DIAGNOSIS") %>%
  dplyr::select(TABLE_NAME,FIELD_NAME,key,descriptor)


#====metadata - source IV: additional====
add_ft<-data.frame(TABLE_NAME="additional",
                   FIELD_NAME="additional",
                   key="BUN_SCR",
                   descriptor="BUN SCr ratio",
                   stringsAsFactors = F)
  # add_row(TABLE_NAME="additional",
  #         FIELD_NAME="additional",
  #         key="...",
  #         descriptor="...")


##========== metadata set =============##
data_dict<-data.frame(var=varlst,stringsAsFactors = F) %>%
  mutate(var_orig=case_when(grepl(":((RX)|(ND)|(01)|(02))+",var)&!grepl("^(10|09)+",var) ~ gsub(":.[^:]*$","",var),
                            grepl("_((min)|(slope)|(change))+",var) ~ gsub("_.[^_]*$","",var),
                            TRUE~var)) %>%
  left_join(metadata %>%
               bind_rows(cdm_metadata) %>%
               bind_rows(ccs_ref) %>%
               bind_rows(add_ft) %>%
               unique,
             by=c("var_orig"="key")) %>%
  #manual curation
  mutate(descriptor=case_when(is.na(descriptor)&grepl("^(day)+",var) ~ var,
                              grepl("_((min)|(slope)|(change))+",var) ~ paste0(gsub(".*_","",var)," of ",descriptor),
                              grepl("_(cum)+",var) ~ paste0("cumulative exposure of ",descriptor),
                              TRUE ~ descriptor
                              )) %>%
  filter(!is.na(TABLE_NAME))

write.csv(data_dict,file="./data/data_dict.csv",row.names = F)


