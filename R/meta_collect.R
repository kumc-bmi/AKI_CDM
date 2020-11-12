##########################
#### collect metadata ####
##########################

source("./R/util.R")

require_libraries(c("DBI",
                    "ROracle",
                    "tidyr",
                    "dplyr",
                    "magrittr"))

config_file_path<-"./config/config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db("Oracle","OCI",config_file)

#====metadata - source I: PCORNET CDM website====
cdm_metadata<-read.csv("./data/meta_data/cdm_metadata.csv",stringsAsFactors = F) %>%
  dplyr::mutate(FIELD_NAME2=FIELD_NAME,
                VALUESET_ITEM2=VALUESET_ITEM) %>%
  unite("VALUESET_ITEM2",c("FIELD_NAME2","VALUESET_ITEM2"),sep="_") %>%
  bind_rows(data.frame(TABLE_NAME=c("DEMOGRAPHIC",rep("VITAL",5)),
                       FIELD_NAME=c("DEMO",rep("VITAL",5)),
                       VALUESET_ITEM=c("AGE",
                                       "BP_DIASTOLIC",
                                       "BP_SYSTOLIC",
                                       "BMI",
                                       "HT",
                                       "WT"),
                       VALUESET_ITEM_DESCRIPTOR=c("age",
                                                  "diastolic blood pressure",
                                                  "systolic blood pressure",
                                                  "body mass index",
                                                  "height",
                                                  "weight"),
                       stringsAsFactors=F) %>%
              dplyr::mutate(VALUESET_ITEM2=VALUESET_ITEM))


#====metadata - source II: local metadata table====
meta_sql<-parse_sql("./src/Oracle/metadata_i2b2.sql",
                    cdm_meta_schema=config_file$cdm_meta_schema)
i2b2_metadata<-execute_single_sql(conn,
                                  statement=meta_sql$statement,
                                  write=(meta_sql$action=="write"),
                                  table_name=toupper(sql$tbl_out))

# #save loinc
# saveRDS(i2b2_metadata %>% filter(FIELD_NAME=="LOINC"),
#         "./ref/loinc_metadata.rda")

# #if breaks
# i2b2_metadata<-readRDS("./data/meta_data/i2b2_metadata.rda")
i2b2_metadata %<>%
  mutate(VALUESET_ITEM2=case_when(FIELD_NAME=="DX"&ITEM_TYPE=="09" ~ paste0("ICD9:",VALUESET_ITEM),
                                  FIELD_NAME=="DX"&ITEM_TYPE=="10" ~ paste0("ICD10:",VALUESET_ITEM),
                                  FIELD_NAME=="RXNORM_CUI" ~ paste0(gsub(":.*",":01",VALUESET_ITEM)),
                                  FIELD_NAME=="PX"&grepl("((^CPT\\:)|(^HCPCS\\:))+",VALUESET_ITEM) ~ gsub("((^CPT\\:)|(^HCPCS\\:))+","CH:",VALUESET_ITEM),
                                  TRUE ~ VALUESET_ITEM)) %>%
  bind_rows(i2b2_metadata %>% filter(FIELD_NAME=="RXNORM_CUI") %>%
              mutate(VALUESET_ITEM2=ifelse(grepl("\\:",VALUESET_ITEM),
                                           paste0(gsub(":.*",":01",VALUESET_ITEM)),
                                           paste0(VALUESET_ITEM,":01"))))  %>%
  bind_rows(i2b2_metadata %>% filter(FIELD_NAME=="RXNORM_CUI") %>%
              mutate(VALUESET_ITEM2=ifelse(grepl("\\:",VALUESET_ITEM),
                                           paste0(gsub(":.*",":02",VALUESET_ITEM)),
                                           paste0(VALUESET_ITEM,":02"))))  %>%
  bind_rows(i2b2_metadata %>% filter(FIELD_NAME=="RXNORM_CUI") %>%
              mutate(VALUESET_ITEM2=paste0(gsub(":.*","",VALUESET_ITEM)))) %>%
  dplyr::select(TABLE_NAME,FIELD_NAME,VALUESET_ITEM,VALUESET_ITEM2,VALUESET_ITEM_DESCRIPTOR)


#====metadata - source III: CCS category====
ccs_ref<-readRDS("./data/meta_data/ccs_ref.rda") %>%
  dplyr::rename(VALUESET_ITEM_DESCRIPTOR=ccs_name) %>%
  dplyr::mutate(VALUESET_ITEM=as.character(ccs_code),
                FIELD_NAME="CCS",TABLE_NAME="DIAGNOSIS") %>%
  dplyr::mutate(VALUESET_ITEM2=VALUESET_ITEM) %>%
  dplyr::select(TABLE_NAME,FIELD_NAME,VALUESET_ITEM,VALUESET_ITEM2,VALUESET_ITEM_DESCRIPTOR)


#====metadata - source IV: additional====
add_ft<-data.frame(TABLE_NAME=rep("additional",8),
                   FIELD_NAME=rep("additional",8),
                   VALUESET_ITEM=c("BUN_SCR",
                                   paste0("day",0:6)),
                   VALUESET_ITEM_DESCRIPTOR=c("BUN SCr ratio",
                                              paste0("day",0:6)),
                   stringsAsFactors = F) %>%
  dplyr::mutate(VALUESET_ITEM2=VALUESET_ITEM)

##========== metadata set =============##
metadata<-i2b2_metadata %>%
  bind_rows(cdm_metadata) %>%
  bind_rows(ccs_ref) %>%
  bind_rows(add_ft) %>%
  unique

saveRDS(metadata,file="./data/meta_data/metadata.rda")

meta<-readRDS("./data_local/meta_data/metadata.rda")

