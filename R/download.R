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
                    "stringr",
                    "openxlsx"))

# read-in and save Table1
tbl1<-readRDS("./data_local/data_raw/Table1.rda") %>%
  arrange(PATID,ENCOUNTERID) %>%
  left_join(readRDS("./data_local/data_raw/AKI_DEMO.rda") %>%
              dplyr::filter(key %in% c("AGE","SEX","RACE","HISPANIC")) %>%
              unique %>% spread(key,value) %>%
              dplyr::mutate(AGE=as.numeric(AGE)),
            by="ENCOUNTERID")
write.csv(tbl1,file="./data_local/data_csv/AKI_Table1.csv",row.names = F)
rm(tbl1); gc()

# read-in and save VITAL
vital<-readRDS("./data_local/data_raw/AKI_VITAL.rda") %>%
  filter(key %in% c("HT","WT","BMI","BP_SYSTOLIC","BP_DIASTOLIC")) %>%
  
  dplyr::mutate(time_of_day=format(strptime(timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")) %>%
  dplyr::select(ENCOUNTERID, key, value, dsa, time_of_day) %>% 
  arrange(ENCOUNTERID,key,dsa,time_of_day) %>%
  dplyr::mutate(value=as.numeric(value)) %>%
  dplyr::mutate(value=ifelse((key=="HT" & (value>95 | value<0))|
                             (key=="WT" & (value>1400 | value<0))|
                             (key=="BMI" & (value>100 | value<0))|
                             (key=="BP_DIASTOLIC" & (value>120 | value<40))|
                             (key=="BP_SYSTOLIC" & (value>210 | value<40)),NA,value)) %>%
  dplyr::filter(!is.na(value))
write.csv(vital,file="./data_local/data_csv/AKI_Vital.csv",row.names = F)
rm(vital); gc()


history<-readRDS("./data_local/data_raw/AKI_VITAL.rda") %>%
  filter(key %in% c("SMOKING","TOBACCO_TYPE")) %>%
  dplyr::select(ENCOUNTERID, key, value, dsa) %>% 
  arrange(ENCOUNTERID,key,dsa) %>%
  unique
write.csv(history,file="./data_local/data_csv/AKI_History.csv",row.names = F)
rm(history); gc()

# read-in and save lab
lab<-readRDS("./data_local/data_raw/AKI_LAB.rda") %>%
  dplyr::mutate(time_of_day=format(strptime(timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")) %>%
  dplyr::select(ENCOUNTERID, key, value, unit, dsa, time_of_day) %>% 
  arrange(ENCOUNTERID,key,dsa,time_of_day)
write.csv(lab,file="./data_local/data_csv/AKI_Lab.csv",row.names = F)
rm(lab); gc()


# read-in and save dx
dx<-readRDS("./data_local/data_raw/AKI_DX.rda") %>%
  dplyr::select(ENCOUNTERID,key,dsa) %>% 
  arrange(ENCOUNTERID,key,dsa) %>%
  unique
write.csv(dx,file="./data_local/data_csv/AKI_DX.csv",row.names = F)
rm(dx); gc()


# read-in and save px
px<-readRDS("./data_local/data_raw/AKI_PX.rda") %>%
  dplyr::select(ENCOUNTERID,key,dsa) %>% 
  arrange(ENCOUNTERID,key,dsa) %>%
  unique
write.csv(px,file="./data_local/data_csv/AKI_PX.csv",row.names = F)
rm(px); gc()


# read-in and save med
med<-readRDS("./data_local/data_raw/AKI_MED.rda") %>%
  dplyr::mutate(edsa=as.numeric(gsub(".*\\,","",dsa))) %>%
  dplyr::select(ENCOUNTERID, key, sdsa, edsa, RX_EXPOS, value, dsa) %>%
  arrange(ENCOUNTERID,sdsa,RX_EXPOS)
write.csv(med,file="./data_local/data_csv/AKI_Med.csv",row.names = F)
rm(med); gc()


# data dictionary
metadata<-read.csv("./data_local/meta_data/feature_dict.csv",stringsAsFactors = F)

metadata_tbl1<-metadata %>% 
  dplyr::filter(FIELD_NAME %in% c("AGE","SEX","RACE","HISPANIC")) %>%
  bind_rows(data.frame(FIELD_NAME=colnames(tbl1)[!colnames(tbl1) %in% c("PATID","ENCOUNTERID","AGE","HISPANIC","RACE","SEX")],
                       VALUESET_ITEM_DESCRIPTOR=c("Date of admission",
                                       "Date of discharge",
                                       "Numeric value of baseline serum creatinine",
                                       "Date of last serum creatinine record for non-AKI patients (if applicable)",
                                       "Number of days between last serum creatinine admission for non-AKI patients (if applicable)",
                                       "Numerical value of last normal serum creatinine for non-AKI patients",
                                       "Serum creatinine increase from baseline for non-AKI patients",
                                       "Date of AKI stage 1, i.e. AKI1, onset (if applicable)",
                                       "Number of days between AKI1 since admission (if applicable)",
                                       "Serum creatinine level at AKI1",
                                       "Serum creatinine increase from baseline at AKI2",
                                       "Date of AKI stage 1, i.e. AKI2, onset (if applicable)",
                                       "Number of days between AKI2 since admission (if applicable)",
                                       "Serum creatinine level at AKI2",
                                       "Serum creatinine increase from baseline at AKI3",
                                       "Date of AKI stage 1, i.e. AKI3, onset (if applicable)",
                                       "Number of days between AKI3 since admission (if applicable)",
                                       "Serum creatinine level at AKI3",
                                       "Serum creatinine increase from baseline at AKI3"),
                       stringsAsFactors=F))
            


metadata_vital<-data.frame(FIELD_NAME=c("key","value","dsa","time_of_day"),
                           FIELD_DESCRIPTOR=c("vital type",
                                              "value of the variable",
                                              "days of the measurement since admission",
                                              "measure time of the day (24hr:mm:ss)"),
                           stringsAsFactors = F) %>%
  bind_rows(metadata %>% 
              filter(FIELD_NAME %in% c("HT","WT","BP_SYSTOLIC","BP_DIASTOLIC")) %>%
              dplyr::mutate(KEY_ITEM=FIELD_NAME,
                            KEY_ITEM_DESCRIPTOR=FIELD_NAME,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::mutate(VALUESET_ITEM_DESCRIPTOR="Numeric value of the corresponding vital") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR,VALUESET_ITEM_DESCRIPTOR))



metadata_history<-data.frame(FIELD_NAME=c("key","value","dsa"),
                             FIELD_DESCRIPTOR=c("variable name",
                                                "value of the variable",
                                                "days of the measurement since admission"),
                             stringsAsFactors = F) %>%
  bind_rows(metadata %>% 
              filter(FIELD_NAME %in% c("SMOKING","TOBACCO_TYPE")) %>%
              dplyr::mutate(KEY_ITEM=FIELD_NAME,
                            KEY_ITEM_DESCRIPTOR=FIELD_NAME,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR,VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR))


metadata_lab<-data.frame(FIELD_NAME=c("key","value","dsa","time_of_day"),
                         FIELD_DESCRIPTOR=c("LOINC code for the lab",
                                            "value of the variable",
                                            "days of the measurement since admission",
                                            "measure time of the day (24hr:mm:ss)"),
                         stringsAsFactors = F) %>%
  bind_rows(metadata %>% 
              filter(FIELD_NAME=="LOINC") %>%
              dplyr::select(VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR) %>%
              dplyr::mutate(KEY_ITEM=VALUESET_ITEM,
                            KEY_ITEM_DESCRIPTOR=VALUESET_ITEM_DESCRIPTOR,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::mutate(VALUESET_ITEM_DESCRIPTOR="Numeric value of the corresponding lab with the specified unit") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR,VALUESET_ITEM))
  


metadata_dx<-data.frame(FIELD_NAME=c("key","value","dsa"),
                        FIELD_DESCRIPTOR=c("ccs diagnosis code",
                                           "value of the variable",
                                           "days of the measurement since admission (should be <0)"),
                        stringsAsFactors = F) %>% 
  bind_rows(metadata %>% 
              filter(FIELD_NAME=="DX_CCS") %>%
              dplyr::select(VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR) %>%
              dplyr::mutate(KEY_ITEM=VALUESET_ITEM,
                            KEY_ITEM_DESCRIPTOR=VALUESET_ITEM_DESCRIPTOR,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR) %>%
              arrange(as.numeric(KEY_ITEM)))
  
  

metadata_px<-data.frame(FIELD_NAME=c("key","value","dsa"),
                        FIELD_DESCRIPTOR=c("procedure code",
                                           "value of the variable",
                                           "days of the measurement since admission"),
                        stringsAsFactors = F) %>%
  bind_rows(metadata %>% 
              filter(FIELD_NAME=="PX") %>%
              dplyr::select(VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR) %>%
              dplyr::mutate(KEY_ITEM=VALUESET_ITEM,
                            KEY_ITEM_DESCRIPTOR=VALUESET_ITEM_DESCRIPTOR,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR))
  

metadata_med<-data.frame(FIELD_NAME=c("key","sdsa","edsa","RX_EXPOS","value","dsa"),
                         FIELD_DESCRIPTOR=c("medication name",
                                            "start day of a drug for a continuous time window",
                                            "end day of a drug for a continuous time window",
                                            "over drug exposure (RX_QUANTITY) during a continuous time window",
                                            "an array detailing the drug exposure each day within a continuous time window (daily exposure = RX_QUANTITY/RX_DAYS_SUPPLY, if applicable)",
                                            "an array detailing the day since admission for each exposure within a continuous time window"),
                         stringsAsFactors = F) %>%
  bind_rows(metadata %>% 
              dplyr::filter(FIELD_NAME=="RXNORM_CUI") %>%
              dplyr::select(VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR) %>%
              dplyr::mutate(KEY_ITEM=VALUESET_ITEM,
                            KEY_ITEM_DESCRIPTOR=VALUESET_ITEM_DESCRIPTOR,
                            FIELD_NAME="key",
                            FIELD_DESCRIPTOR="variable name") %>%
              dplyr::select(FIELD_NAME,FIELD_DESCRIPTOR,KEY_ITEM,KEY_ITEM_DESCRIPTOR) %>%
              dplyr::mutate(KEY_ITEM_DESCRIPTOR=ifelse(grepl("(\\:01)+$",KEY_ITEM),paste0(KEY_ITEM_DESCRIPTOR,":ordered"),
                                                       paste0(KEY_ITEM_DESCRIPTOR,":dispensed"))))
  
  

metadata<-list(Table1=metadata_tbl1,
               AKI_Vital=metadata_vital,
               AKI_History=metadata_history,
               AKI_Lab=metadata_lab,
               AKI_DX=metadata_dx,
               AKI_PX=metadata_px,
               AKI_Med=metadata_med)

write.xlsx(metadata,file="./data_local/data_csv/metadata.xlsx")


