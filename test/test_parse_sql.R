#### test parse_sql() ####
source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))



#Oracle
config_file_path<-"./config_Oracle_example.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
sql<-parse_sql(file_path="./inst/Oracle/cohort_initial.sql",
               remote_CDM=F,
               cdm_db_link=NULL,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")


#tSQL
config_file_path<-"./config_tSQL_example.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
sql<-parse_sql(file_path="./inst/tSQL/cohort_initial.sql",
               remote_CDM=F,
               cdm_db_link=NULL,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")

#passed!