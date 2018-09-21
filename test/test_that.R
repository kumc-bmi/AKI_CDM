#source utility functions
source("./R/util.R")
source("./R/extract_cohort.R")
source("./R/viz.R")

#load libraries
require_libraries(c("DBI",
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr",
                    "kableExtra",
                    "ggplot2",
                    "ggrepel",
                    "RCurl",
                    "XML",
                    "openxlsx"))

params<-list(  DBMS_type="Oracle",
               remote_CDM=FALSE)

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,config_file)
DBMS_type<-attr(conn,"DBMS_type")


#### test sql parsor ####
sql<-parse_sql(file_path="./inst/Oracle/cohort_initial.sql",
               remote_CDM=F,
               cdm_db_link=config_file$cdm_db_link,
               cdm_db_name=config_file$cdm_db_name,
               cdm_db_schema=config_file$cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")

sql<-parse_sql(file_path="./inst/tSQL/cohort_initial.sql",
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema,
               start_date="2010-01-01",
               end_date="2018-12-31")

sql<-parse_sql(file_path="./inst/Oracle/collect_demo.sql",
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema)

sql<-parse_sql(file_path="./inst/Oracle/collect_demo.sql",
               cdm_db_link="remote_link",
               cdm_db_name=NULL,
               cdm_db_schema=cdm_db_schema)

sql<-parse_sql(file_path="./inst/tSQL/collect_demo.sql",
               cdm_db_name=db_params$cdm_db_name,
               cdm_db_schema=db_params$cdm_db_schema)


#### test extract_cohort ####
remote_CDM=params$remote_CDM
cdm_db_link=config_file$cdm_db_link
cdm_db_name=config_file$cdm_db_name
cdm_db_schema=config_file$cdm_db_schema
start_date="2010-01-01"
end_date="2018-12-31"
verb=F

statements<-paste0(
  paste0("./inst/",DBMS_type),
  c("/cohort_initial.sql",
    "/cohort_all_SCr.sql",
    "/cohort_enc_SCr.sql",
    "/cohort_baseline_SCr.sql",
    "/cohort_exclude.sql",
    "/cohort_eligib.sql",
    "/cohort_AKI_staging.sql",
    "/cohort_final.sql")
)

# test
sql<-parse_sql(file_path=statements[5],
               cdm_db_link=cdm_db_link,
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema,
               start_date=start_date,
               end_date=end_date)

execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))


