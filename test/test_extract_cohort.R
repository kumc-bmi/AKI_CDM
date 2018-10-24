#### test execute_..._sql() ####
source("./R/util.R")
require_libraries(c("DBI",
                    "magrittr",
                    "tidyr",
                    "dplyr",
                    "stringr"))

params<-list(  DBMS_type="tSQl",
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

#statements to be tested
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


####batch snippets
execute_batch_sql(conn,statements,verb=T,
                  cdm_db_link=cdm_db_link,
                  cdm_db_name=cdm_db_name,
                  cdm_db_schema=cdm_db_schema,
                  start_date=start_date,
                  end_date=end_date)
#passed!


####consort table
#collect attrition info
sql<-parse_sql(paste0("./inst/",DBMS_type,"/consort_diagram.sql"))
attrition<-execute_single_sql(conn,
                              statement=sql$statement,
                              write=(sql$action=="write"),
                              table_name=toupper(sql$tbl_out))
#passed!
