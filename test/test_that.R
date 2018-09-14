#### test sql parsor ####
config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)

cdm_db_name<-config_file$cdm_db_name
cdm_db_schema<-config_file$cdm_db_schema
temp_db_schema<-config_file$temp_db_schema


sql<-parse_sql(file_path="./inst/Oracle/cohort_initial.sql",
               cdm_db_name=cdm_db_name,
               cdm_db_schema=cdm_db_schema)

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

