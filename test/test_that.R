#### test sql parsor ####
config_file_path<-"../config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
sql<-parse_sql(file_path="./inst/cohort_initial.sql",
               cdm_db_schema=config_file$cdm_db_schema,
               cdm_db_server=config_file$cdm_db_server,
               start_date="2010-01-01",
               end_date="2018-12-31")

sql<-parse_sql("./inst/attrition_diagram.sql")


