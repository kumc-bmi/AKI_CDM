#### test ####
sql<-parse_sql(file_path="./inst/cohort_enc_SCr.sql",
               cdm_db_schema="PCORNET_CDM_C4R3",
               start_date="2010-01-01",
               end_date="2018-12-31")


execute_single_sql(conn,
                   statement=sql$statement,
                   write=(sql$action=="write"),
                   table_name=toupper(sql$tbl_out))


sql<-parse_sql("./inst/attrition_diagram.sql")
