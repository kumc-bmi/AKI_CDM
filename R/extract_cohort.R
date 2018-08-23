#### extract AKI cohort ####

extract_cohort<-function(conn,
                         cdm_db_schema="PCORNET_CDM_C4R3",
                         oracle_temp_schema=cdm_db_schema,
                         start_date="2010-01-01",
                         end_date="current date"){
  
  #write intermediate tables on Oracle
  sql<-paste0(parse_sql("./inst/create_initial_cohort.sql"),"  ",
              parse_sql("./inst/collect_SCr.sql"),"  ",
              parse_sql("./inst/exclude.sql"),"  ",
              parse_sql("./inst/get_baseline_SCr.sql"),"  ",
              parse_sql("./inst/create_eligib_cohort.sql"),"  ",
              parse_sql("./inst/stage_AKI.sql"))
  sql<-gsub("&&PCORNET_CDM",cdm_db_schema,sql)
  sql<-gsub("&&start_date",start_date,sql)
  sql<-gsub("&&end_date",end_date,sql)
  dbSendQuery(conn,sql,schema=oracle_temp_schema)

  #collect stats
  attrition<-dbGetQuery(conn,
                        parse_sql("./inst/attrition_diagram.sql"))
  
  #final cohort
  aki_enc<-dbGetQuery(conn,
                      parse_sql("./inst/get_final_cohort.sql"))
  
  out<-list(aki_enc=aki_enc,
            attrition=attrition)
  
  return(out)
}

