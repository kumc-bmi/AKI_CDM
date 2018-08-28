#### extract AKI cohort ####

extract_cohort<-function(conn,
                         cdm_db_schema,
                         oracle_temp_schema=cdm_db_schema,
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  
  #execute the following sql snippets on Oracle
  statements<-c("./inst/cohort_initial.sql",
                "./inst/cohort_all_SCr.sql",
                "./inst/cohort_enc_SCr.sql",
                "./inst/cohort_baseline_SCr.sql",
                "./inst/cohort_exclude.sql",
                "./inst/cohort_eligib.sql",
                "./inst/cohort_AKI_staging.sql",
                "./inst/cohort_final.sql")

  execute_batch_sql(conn,statements,verb,
                    oracle_temp_schema=cdm_db_schema,
                    start_date="2010-01-01",
                    end_date="2018-12-31")

  #collect attrition info
  attrition<-dbGetQuery(conn,
                        parse_sql("./inst/consort_diagram.sql")$statement)
  
  #query back final cohort
  aki_enc<-dbGetQuery(conn,
                      "select * from AKI_onsets")
  
  #output
  out<-list(aki_enc=aki_enc,
            attrition=attrition)
  
  return(out)
}

