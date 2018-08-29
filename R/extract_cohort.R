#### extract AKI cohort ####

extract_cohort<-function(conn,
                         cdm_db_schema,
                         oracle_temp_schema,
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  
  #make sure we are on the writable server: oracle_temp_schema
  dbSendQuery(conn,
              paste("alter session set current_schema =",
                    oracle_temp_schema))
  cat("R is currently connected to schema",oracle_temp_schema,".\n")
  
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
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      drop_temp<-paste("drop table",parse_out$tbl_out,"purge")
      dbSendQuery(conn,drop_temp)
    }else{
      warning("no temporary table was created by this statment!")
    }
  }

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

