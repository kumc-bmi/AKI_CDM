#### extract AKI cohort ####

extract_cohort<-function(conn,
                         oracle_temp_schema,
                         cdm_db_schema,
                         same_server=T,
                         cdm_db_server=" ",
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  
  DBMS_type<-attr(conn,"DBMS_type")
  
  #make sure we are on the writable server: oracle_temp_schema
  if(DBMS_type=="Oracle"){
    dbSendQuery(conn,
                paste("alter session set current_schema =",
                      oracle_temp_schema))
  }else if(DBMS_type=="tSQL"){
    dbSendQuery(conn,
                paste("alter user",
                      oracle_temp_schema))
  }else if(DBMS_type=="PostgreSQL"){
    dbSendQuery(conn,
                paste("SET search_path TO",
                      oracle_temp_schema))
  }else{
    stop("the DBMS type is not currectly supported!")
  }

  cat("R is currently connected to schema",oracle_temp_schema,".\n")
  
  #check if cdm_db_server has been specified
  if(!same_server & cdm_db_server==" "){
    warning("must specify the db server name for CDM when same_server=F!")
  }
  
  #execute the following sql snippets on Oracle
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
  
  execute_batch_sql(conn,statements,verb,
                    cdm_db_schema=cdm_db_schema,
                    cdm_db_server=cdm_db_server,
                    start_date="2010-01-01",
                    end_date="2018-12-31")
  
  #collect attrition info
  attrition<-dbGetQuery(conn,
                        parse_sql(paste0("./inst/",DBMS_type,
                                         "/consort_diagram.sql"))$statement)
  
  #query back final cohort
  aki_enc<-dbGetQuery(conn,
                      "select * from AKI_onsets")
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      drop_temp<-paste("drop table",parse_out$tbl_out,"purge")
      dbSendQuery(conn,drop_temp)
    }else{
      warning("no temporary table was created by this statment!")
    }
    if(verb){
      cat("temp table",toupper(parse_out$tbl_out),"purged.\n")
    }
  }
  
  #output
  out<-list(aki_enc=aki_enc,
            attrition=attrition)
  
  return(out)
}

