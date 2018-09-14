#### extract AKI cohort ####

extract_cohort<-function(conn,
                         remote_CDM=params$remote_CDM,
                         cdm_db_link,
                         cdm_db_name,
                         cdm_db_schema,
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  #global parameter
  DBMS_type<-attr(conn,"DBMS_type")
  if(!(DBMS_type %in% c("Oracle","tSQL","PostgreSQL"))){
    stop("DBMS_type=",DBMS_type,"is not currently supported \n(should be one of 'Oracle','tSQL','PostgreSQL', case-sensitive)")
  }

  #check if cdm_db_server has been specified when remote_CDM=T
  if(remote_CDM & is.null(cdm_db_link)){
    warning("must specify the cdm_db_link for CDM when remote_CDM=T!")
  }
  
  #execute the following sql snippets according to the specified order
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
                    cdm_db_link=cdm_db_link,
                    cdm_db_name=cdm_db_name,
                    cdm_db_schema=cdm_db_schema,
                    start_date=start_date,
                    end_date=end_date)
  
  #collect attrition info
  attrition<-dbGetQuery(conn,
                        parse_sql(paste0("./inst/",DBMS_type,
                                         "/consort_diagram.sql"))$statement)
  
  #query back final cohort
  tbl1_nm<-"AKI_onsets"
  if(DBMS_type=="tSQL"){
    tbl1_nm<-paste0("#",tbl1_nm) #special syntax for tSQL
  }
  aki_enc<-dbGetQuery(conn,
                      paste("select * from",tbl1_nm))
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      if(DBMS_type=="Oracle"){
        drop_temp<-paste("drop table",parse_out$tbl_out,"purge") # purge is only required in Oracle for completely destroying temporary tables
      }else{
        drop_temp<-paste("drop table",parse_out$tbl_out)
      }
      dbSendQuery(conn,drop_temp)
    }else{
      warning("no temporary table was created by this statment!")
    }
    if(verb){
      cat("temp table",toupper(parse_out$tbl_out),"dropped. \n")
    }
  }
  
  #output
  out<-list(aki_enc=aki_enc,
            attrition=attrition)
  
  return(out)
}

