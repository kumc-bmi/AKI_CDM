#### extract AKI cohort ####

extract_cohort<-function(conn,
                         remote_CDM=params$remote_CDM,
                         cdm_db_link=NULL,
                         cdm_db_name,
                         cdm_db_schema,
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  
  #check if DBMS type is currently supported
  if(!(attr(conn,"DBMS_type") %in% c("Oracle","tSQL","PostgreSQL"))){
    stop("DBMS_type=",attr(conn,"DBMS_type"),"is not currently supported \n(should be one of 'Oracle','tSQL','PostgreSQL', case-sensitive)")
  }

  #check if cdm_db_server has been specified when remote_CDM=T
  if(remote_CDM & is.null(cdm_db_link)){
    warning("must specify the cdm_db_link for CDM when remote_CDM=T!")
  }
  
  #execute(write) the following sql snippets according to the specified order
  statements<-paste0(
    paste0("./inst/",attr(conn,"DBMS_type")),
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
  sql<-parse_sql(paste0("./inst/",attr(conn,"DBMS_type"),"/consort_diagram.sql"))
  attrition<-execute_single_sql(conn,
                                statement=sql$statement,
                                write=(sql$action=="write"),
                                table_name=toupper(sql$tbl_out))

  #read Table1
  tbl1<-parse_sql(statements[length(statements)])$tbl_out
  aki_enc<-dbGetQuery(conn,paste("select * from",tbl1))
  
  #clean out intermediate tables
  for(i in 1:(length(statements)-1)){
    parse_out<-parse_sql(statements[i])
    if(parse_out$action=="write"){
      drop_tbl(conn,toupper(parse_out$tbl_out))
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

