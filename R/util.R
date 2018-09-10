##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list){
  #install missing packages
  new_packages<-package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)>0){
    install.packages(new_packages,repos = "http://cran.us.r-project.org")
  }
  
  for (lib in package_list) {
    library(lib, character.only=TRUE)
    cat("\n", lib, " loaded.", sep="")
  }
}

connect_to_db<-function(DBMS_type,config_file){
  if(DBMS_type=="Oracle"){
    require_libraries("ROracle")
    conn<-dbConnect(ROracle::Oracle(),
                    config_file$username,
                    config_file$password,
                    config_file$access)
  }else if(DBMS_type=="tSQL"){
    require_libraries("odbc")
    server<-gsub("/","",str_extract(config_file$access,"//.*(/)"))
    database<-str_extract("//host:port/sid","([^/]+$)")
    conn<-dbConnect(odbc::odbc(),
                    driver="SQL server",
                    uid=config_file$username,
                    pwd=config_file$password,
                    server=server,
                    database=database)
  }else if(DBMS_type=="PostgreSQL"){
    require_libraries("RPostgres")
    server<-gsub("/","",str_extract(config_file$access,"//.*(/)"))
    host<-gsub(":.*","",server)
    port<-gsub(".*:","",server)
    dbname<-str_extract("//host:port/sid","([^/]+$)")
    conn<-dbConnect(RPostgres::Postgres(),
                    host=host,
                    port=port,
                    dbname=dbname,
                    user=config_file$username,
                    password=config_file$password)
  }else{
    stop("the DBMS type is not currectly supported!")
  }
  attr(conn,"DBMS_type")<-DBMS_type
  return(conn)
}


## parse Oracle sql lines
parse_sql<-function(file_path,...){
  param_val<-list(...)
  param_val<-c(param_val[1], #cdm schema
               paste0(ifelse(param_val[2]!=" ","@",""),param_val[2]), #cdm server
               paste0("'",param_val[3],"'"), #start date (observation window)
               paste0("'",param_val[4],"'")) #end date (observation window)

  #read file
  con<-file(file_path,"r")
  
  #initialize string
  sql_string <- ""
  
  #intialize result holder
  params_ind<-FALSE
  tbl_out<-NULL
  action<-NULL
  
  while (TRUE){
    #parse the first line
    line <- readLines(con, n = 1)
    #check for endings
    if (length(line)==0) break
    #collect overhead info
    if(grepl("^(/\\*out)",line)){
      tbl_out<-trimws(gsub("(/\\*out\\:\\s)","",line),"both")
    }else if(grepl("^(/\\*action)",line)){
      action<-trimws(gsub("(/\\*action\\:\\s)","",line),"both")
    }else if(grepl("^(/\\*params)",line)){
      params<-gsub(",","",strsplit(trimws(gsub("(/\\*params\\:\\s)","",line),"both")," ")[[1]])
      params_ind<-TRUE
    }
    #remove the first line
    line<-gsub("\\t", " ", line)
    #translate comment symbol '--'
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    #attach new line
    if(!grepl("^(/\\*)",line)){
      sql_string <- paste(sql_string, line)
    }
  }
  close(con)
  
  #update parameters as needed
  if(params_ind){
    for(i in seq_along(params)){
      sql_string<-gsub(params[i],param_val[i],sql_string)
    }
    gsub("\\[\\ ]\\.","",sql_string) #for t-sql
    gsub("\\[@","[",sql_string) #for t-sql
  }

  out<-list(tbl_out=tbl_out,
            action=action,
            statement=sql_string)
  
  return(out)
}


## execute single sql snippet
execute_single_sql<-function(conn,statement,write,table_name){
  if(write){
    if(dbExistsTable(conn,table_name)){
      dbSendQuery(conn,paste("drop table",table_name,"purge"))
    }
    dbSendQuery(conn,statement)
  }
  
  if(!write){
    dbSendQuery(conn,statement)
  }
}


## execute multiple sql snippets
#---statements have to be in correct logical order
execute_batch_sql<-function(conn,statements,verb,...){
  for(i in seq_along(statements)){
    sql<-parse_sql(file_path=statements[i],...)
    execute_single_sql(conn,
                       statement=sql$statement,
                       write=(sql$action=="write"),
                       table_name=toupper(sql$tbl_out))
    if(verb){
      cat(statements[i],"has been executed and table",
          toupper(sql$tbl_out),"was created.\n")
    }
  }
}

## render report
render_report<-function(which_report="./report/AKI_CDM_EXT_VALID_p1_QA.Rmd",
                        DBMS_type){
  #to avoid <Error in unlockBinding("params", <environment>) : no binding for "params">
  #a hack to trick r thinking it's in interactive environment
  unlockBinding('interactive',as.environment('package:base'))
  assign('interactive',function() TRUE,envir=as.environment('package:base'))
  
  rmarkdown::render(input=which_report,
                    params=list(DBMS_type=DBMS_type),
                    output_dir="./output/",
                    knit_root_dir="../")
}


## compress dataframe into a condensed format
compress_df<-function(dat,tbl=c("demo","vital","lab","DRG","dx","px","med"),save=F){
  if(tbl=="demo"){
    tbl_zip<-dat %>% 
      filter(key %in% c("AGE","HISPANIC","RACE","SEX")) 
    
    idx_map<-tbl_zip %>% dplyr::select(key) %>%
      mutate(idx=paste0("demo",dense_rank(key))) %>% 
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      spread(key,value,fill=0) %>% #impute 0 for alignment
      unite("fstr",c("AGE","HISPANIC","RACE","SEX"),sep="_")
  }else if(tbl=="vital"){
    tbl_zip<-dat %>%
      filter(key %in% c("HT","WT","BMI",
                        "BP_SYSTOLIC","BP_DIASTOLIC",
                        "SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
      mutate(key=recode(key,
                        HT="1HT",
                        WT="2WT",
                        BMI="3BMI",
                        SMOKING="4SMOKING",
                        TOBACCO="5TOBACCO",
                        TOBACCO_TYPE="6TOBACCO_TYPE",
                        BP_SYSTOLIC="7BP_SYSTOLIC",
                        BP_DIASTOLIC="8BP_DIASTOLIC")) %>%
      arrange(key,dsa) 
    
    idx_map<-tbl_zip %>% dplyr::select(key) %>%
      mutate(idx=paste0("vital",dense_rank(key))) %>% 
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      unite("val_date",c("value","dsa"),sep=",") %>%
      group_by(PATID,ENCOUNTERID,key) %>%
      dplyr::summarize(fstr=paste(val_date,collapse=";")) %>%
      ungroup %>%
      spread(key,fstr,fill=0) %>% #impute 0 for alignment
      unite("fstr",c("1HT","2WT","3BMI",
                     "4SMOKING","5TOBACCO","6TOBACCO_TYPE",
                     "7BP_SYSTOLIC","8BP_DIASTOLIC"),sep="_")
  }else if(tbl=="lab"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("lab",dense_rank(key))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      unite("val_unit_date",c("value","unit","dsa"),sep=",") %>%
      group_by(PATID,ENCOUNTERID,idx) %>%
      dplyr::summarize(fstr=paste(val_unit_date,collapse=";")) %>%
      ungroup %>%
      unite("fstr2",c("idx","fstr"),sep=":") %>%
      group_by(PATID,ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr2,collapse="_")) %>%
      ungroup
  }else if(tbl=="DRG"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("dx",dense_rank(key2))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key2,idx) %>%
      unique %>% arrange(idx) %>%
      dplyr::rename(key=key2)
    
    tbl_zip %<>%
      group_by(PATID,ENCOUNTERID,key1,idx) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      unite("fstr",c("idx","dsa"),sep=":") %>%
      group_by(PATID,ENCOUNTERID,key1) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>%
      spread(key1,fstr,fill=0) %>%
      unite("fstr",c("ADMIT_DRG","COMMORB_DRG"),sep="|") %>%
      unique
  }else if(tbl=="dx"){
    tbl_zip<-dat %>%
      group_by(PATID,ENCOUNTERID,key) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      mutate(key=paste0("ccs",key))
    
    idx_map<-tbl_zip %>% dplyr::select(key) %>%
      mutate(idx=dense_rank(key)) %>% unique %>% arrange(idx)
    
    tbl_zip %<>%
      unite("fstr",c("key","dsa"),sep=":") %>%
      group_by(PATID,ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>% unique
  }else if(tbl=="px"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("px",dense_rank(key))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      group_by(PATID,ENCOUNTERID,idx) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      unite("fstr",c("idx","dsa"),sep=":") %>%
      group_by(PATID,ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>% unique
  }else if(tbl=="med"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("med",dense_rank(key))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      unite("val_date",c("value","dsa"),sep=",") %>%
      group_by(PATID,ENCOUNTERID,idx) %>%
      dplyr::summarize(fstr=paste(val_date,collapse=";")) %>%
      ungroup %>%
      unite("fstr2",c("idx","fstr"),sep=":") %>%
      group_by(PATID,ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr2,collapse="_")) %>%
      ungroup
  }else{
    warning("data elements not considered!")
  }
  if(save){
    save(tbl_zip,file=paste0("./data/",tbl,"_zip.Rdata"))
  }
  
  zip_out<-list(tbl_zip=tbl_zip,idx_map=idx_map)
  return(zip_out)
}
