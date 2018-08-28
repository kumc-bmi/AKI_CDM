##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list){
  #install missing packages
  new_packages<-package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)>0){
    install.packages(new_packages)
  }
  
  for (lib in package_list) {
    library(lib, character.only=TRUE)
    cat("\n", lib, " loaded.", sep="")
  }
}


## parse Oracle sql lines
parse_sql<-function(file_path,...){
  param_val<-list(...)
  param_val<-c(param_val[1],
               paste0("'",param_val[2],"'"),
               paste0("'",param_val[3],"'"))
  
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
  
  #update parameters, if applicable
  if(params_ind){
    for(i in seq_along(params)){
      sql_string<-gsub(params[i],param_val[i],sql_string)
    }
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

## compress data frame
compress_df<-function(dat,tbl=c("demo",),save=F){
  if(tbl=="demo"){
    tbl_zip<-dat %>% 
      filter(key %in% c("AGE","HISPANIC","RACE","SEX")) %>%
      group_by(PATID,ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(value,collapse="_")) %>%
      ungroup
  }
  
  
  if(save)
    save(tbl_zip,file=paste0("./data/",tbl,"_zip.Rdata"))
}
