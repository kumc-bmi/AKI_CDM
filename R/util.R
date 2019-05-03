##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list){
  #install missing packages
  install_pkg<-as.data.frame(installed.packages())
  new_packages<-package_list[!(package_list %in% install_pkg[which(install_pkg$LibPath==.libPaths()[1]),"Package"])]
  if(length(new_packages)>0){
    install.packages(new_packages,lib=.libPaths()[1],repos = "http://cran.us.r-project.org")
  }
  
  for (lib in package_list) {
    library(lib, character.only=TRUE,lib.loc=.libPaths()[1])
    cat("\n", lib, " loaded.", sep="")
  }
}

connect_to_db<-function(DBMS_type,driver_type=c("OCI","JDBC"),config_file){
  if(is.null(driver_type)){
    stop("must specify type of database connection driver!")
  }
  
  if(DBMS_type=="Oracle"){
    if(driver_type=="OCI"){
      require_libraries("ROracle")
      conn<-dbConnect(ROracle::Oracle(),
                      config_file$username,
                      config_file$password,
                      file.path(config_file$access,config_file$sid))
    }else if(driver_type=="JDBC"){
      require_libraries("RJDBC")
      # make sure ojdbc6.jar is in the AKI_CDM folder
      # Source: https://www.r-bloggers.com/connecting-r-to-an-oracle-database-with-rjdbc/
      drv<-JDBC(driverClass="oracle.jdbc.OracleDriver",
                classPath="./inst/ojdbc6.jar")
      url <- paste0("jdbc:oracle:thin:@", config_file$access,":",config_file$sid)
      conn <- RJDBC::dbConnect(drv, url, 
                               config_file$username, 
                               config_file$password)
    }else{
      stop("The driver type is not currently supported!")
    }

  }else if(DBMS_type=="tSQL"){
    require_libraries("RJDBC")
    # make sure sqljdbc.jar is in the AKI_CDM folder
    drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",
                classPath="./inst/sqljdbc.jar",
                identifier.quote="`")
    url <- paste0("jdbc:sqlserver:", config_file$access,
                  ";DatabaseName=",config_file$cdm_db_name,
                  ";username=",config_file$username,
                  ";password=",config_file$password)
    conn <- dbConnect(drv, url)
    
  }else if(DBMS_type=="PostgreSQL"){
    #not tested yet!
    require_libraries("RPostgres")
    server<-gsub("/","",str_extract(config_file$access,"//.*(/)"))
    host<-gsub(":.*","",server)
    port<-gsub(".*:","",server)
    conn<-dbConnect(RPostgres::Postgres(),
                    host=host,
                    port=port,
                    dbname=config_file$cdm_db_name,
                    user=config_file$username,
                    password=config_file$password)
  }else{
    stop("the DBMS type is not currectly supported!")
  }
  attr(conn,"DBMS_type")<-DBMS_type
  attr(conn,"driver_type")<-driver_type
  return(conn)
}


## parse Oracle sql lines
parse_sql<-function(file_path,...){
  param_val<-list(...)
  
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
      #output table name
      tbl_out<-trimws(gsub("(/\\*out\\:\\s)","",line),"both")
    }else if(grepl("^(/\\*action)",line)){
      #"write" or "query"(fetch) the output table
      action<-trimws(gsub("(/\\*action\\:\\s)","",line),"both")
    }else if(grepl("^(/\\*params)",line)){
      params_ind<-TRUE
      #breakdown global parameters
      params<-gsub(",","",strsplit(trimws(gsub("(/\\*params\\:\\s)","",line),"both")," ")[[1]])
      params_symbol<-params
      #normalize the parameter names
      params<-gsub("&&","",params) 
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
    #align param_val with params
    params_miss<-params[!(params %in% names(param_val))]
    for(j in seq_along(params_miss)){
      param_val[params_miss[j]]<-list(NULL)
    }
    param_val<-param_val[which(names(param_val) %in% params)]
    param_val<-param_val[order(names(param_val))]
    params_symbol<-params_symbol[order(params)]
    params<-params[order(params)]

    #substitube params_symbol by param_val
    for(i in seq_along(params)){
      sql_string<-gsub(params_symbol[i],
                       ifelse(is.null(param_val[[i]])," ",
                              ifelse(params[i]=="cdm_db_link",
                                     paste0("@",param_val[[i]]),
                                     ifelse(params[i] %in% c("start_date","end_date"),
                                            paste0("'",param_val[[i]],"'"),
                                            param_val[[i]]))),
                       sql_string)
    }
  }
  #clean up excessive "[ ]." or "[@" in tSQL when substitute value is NULL
  sql_string<-gsub("\\[\\ ]\\.","",sql_string)
  sql_string<-gsub("\\[@","[",sql_string)
  
  out<-list(tbl_out=tbl_out,
            action=action,
            statement=sql_string)
  
  return(out)
}


## execute single sql snippet
execute_single_sql<-function(conn,statement,write,table_name){
  DBMS_type<-attr(conn,"DBMS_type")
  driver_type<-attr(conn,"driver_type")
  
  if(write){
    #oracle and sql sever uses different connection driver and different functions are expected for sending queries
    #dbSendQuery silently returns an S4 object after execution, which causes error in RJDBC connection (for sql server)
    if(DBMS_type=="Oracle"){
      if(!(driver_type %in% c("OCI","JDBC"))){
        stop("Driver type not supported for ",DBMS_type,"!\n")
      }else{
        #drop existing tables, if applicable
        chk_exist<-dbGetQuery(conn,paste0("select tname from tab where tname ='",table_name,"'"))
        if(length(chk_exist$TNAME)>0){
          if(driver_type=="OCI"){
            dbSendQuery(conn,paste("drop table",table_name))
          }else{
            dbSendUpdate(conn,paste("drop table",table_name)) 
          }
        }
        
        if(driver_type=="OCI"){
          dbSendQuery(conn,statement)
        }else{
          dbSendUpdate(conn,statement) 
        }
      }
    }else if(DBMS_type=="tSQL"){
      if(driver_type=="JDBC"){
        #drop existing tables, if applicable
        # dbSendUpdate(conn,paste0("IF EXISTS (select * from dbo.sysobjects
        #                                      where id = object_id(N'dbo.",table_name,"') and 
        #                                      objectproperty(id, N'IsTable') = 1)",
        #                          "BEGIN ",paste("drop table",table_name)," END ",
        #                          "GO"))
        #write new tables
        dbSendUpdate(conn,statement)
      }else{
        stop("Driver type not supported for ",DBMS_type,"!\n")
      }
    }else{
      stop("DBMS type not supported!")
    }
  }else{
    dat<-dbGetQuery(conn,statement)
    return(dat)
  }
  cat("create temporary table: ", table_name, ".\n")
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


## clean up intermediate tables
drop_tbl<-function(conn,table_name){
  DBMS_type<-attr(conn,"DBMS_type")
  driver_type<-attr(conn,"driver_type")
  
  if(DBMS_type=="Oracle"){
    # purge is only required in Oracle for completely destroying temporary tables
    drop_temp<-paste("drop table",table_name,"purge") 
    if(driver_type=="OCI"){
      dbSendQuery(conn,drop_temp)
    }else if(driver_type=="JDBC"){
      dbSendUpdate(conn,drop_temp)
    }else{
      stop("Driver type not supported for ",DBMS_type,"!.\n")
    }
    
  }else if(DBMS_type=="tSQL"){
    drop_temp<-paste("drop table",table_name)
    if(driver_type=="JDBC"){
      dbSendUpdate(conn,drop_temp)
    }else{
      stop("Driver type not supported for ",DBMS_type,"!.\n")
    }
    
  }else{
    warning("DBMS type not supported!")
  }
}

## print link for LOINC code search result
get_loinc_ref<-function(loinc){
  #url to loinc.org 
  url<-paste0(paste0("https://loinc.org/",loinc))
  
  #return the link
  return(url)
}


## pring link for RXNORM codes search result
get_rxcui_nm<-function(rxcui){
  #url link to REST API
  rx_url<-paste0("https://rxnav.nlm.nih.gov/REST/rxcui/",rxcui,"/")
  
  #get and parse html object
  rxcui_obj <- getURL(url = rx_url)
  rxcui_content<-htmlParse(rxcui_obj)
  
  #extract name
  rxcui_name<-xpathApply(rxcui_content, "//body//rxnormdata//idgroup//name", xmlValue)
  
  if (length(rxcui_name)==0){
    rxcui_name<-NA
  }else{
    rxcui_name<-unlist(rxcui_name)
  }
  return(rxcui_name)
}

get_ndc_nm<-function(ndc){
  #url link to REST API
  rx_url<-paste0("https://ndclist.com/?s=",ndc)
  
  #get and parse html object
  rx_obj<-getURL(url = rx_url)
  if (rx_obj==""){
    rx_name<-NA
  }else{
    #extract name
    rx_content<-htmlParse(rx_obj)
    rx_attr<-xpathApply(rx_content, "//tbody//td[@data-title]",xmlAttrs)
    rx_name<-xpathApply(rx_content, "//tbody//td[@data-title]",xmlValue)[which(rx_attr=="Proprietary Name")]
    rx_name<-unlist(rx_name)
    
    if(length(rx_name) > 1){
      rx_name<-rx_url
    }
  }
  return(rx_name)
}


#ref: https://www.r-bloggers.com/web-scraping-google-urls/
google_code<-function(code,nlink=1){
  code_type<-ifelse(gsub(":.*","",code)=="CH","CPT",
                    gsub(":.*","",code))
  code<-gsub(".*:","",code)
  
  #search on google
  gu<-paste0("https://www.google.com/search?q=",code_type,":",code)
  html<-getURL(gu)
  
  #parse HTML into tree structure
  doc<-htmlParse(html)
  
  #extract url nodes using XPath. Originally I had used "//a[@href][@class='l']" until the google code change.
  attrs<-xpathApply(doc, "//h3//a[@href]", xmlAttrs)
  
  #extract urls
  links<-sapply(attrs, function(x) x[[1]])
  
  #only keep the secure links
  links<-links[grepl("(https\\:)+",links)]
  links<-gsub("(\\&sa=U).*$","",links)
  links<-paste0("https://",gsub(".*(https://)","",links))
  
  #free doc from memory
  free(doc)
  
  return(links[1])
}


## render report
render_report<-function(which_report="./report/AKI_CDM_EXT_VALID_p1_QA.Rmd",
                        DBMS_type,driver_type,remote_CDM=F,
                        start_date,end_date=as.character(Sys.Date())){
  
  # to avoid <Error in unlockBinding("params", <environment>) : no binding for "params">
  # a hack to trick r thinking it's in interactive environment --not work!
  # unlockBinding('interactive',as.environment('package:base'))
  # assign('interactive',function() TRUE,envir=as.environment('package:base'))
  
  rmarkdown::render(input=which_report,
                    params=list(DBMS_type=DBMS_type,
                                driver_type=driver_type,
                                remote_CDM=remote_CDM,
                                start_date=start_date,
                                end_date=end_date),
                    output_dir="./output/",
                    knit_root_dir="../")
}


## convert long mastrix to wide sparse matrix
long_to_sparse_matrix<-function(df,id,variable,val,binary=FALSE){
  if(binary){
    x_sparse<-with(df,
                   sparseMatrix(i=as.numeric(as.factor(get(id))),
                                j=as.numeric(as.factor(get(variable))),
                                x=1,
                                dimnames=list(levels(as.factor(get(id))),
                                              levels(as.factor(get(variable))))))
  }else{
    x_sparse<-with(df,
                   sparseMatrix(i=as.numeric(as.factor(get(id))),
                                j=as.numeric(as.factor(get(variable))),
                                x=ifelse(is.na(get(val)),1,as.numeric(get(val))),
                                dimnames=list(levels(as.factor(get(id))),
                                              levels(as.factor(get(variable))))))
  }
  
  return(x_sparse)
}



## compress dataframe into a condensed format
compress_df<-function(dat,tbl=c("DEMO","VITAL","LAB","DX","PX","MED","DRG"),save=F){
  if(tbl=="DEMO"){
    tbl_zip<-dat %>% 
      filter(key %in% c("AGE","HISPANIC","RACE","SEX")) 
    
    idx_map<-tbl_zip %>% dplyr::select(key) %>%
      mutate(idx=paste0("demo",dense_rank(key))) %>% 
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      spread(key,value,fill=0) %>% #impute 0 for alignment
      unite("fstr",c("AGE","HISPANIC","RACE","SEX"),sep="_")
  }else if(tbl=="VITAL"){
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
      mutate(add_time=difftime(timestamp,format(timestamp,"%Y-%m-%d"),units="mins")) %>%
      mutate(dsa=dsa+round(as.numeric(add_time)/(24*60),2)) %>%
      arrange(key,dsa) 
    
    idx_map<-tbl_zip %>% dplyr::select(key) %>%
      mutate(idx=paste0("vital",dense_rank(key))) %>% 
      unique %>% arrange(idx)
    
    tbl_zip %<>% unique %>%
      unite("val_date",c("value","dsa"),sep=",") %>%
      group_by(ENCOUNTERID,key) %>%
      dplyr::summarize(fstr=paste(val_date,collapse=";")) %>%
      ungroup %>%
      spread(key,fstr,fill=0) %>% #impute 0 for alignment
      unite("fstr",c("1HT","2WT","3BMI",
                     "4SMOKING","5TOBACCO","6TOBACCO_TYPE",
                     "7BP_SYSTOLIC","8BP_DIASTOLIC"),sep="_")
  }else if(tbl=="LAB"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("lab",dense_rank(key)))
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      arrange(ENCOUNTERID,idx,dsa) %>%
      unite("val_unit_date",c("value","unit","dsa"),sep=",") %>%
      group_by(ENCOUNTERID,idx) %>%
      dplyr::summarize(fstr=paste(val_unit_date,collapse=";")) %>%
      ungroup %>%
      unite("fstr2",c("idx","fstr"),sep=":") %>%
      group_by(ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr2,collapse="_")) %>%
      ungroup
  }else if(tbl=="DRG"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("dx",dense_rank(key2))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key2,idx) %>%
      unique %>% arrange(idx) %>% dplyr::rename(key=key2)
    
    tbl_zip %<>%
      group_by(ENCOUNTERID,key1,idx) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      unite("fstr",c("idx","dsa"),sep=":") %>%
      group_by(ENCOUNTERID,key1) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>%
      spread(key1,fstr,fill=0) %>%
      unite("fstr",c("ADMIT_DRG","COMMORB_DRG"),sep="|") %>%
      unique
  }else if(tbl=="DX"){
    tbl_zip<-dat %>%
      group_by(ENCOUNTERID,key) %>%
      arrange(dsa) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      mutate(idx=paste0("ccs",key))
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(key)
    
    tbl_zip %<>%
      unite("fstr",c("idx","dsa"),sep=":") %>%
      group_by(ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>% unique
  }else if(tbl=="PX"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("px",dense_rank(key))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      group_by(ENCOUNTERID,idx) %>%
      arrange(dsa) %>%
      dplyr::summarize(dsa=paste(dsa,collapse=",")) %>%
      ungroup %>%
      unite("fstr",c("idx","dsa"),sep=":") %>%
      group_by(ENCOUNTERID) %>%
      dplyr::summarize(fstr=paste(fstr,collapse="_")) %>%
      ungroup %>% unique
  }else if(tbl=="MED"){
    tbl_zip<-dat %>%
      mutate(idx=paste0("med",dense_rank(key))) 
    
    idx_map<-tbl_zip %>% dplyr::select(key,idx) %>%
      unique %>% arrange(idx)
    
    tbl_zip %<>%
      transform(value=strsplit(value,","),
                dsa=strsplit(dsa,",")) %>%
      unnest %>%
      unite("val_date",c("value","dsa"),sep=",") %>%
      group_by(ENCOUNTERID,idx) %>%
      dplyr::summarize(fstr=paste(val_date,collapse=";")) %>%
      ungroup %>%
      unite("fstr2",c("idx","fstr"),sep=":") %>%
      group_by(ENCOUNTERID) %>%
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


get_perf_summ<-function(pred,real,keep_all_cutoffs=F){
  # various performace table
  pred_obj<-ROCR::prediction(pred,real)
  
  prc<-performance(pred_obj,"prec","rec")
  roc<-performance(pred_obj,"sens","spec")
  nppv<-performance(pred_obj,"ppv","npv")
  pcfall<-performance(pred_obj,"pcfall")
  acc<-performance(pred_obj,"acc")
  fscore<-performance(pred_obj,"f")
  mcc<-performance(pred_obj,"phi")
  
  perf_at<-data.frame(cutoff=prc@alpha.values[[1]],
                      prec=prc@y.values[[1]],
                      rec_sens=prc@x.values[[1]],
                      stringsAsFactors = F) %>% 
    arrange(cutoff) %>%
    left_join(data.frame(cutoff=nppv@alpha.values[[1]],
                         ppv=nppv@y.values[[1]],
                         npv=nppv@x.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    dplyr::mutate(prec_rec_dist=abs(prec-rec_sens)) %>%
    left_join(data.frame(cutoff=fscore@x.values[[1]],
                         fscore=fscore@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=roc@alpha.values[[1]],
                         spec=roc@x.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    dplyr::mutate(Euclid_meas=sqrt((1-rec_sens)^2+(0-(1-spec))^2),
                  Youden_meas=rec_sens+spec-1) %>%
    left_join(data.frame(cutoff=pcfall@x.values[[1]],
                         pcfall=pcfall@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=acc@x.values[[1]],
                         acc=acc@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    left_join(data.frame(cutoff=mcc@x.values[[1]],
                         mcc=mcc@y.values[[1]],
                         stringsAsFactors = F),
              by="cutoff") %>%
    filter(prec > 0 & rec_sens > 0 & spec > 0) %>%
    group_by(cutoff) %>%
    dplyr::mutate(size=n()) %>%
    ungroup
  
  # performance summary
  lab1<-pred[real==1]
  lab0<-pred[real==0]
  pr<-pr.curve(scores.class0 = lab1,
               scores.class1 = lab0,curve=F)
  roc_ci<-pROC::ci.auc(real,pred)
  
  perf_summ<-data.frame(overall_meas=c("roauc_low",
                                       "roauc",
                                       "roauc_up",
                                       "opt_thresh",
                                       "opt_sens",
                                       "opt_spec",
                                       "opt_ppv",
                                       "opt_npv",
                                       "prauc1",
                                       "prauc2",
                                       "opt_prec",
                                       "opt_rec",
                                       "opt_fscore"),
                        meas_val=c(roc_ci[[1]],
                                   roc_ci[[2]],
                                   roc_ci[[3]],
                                   perf_at$cutoff[which.min(perf_at$Euclid_meas)],
                                   perf_at$rec_sens[which.min(perf_at$Euclid_meas)],
                                   perf_at$spec[which.min(perf_at$Euclid_meas)],
                                   perf_at$ppv[which.min(perf_at$Euclid_meas)],
                                   perf_at$npv[which.min(perf_at$Euclid_meas)],
                                   pr$auc.integral,
                                   pr$auc.davis.goadrich,
                                   perf_at$prec[which.min(perf_at$prec_rec_dist)],
                                   perf_at$rec_sens[which.min(perf_at$prec_rec_dist)],
                                   perf_at$fscore[which.min(perf_at$prec_rec_dist)]),
                        stringsAsFactors = F) %>%
    bind_rows(perf_at %>% 
                dplyr::summarize(prec_m=mean(prec,na.rm=T),
                                 sens_m=mean(rec_sens,na.rm=T),
                                 spec_m=mean(spec,na.rm=T),
                                 ppv_m=mean(ppv,na.rm=T),
                                 npv_m=mean(npv,na.rm=T),
                                 acc_m=mean(acc,na.rm=T),
                                 fscore_m=mean(fscore,na.rm=T),
                                 mcc_m=mean(mcc,na.rm=T)) %>%
                gather(overall_meas,meas_val))
  
  out<-list(perf_summ=perf_summ)
  if(keep_all_cutoffs){
    out$perf_at<-perf_at
  }
  
  return(out)
}

get_calibr<-function(pred,real,n_bin=20){
  calib<-data.frame(pred=pred,
                    y=real) %>%
    arrange(pred) %>%
    dplyr::mutate(pred_bin = cut(pred,
                                 breaks=unique(quantile(pred,0:(n_bin)/(n_bin))),
                                 include.lowest=T,
                                 labels=F)) %>%
    ungroup %>% group_by(pred_bin) %>%
    dplyr::summarize(expos=n(),
                     bin_lower=min(pred),
                     bin_upper=max(pred),
                     bin_mid=median(pred),
                     y_agg = sum(y),
                     pred_p = mean(pred)) %>%
    dplyr::mutate(y_p=y_agg/expos) %>%
    dplyr::mutate(binCI_lower = pmax(0,pred_p-1.96*sqrt(y_p*(1-y_p)/expos)),
                  binCI_upper = pred_p+1.96*sqrt(y_p*(1-y_p)/expos))
  
  return(calib)
}
