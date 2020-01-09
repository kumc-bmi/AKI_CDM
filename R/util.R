##---------------------------helper functions--------------------------------------##
## install (if needed) and require packages
require_libraries<-function(package_list,verb=T){
  for (lib in package_list) {
    chk_install<-!(lib %in% installed.packages()[,"Package"])
    if(chk_install){
      install.packages(lib)
    }
    library(lib, character.only=TRUE,lib.loc=.libPaths())
    if(verb){
      cat("\n", lib, " loaded.", sep="") 
    }
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

extract_cohort<-function(conn,
                         cdm_db_name,
                         cdm_db_schema,
                         start_date="2010-01-01",
                         end_date="2018-12-31",
                         verb=T){
  
  #check if DBMS type is currently supported
  if(!(attr(conn,"DBMS_type") %in% c("Oracle","tSQL","PostgreSQL"))){
    stop("DBMS_type=",attr(conn,"DBMS_type"),"is not currently supported \n(should be one of 'Oracle','tSQL','PostgreSQL', case-sensitive)")
  }
  
  #execute(write) the following sql snippets according to the specified order
  statements<-paste0(
    paste0("./src/",attr(conn,"DBMS_type")),
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
                    cdm_db_name=cdm_db_name,
                    cdm_db_schema=cdm_db_schema,
                    start_date=start_date,
                    end_date=end_date)
  
  #collect attrition info
  sql<-parse_sql(paste0("./src/",attr(conn,"DBMS_type"),"/consort_diagram.sql"))
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


consort_diag<-function(consort_tbl){
  require_libraries("diagram")
  tbl<-data.frame(CNT_TYPE=c("Initial",
                             "Has_at_least_2_SCr",
                             "Initial_GFR_below_15",
                             "RRT_within_48hr",
                             "Burn_patients",
                             "Pre_ESRD",
                             "Pre_RRT",
                             "Total",
                             "nonAKI",
                             "AKI1",
                             "nonAKI_to_AKI2",
                             "AKI1_to_AKI2",
                             "nonAKI_to_AKI3",
                             "nonAKI_to_AKI2_to_AKI3",
                             "AKI1_to_AKI2_to_AKI3"),
                  label_txt=c("Inpatient visit with LOS >= 2\nand of age >= 18",
                              "Has at least 2 SCr record",
                              "Excluded: Initial eGFR below 15",
                              "Excluded: RRT with 48 hours since \nadmission",
                              "Excluded: Burn Patients",
                              "Excluded: Pre-existance of \nESRD",
                              "Excluded: Pre-existance of \ndialysis and renal transplantation",
                              "Total eligible encounters",
                              "Non-AKI",
                              "AKI1",
                              "AKI2",
                              "AKI1 to AKI2",
                              "AKI3",
                              "AKI2 to AKI3",
                              "AKI1 to AKI2 to AKI3"),
                  stringsAsFactors=F) %>%
    left_join(consort_tbl, by="CNT_TYPE") %>%
    replace_na(list(ENC_CNT=0)) %>%
    mutate(cnt_ref=ifelse(CNT_TYPE %in% c("Initial","Has_at_least_1_SCr","Total"),ENC_CNT,NA)) %>%
    fill(cnt_ref,.direction="down") %>%
    mutate(cnt_ref=ifelse(CNT_TYPE=="Has_at_least_1_SCr",lag(cnt_ref,n=1L),cnt_ref)) %>%
    mutate(ENC_PROP=round(ENC_CNT/cnt_ref,4)) %>%
    mutate(label_val=paste0("(",ENC_CNT,",",ENC_PROP*100,"%)")) %>%
    mutate(label=paste(label_txt,"\n",label_val)) %>%
    mutate(node_id=c(2,5,7,9,10,12,13,17,18,22,23,25,24,26,28))
  
  #prepare canvas
  par(mfrow=c(1,1))
  par(mar=c(0,0,0,0))
  openplotmat()
  
  ##number of elements per row
  elpos<-coordinates(rep(3,10))
  fromto<-matrix(ncol=2,byrow=T,
                 c(2,5,
                   5,8,
                   8,7,
                   8,9,
                   8,11,
                   11,10,
                   11,12,
                   11,14,
                   14,13,
                   14,17,
                   17,18,
                   17,20,
                   20,19,
                   20,21,
                   19,22,
                   20,23,
                   21,24,
                   22,25,
                   23,26,
                   25,28
                 ))
  ##draw arrows
  arrpos <- matrix(ncol = 2, nrow = nrow(fromto))
  for (i in 1:nrow(fromto)){
    arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                  from = elpos[fromto[i, 1], ],
                                  lwd = 1, arr.pos = 0.6, arr.length = 0.3)
  }
  
  ##draw nodes
  for(i in 1:nrow(tbl)){
    textrect(elpos[tbl$node_id[i],],
             radx=0.15,
             rady=0.05,
             lab=tbl$label[i],
             font=4,
             cex=0.7)
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
                        DBMS_type,driver_type,
                        start_date,end_date=as.character(Sys.Date())){
  
  # to avoid <Error in unlockBinding("params", <environment>) : no binding for "params">
  # a hack to trick r thinking it's in interactive environment --not work!
  # unlockBinding('interactive',as.environment('package:base'))
  # assign('interactive',function() TRUE,envir=as.environment('package:base'))
  
  rmarkdown::render(input=which_report,
                    params=list(DBMS_type=DBMS_type,
                                driver_type=driver_type,
                                start_date=start_date,
                                end_date=end_date),
                    output_dir="./output/",
                    knit_root_dir="../")
}


#### survival-like data format transformation ####
format_data<-function(dat,type=c("demo","vital","lab","dx","px","med"),pred_end){
  if(type=="demo"){
    #demo has to be unqiue for each encounter
    dat_out<-dat %>%
      filter(key %in% c("AGE","SEX","RACE","HISPANIC")) %>%
      group_by(ENCOUNTERID,key) %>%
      top_n(n=1L,wt=value) %>% #randomly pick one if multiple entries exist
      ungroup %>% 
      mutate(cat=value,dsa=-1,key_cp=key,
             value2=ifelse(key=="AGE",value,"1")) %>%
      unite("key2",c("key_cp","cat"),sep="_") %>%
      mutate(key=ifelse(key=="AGE",key,key2),
             value=as.numeric(value2)) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
  }else if(type=="vital"){
    dat_out<-c()
    
    #multiple smoking status is resolved by using the most recent record
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  arrange(value) %>% dplyr::slice(1:1) %>%
                  ungroup %>%
                  mutate(cat=value,dsa=-1,key_cp=key,value=1) %>%
                  unite("key",c("key_cp","cat"),sep="_") %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa))
    
    
    #multiple ht,wt,bmi resolved by taking median
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("HT","WT","BMI")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  mutate(value=ifelse((key=="HT" & (value>95 | value<=0))|
                                        (key=="WT" & (value>1400 | value<=0))|
                                        (key=="BMI" & (value>70 | value<=0)),NA,value)) %>%
                  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
                  ungroup %>% mutate(dsa=-1))
    
    #multiple bp are aggregated by taking: lowest & slope
    bp<-dat %>% dplyr::select(-PATID) %>%
      filter(key %in% c("BP_DIASTOLIC","BP_SYSTOLIC")) %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=ifelse((key=="BP_DIASTOLIC" & (value>120 | value<40))|
                            (key=="BP_SYSTOLIC" & (value>210 | value<40)),NA,value)) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::mutate(value_imp=median(value,na.rm=T)) %>%
      ungroup 
    
    bp %<>%
      filter(!is.na(value_imp)) %>%
      mutate(imp_ind=ifelse(is.na(value),1,0)) %>%
      mutate(value=ifelse(is.na(value),value_imp,value)) %>%
      dplyr::select(-value_imp) 
    
    bp %<>% dplyr::select(-imp_ind)
    #--minimal bp
    bp_min<-bp %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value_lowest=min(value,na.rm=T)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_min")) %>%
      dplyr::rename(value=value_lowest)
    
    #--trend of bp
    bp_slp_eligb<-bp %>%
      mutate(add_time=difftime(strptime(timestamp,"%Y-%m-%d %H:%M:%S"),strptime(timestamp,"%Y-%m-%d"),units="mins")) %>%
      mutate(timestamp=round(as.numeric(add_time)/60,2)) %>% #coefficient represents change per hour
      dplyr::select(-add_time) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::mutate(df=length(unique(timestamp))-1) %>%
      dplyr::mutate(sd=ifelse(df>0,sd(value),0))
    
    bp_slp_obj<-bp_slp_eligb %>%
      filter(df > 1 & sd >= 1e-2) %>%
      do(fit_val=glm(value ~ timestamp,data=.))
    
    bp_slp<-tidy(bp_slp_obj,fit_val) %>%
      filter(term=="timestamp") %>%
      dplyr::rename(value=estimate) %>%
      ungroup %>%
      mutate(value=ifelse(p.value>0.5 | is.nan(p.value),0,value)) %>%
      dplyr::select(ENCOUNTERID,key,dsa,value) %>%
      bind_rows(bp_slp_eligb %>% 
                  filter(df<=1 | sd < 1e-2) %>% mutate(value=0) %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                  ungroup %>% unique) %>%
      bind_rows(bind_rows(bp_slp_eligb %>% 
                            filter(df==1 & sd >= 1e-2) %>% 
                            mutate(value=round((max(value)-min(value))/(max(timestamp)-min(timestamp)),2)) %>%
                            dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                            ungroup %>% unique)) %>%
      mutate(key=paste0(key,"_slope"))
    
    #--stack bp
    bp<-bp_min %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(bp_slp %>% 
                  dplyr::select(ENCOUNTERID,key,value,dsa))
    
    #all vitals
    dat_out %<>%
      mutate(dsa=-1) %>% bind_rows(bp)
    
    #clean out some memories
    rm(bp,bp_min,bp_slp_eligb,bp_slp_obj,bp_slp)
    gc()
    
  }else if(type=="lab"){
    #multiple same lab on the same day will be resolved by taking the average
    dat_out<-dat %>%
      filter(key != "NI") %>%
      mutate(key_cp=key,unit_cp=unit) %>%
      unite("key_unit",c("key_cp","unit_cp"),sep="@") %>%
      group_by(ENCOUNTERID,key,unit,key_unit,dsa) %>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      ungroup
    
    #calculated new features: BUN/SCr ratio (same-day)
    bun_scr_ratio<-dat_out %>% 
      mutate(key_agg=case_when(key %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
                                          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
                                          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9') ~ "SCR",
                               key %in% c('12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
                                          '11065-0','3094-0','35234-4','14937-7') ~ "BUN",
                               key %in% c('3097-3','44734-2') ~ "BUN_SCR")) %>% #not populated
      filter((toupper(unit) %in% c("MG/DL","MG/MG")) & 
               (key_agg %in% c("SCR","BUN","BUN_SCR"))) %>%
      group_by(ENCOUNTERID,key_agg,dsa) %>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      ungroup %>%
      spread(key_agg,value) %>%
      filter(!is.na(SCR)&!is.na(BUN)) %>%
      mutate(BUN_SCR = round(BUN/SCR,2)) %>%
      mutate(key="BUN_SCR") %>%
      dplyr::rename(value=BUN_SCR) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
    dat_out %<>% bind_rows(bun_scr_ratio)
    
    #engineer new features: change of lab from last collection
    lab_delta_eligb<-dat_out %>%
      group_by(ENCOUNTERID,key) %>%
      dplyr::mutate(lab_cnt=sum(dsa<=pred_end)) %>%
      ungroup %>%
      group_by(key) %>%
      dplyr::summarize(p5=quantile(lab_cnt,probs=0.05,na.rm=T),
                       p25=quantile(lab_cnt,probs=0.25,na.rm=T),
                       med=median(lab_cnt,na.rm=T),
                       p75=quantile(lab_cnt,probs=0.75,na.rm=T),
                       p95=quantile(lab_cnt,probs=0.95,na.rm=T))
    
    #--collect changes of lab only for those are regularly repeated (floor(pred_end/2))
    freq_lab<-lab_delta_eligb %>% filter(med>=(floor(pred_end/2)))
    if(nrow(freq_lab)>0){
      lab_delta<-dat_out %>%
        semi_join(freq_lab,by="key")
      
      dsa_rg<-seq(0,pred_end)
      
      lab_delta %<>%
        group_by(ENCOUNTERID,key) %>%
        dplyr::mutate(dsa_max=max(dsa)) %>%
        filter(dsa<=dsa_max) %>%
        arrange(dsa) %>%
        dplyr::mutate(value_lag=lag(value,n=1L,default=NA)) %>%
        ungroup %>%
        filter(!is.na(value_lag)) %>%
        mutate(value=value-value_lag,
               key=paste0(key,"_change")) %>%
        dplyr::select(ENCOUNTERID,key,value,dsa) %>%
        unique
      
      dat_out %<>% bind_rows(lab_delta)
    }
  }else if(type == "dx"){
    #multiple records resolved as "present (1) or absent (0)"
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value=(n() >= 1)*1) %>%
      ungroup %>%
      group_by(ENCOUNTERID,key) %>%
      top_n(n=1L,wt=dsa) %>%
      ungroup %>% 
      mutate(key=as.character(key)) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
  }else if(type == "px"){
    #multiple records resolved as "present (1) or absent (0)"
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value=(n() >= 1)*1) %>%
      ungroup %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
  }else if(type=="med"){
    #multiple records accumulated
    dat_out<-dat %>%
      group_by(ENCOUNTERID,key) %>%
      arrange(dsa) %>%
      dplyr::mutate(value=cumsum(value)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_cum")) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(dat %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                  unique)
  }
  
  return(dat_out)
}

#tw should be the same time unit as dsa
get_dsurv_temporal<-function(dat,censor,tw,pred_in_d=1,carry_over=T){
  y_surv<-c()
  X_surv<-c()
  for(t in tw){
    #stack y
    censor_t<-censor %>%
      mutate(pred_pt=case_when(dsa_y >= t ~ t,
                               dsa_y <  t ~ NA_real_),
             y_ep=case_when(dsa_y == t ~ y,
                            dsa_y >  t ~ pmax(0,y-1),
                            dsa_y <  t ~ NA_real_)) %>%
      filter(!is.na(pred_pt)) %>%
      group_by(ENCOUNTERID) %>%
      arrange(desc(pred_pt),desc(y_ep)) %>%
      dplyr::slice(1:1) %>%
      ungroup %>%
      mutate(dsa_y=pred_pt,y=y_ep) %>%
      dplyr::select(-pred_pt,-y_ep)
    
    y_surv %<>% 
      bind_rows(censor_t %>%
                  dplyr::select(ENCOUNTERID,dsa_y,y))
    
    #stack x
    if(carry_over){
      X_surv %<>% 
        bind_rows(dat %>% left_join(censor_t,by="ENCOUNTERID") %>%
                    filter(dsa < dsa_y-(pred_in_d-1)) %>% # prediction point is at least "pred_in_d" days before endpoint
                    group_by(ENCOUNTERID,key) %>%
                    top_n(n=1,wt=dsa) %>% # take latest value (carry over)
                    ungroup %>%
                    dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value) %>%
                    bind_rows(censor_t %>% 
                                mutate(dsa=dsa_y-1,
                                       key=paste0("day",(dsa_y-1)),
                                       value=1) %>%
                                dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value)))
    }else{
      X_surv %<>% 
        bind_rows(dat %>% left_join(censor_t,by="ENCOUNTERID") %>%
                    filter(dsa < dsa_y-(pred_in_d-1)) %>% # prediction point is at least "pred_in_d" days before endpoint
                    dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value) %>%
                    bind_rows(censor_t %>% 
                                mutate(dsa=dsa_y-1,
                                       key=paste0("day",(dsa_y-1)),
                                       value=1) %>%
                                dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value)))
    }
    
  }
  
  Xy_surv<-list(X_surv = X_surv,
                y_surv = y_surv)
  
  return(Xy_surv)
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
