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


#----collect facts from i2b2 observation_fact table----
#note: there should be a reference patient table ("pat_num") on oracle server with the key column "key_col"
collect_i2b2_obs<-function(conn,
                           code_vec=c(),
                           regexp_str="",
                           col_out=c("patient_num",
                                     "encounter_num",
                                     "concept_cd",
                                     "units_cd",
                                     "nval_num",
                                     "tval_char",
                                     "modifier_cd",
                                     "start_date",
                                     "end_date"),
                           key_col=c("patient_num"),
                           schema=c("blueherondata"),
                           pat_num){
  
  col_out<-col_out[!col_out %in% key_col]
  
  match_key<-c()
  for(i in seq_along(key_col)){
    match_key<-c(match_key,paste(paste0(c("p.","f."),key_col[i]),collapse = "="))
  }
  if(length(key_col)>1){
    match_key<-paste(match_key,collapse = " and ")
  }
  
  i2b2_obs_lst<-list()
  
  for(i in seq_along(schema)){
    sql<-paste0("select distinct ",
                paste0(paste(paste0("p.",key_col,collapse = ","),",",
                             paste(paste0("f.",col_out,collapse = ",")),
                             paste0(" from ", pat_num, " p"),
                             paste0(" join ", schema, ".observation_fact f"),
                             " on ",match_key)))
    
    if(length(code_vec)>0&nchar(regexp_str)>0){
      sql<-paste0(sql," and ",
                  "(f.concept_cd in ",paste0("('",paste(code_vec,collapse="','"),"')")," or ",
                  "regexp_like(f.concept_cd,'",regexp_str,"','i'))")
    }else if(length(code_vec)==0&nchar(regexp_str)>0){
      sql<-paste0(sql," and ",
                  "regexp_like(f.concept_cd,'",regexp_str,"','i')")
    }else if(length(code_vec)>0&nchar(regexp_str)==0){
      sql<-paste0(sql," and ",
                  "f.concept_cd in ",paste0("('",paste(code_vec,collapse="','"),"')"))
    }else{
      stop("either code_vec or regexp_str should be specified for filtering concept_cd!")
    }
    
    i2b2_obs_lst[[schema[i]]]<-DBI::dbGetQuery(conn,sql)
  }
  
  return(i2b2_obs_lst)
}

#----collect concepts from i2b2 concept_dimension table----
#note: there should be a reference concept table ("concept") on oracle server with the key column "key_col"
collect_i2b2_cd<-function(conn,
                          exact_match=T,
                          cd_prefix=NULL,
                          col_out=c("concept_cd",
                                    "name_char",
                                    "concept_path"),
                          key_col=c("CONCEPT_CD"),
                          schema=c("blueherondata"),
                          concept){
  
  col_out<-col_out[!col_out %in% key_col]
  
  match_key<-c()
  for(i in seq_along(key_col)){
    if(exact_match){
      match_key<-c(match_key,paste(paste0(c("cd.","f."),key_col[i]),collapse = "="))
    }else{
      match_key<-c(match_key,paste0("regexp_like(f.",key_col[i],",('(' || cd.",key_col[i]," || ')+'),'i')"))
    }
    
    if(!is.null(cd_prefix)){
      match_key<-paste0(match_key," and regexp_like(f.",key_col[i],",'^(",cd_prefix,")+')")
    }else{
      
    }
  }
  
  if(length(key_col)>1){
    match_key<-paste(match_key,collapse = " and ")
  }
  
  i2b2_obs_lst<-list()
  
  for(i in seq_along(schema)){
    sql<-paste0("select distinct ",
                paste0(paste(paste0("cd.",key_col,collapse = ",")," ICD_FUZZY,",
                             paste(paste0("f.",col_out,collapse = ",")),
                             paste0(" from ", concept, " cd"),
                             paste0(" join ", schema, ".concept_dimension f"),
                             " on ",match_key)))
    
    i2b2_obs_lst[[schema[i]]]<-DBI::dbGetQuery(conn,sql)
  }
  
  return(i2b2_obs_lst)
}


#----collect data from one of the CDM tables----
#note: there should be a reference patient table ("pat_num") on oracle server with the key column "key_col"
collect_cdm<-function(conn,
                      code_vec=c(),
                      str_vec=c(),
                      col_out=NULL,
                      key_col_schema=c("PATID"),
                      key_col_pat=c("PATIENT_NUM"),
                      schema=c("PCORNET_CDM_C7R2"),
                      tbl="DEMOGRAPHIC",
                      pat_num){
  if(is.null(col_out)){
    col_out<-colnames(DBI::dbGetQuery(conn,
                                      paste0("select * from ",schema[1],".",tbl," where 1=0")))
  }
  col_out<-col_out[!col_out %in% key_col_schema]
  
  
  match_key<-c()
  for(i in seq_along(key_col_pat)){
    match_key<-c(match_key,paste(c(paste0("p.",key_col_pat[i]),
                                   paste0("f.",key_col_schema[i])),
                                 collapse = "="))
  }
  if(length(key_col_pat)>1){
    match_key<-paste(match_key,collapse = " and ")
  }
  
  cdm_obs_lst<-list()
  
  for(i in seq_along(schema)){
    sql<-paste0("select distinct ",
                paste0(paste(paste0("p.",key_col_pat,collapse = ","),",",
                             paste(paste0("f.",col_out,collapse = ",")),
                             paste0(" from ", pat_num, " p"),
                             paste0(" join ", schema[i], ".",tbl," f"),
                             " on ",match_key)))
    if(tbl=="PROCEDURES"){
      #procedures are identified uniquely by (PX_TYPE || ':' || PX)
      sql<-paste0(sql," and",
                  " (f.PX_TYPE || ':' || f.PX) in ",paste0("('",paste(code_vec,collapse="','"),"')"))
      
    }else if(tbl=="PRESCRIBING"){
      #prescribing are identified uniquely by RXNORM_CUI or RAW_RX_MED_NAME
      if(length(code_vec)>0){
        sql<-paste0(sql," and",
                    "f.RXNORM_CUI in ",paste0("('",paste(code_vec,collapse="','"),"')"))
      }
      if(length(str_vec)>0){
        if(length(code_vec)>0){
          sql<-paste0(sql, " or ",
                      " regexp_like(f.RAW_RX_MED_NAME,",paste0("'((",paste(str_vec,collapse = ")|("),"))+'"),",'i')")
        }else{
          sql<-paste0(sql, " and ",
                      " regexp_like(f.RAW_RX_MED_NAME,",paste0("'((",paste(str_vec,collapse = ")|("),"))+'"),",'i')")
        }
      }
      
    }else if(tbl=="DISPENSING"){
      #dispensing are identified by NDC codes
      sql<-paste0(sql," and ",
                  "(f.NDC in ",paste0("('",paste(code_vec,collapse="','"),"')"))
      
    }else if(tbl=="DIAGNOSIS"){
      #diagnosis are identified by (DX_TYPE || ':' || DX)
      if(length(code_vec)>0){
        sql<-paste0(sql," and",
                    "(f.DX_TYPE || ':' || f.DX) in ",paste0("('",paste(code_vec,collapse="','"),"')"))
      }
      if(length(str_vec)>0){
        if(length(code_vec)>0){
          sql<-paste0(sql, " or ",
                      " regexp_like((f.DX_TYPE || ':' || f.DX),",paste0("'((",paste(str_vec,collapse = ")|("),"))+'"),",'i')")
        }else{
          sql<-paste0(sql," and",
                      " regexp_like((f.DX_TYPE || ':' || f.DX),",paste0("'((",paste(str_vec,collapse = ")|("),"))+'"),",'i')")
        }
        
      }
      
    }else{
      sql<-sql
    }
    
    cdm_obs_lst[[schema[i]]]<-DBI::dbGetQuery(conn,sql)
  }
  
  return(cdm_obs_lst)
  
}


consort_diag<-function(consort_tbl){
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
    
    bp_slp<-bp_slp_eligb %>%
      filter(df > 1 & sd >= 1e-2) %>%
      nest(data=c(timestamp,value,df,sd)) %>%
      mutate(
        fit_val=map(data, ~ lm(value ~ timestamp, data=.x)),
        tidied=map(fit_val,tidy)
      ) %>%
      unnest(tidied)
    
    bp_slp %<>%
      filter(term=="timestamp") %>%
      dplyr::rename(value=estimate) %>%
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


## convert long matrix to wide sparse matrix
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

get_recalibr<-function(pred,real,p=0.5,n_bin=20){
  re_calib_in<-get_calibr(pred=pred,real=real) %>%
    mutate(grp_idx=sample(1:2,n(),prob=c(p,1-p),replace = TRUE)) %>%
    select(y_agg,pred_p,pred_bin, expos, grp_idx) %>%
    gather(overall_mear,meas_med,-pred_bin,-expos,-grp_idx) %>%
  
  re_calib_in1<-re_calib_in %>% filter(grp_idx==1)
  re_calib_in2<-re_calib_in %>% filter(grp_idx==2)
  
  re_calib_in_lm<- re_calib_in %>% filter(grp_idx==1) %>%
    group_by(pred_bin) %>%
    dplyr::summarize(expos=round(mean(expos)),
                     meas_med=median(meas_med),
                     .groups="drop") %>%
    spread(overall_meas,meas_med) %>%
    group_by(pred_in_d,pred_task,fs_type,grp) %>%
    do(recalib=lm(y_p ~ pred_p,data=.))
  
  re_calib_in_coef<-tidy(re_calib_in_lm,recalib) %>%
    select(term,estimate) %>%
    mutate(term=recode(term,
                       "(Intercept)"="a",
                       "pred_p"="b")) %>%
    spread(term,estimate) %>%
    ungroup
  
  int_calib<-re_calib_in %>% filter(grp_idx==2) %>%
    group_by(pred_bin) %>%
    dplyr::summarize(expos=round(mean(expos)),
                     meas_med=median(meas_med),
                     .groups="drop") %>%
    spread(overall_meas,meas_med) %>% mutate(k=1)
    left_join(re_calib_in_coef %>% mutate(k=1),by="k") %>%
    mutate(pred_p_adj=pred_p*b+a) %>% # simple re-calibration
    dplyr::rename("real_pos"="y_agg") %>%
    mutate(real_neg=expos-real_pos,
           pred_pos=round(expos*pred_p),
           pred_pos_adj=round(expos*pred_p_adj),
           pred_neg=expos-pred_pos,
           pred_neg_adj=expos-pred_pos_adj) %>%
    filter(pred_pos>0&pred_neg>0&pred_pos_adj>0&pred_neg_adj>0) %>%
    mutate(pos_term=((real_pos-pred_pos)^2)/pred_pos,
           neg_term=((real_neg-pred_neg)^2)/pred_neg,
           pos_adj_term=((real_pos-pred_pos_adj)^2)/pred_pos_adj,
           neg_adj_term=((real_neg-pred_neg_adj)^2)/pred_neg_adj)
  
  int_calib_pvt<-int_calib %>% 
    select(pred_bin,pred_p,pred_p_adj,expos) %>%
    left_join(int_calib %>% select(pred_bin,real_pos),
              by="pred_bin") %>%
    unique %>% mutate(y_p=real_pos/expos)
  
  int_calib_hl<-int_calib %>%
    dplyr::summarize(chi_sq=sum(pos_term)+sum(neg_term),
                     chi_sq_adj=sum(pos_adj_term)+sum(neg_adj_term),
                     df=max(3,length(unique(pred_bin)))-2,
                     groups="drop") %>%
    mutate(p_val=pchisq(chi_sq,df=df,lower.tail = F),
           p_val_adj=pchisq(chi_sq_adj,df=df,lower.tail = F))
  
  return(list(int_calib_pvt=int_calib_pvt,
              int_calib_hl=int_calib_hl))
}

bin_fd<-function(x){
  n<-length(x)
  k<-length(unique(x))
  x<-x[order(x)]
  
  #remove outliers (middle 95%)
  lb<-quantile(x,probs=0.025,na.rm=T)
  ub<-quantile(x,probs=0.975,na.rm=T)
  x<-x[x>=lb&x<=ub]
  
  #https://www.answerminer.com/blog/binning-guide-ideal-histogram
  if(IQR(x,na.rm=T)!=0){
    n_bin<-(max(x,na.rm=T)-min(x,na.rm=T))/(2*IQR(x,na.rm=T)/(k^(1/3)))
  }else{
    n_bin<-(max(x,na.rm=T)-min(x,na.rm=T))/(3.5*sd(x,na.rm=T)/(k^(1/3)))
  }
  brk<-levels(cut(x,n_bin,include.lowest = T,right=F))
  lb<-c(-Inf,as.numeric(gsub("(,).*","",gsub("\\[","",brk))))
  ub<-c(lb[-1],Inf)
  
  return(data.frame(bin=seq_len(length(lb)),brk_lb=lb,brk_ub=ub))
}

get_ks<-function(x,y,unbiased=T,rs=0.6){
  x_rs<-sample(x,round(length(x)*rs))
  y_rs<-sample(y,round(length(y)*rs))
  
  #broadcast x_rs
  # x_mt<-t(replicate(length(y),x_rs)) #slow
  x_mt<-x_rs %*% t(rep(1,length(y_rs))) 
  #get pair-wise gaussian kernel matrix
  gauss_k<-exp(-(x_mt-y_rs)^2/2)
  #nullify the diagnoal
  gauss_k[row(gauss_k) == col(gauss_k)] <- NA
  #take the average
  if(unbiased==T){
    xyk<-sum(gauss_k,na.rm=T)/(length(x_rs)*(length(y_rs)-1)) 
  }else{
    xyk<-sum(gauss_k,na.rm=T)/(length(x_rs)*length(y_rs))
  }
  return(xyk)
}

penalize_sample<-function(x,n,alpha=0.99){
  #kernel density estimation
  gk<-density(x)
  #get cumulative distribution
  fk<-scales::rescale(cumsum(gk$y),c(0,1))
  #unlikely value range
  bias_pool<-c(gk$x[c(which(fk>=alpha),(fk<=(1-alpha)))])
  #generate noises
  bias_rs<-sample(bias_pool,n,replace=T)
  return(bias_rs)
}
