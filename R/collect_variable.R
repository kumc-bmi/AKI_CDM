#### collect variables from CDM tables ####

get_all_var<-function(conn,
                      cdm_db_schema,
                      oracle_temp_schema=cdm_db_schema,
                      verb=T){
  var_lst<-list()
  
  #demographic
  var_lst[["demo"]]<-dbGetQuery(conn,
                                parse_sql("./inst/collect_demo.sql",
                                          cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting DEMOGRAPHIC info./n")
  
  #encounter
  var_lst[["enc"]]<-dbGetQuery(conn,
                               parse_sql("./inst/collect_enc.sql",
                                         cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting ENCOUNTER info./n")
  
  #vital
  var_lst[["vital"]]<-dbGetQuery(conn,
                                 parse_sql("./inst/collect_vital.sql",
                                           cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting VITAL info./n")
  
  #medication
  var_lst[["med"]]<-dbGetQuery(conn,
                               parse_sql("./inst/collect_med.sql",
                                         cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting MEDICATION info./n")
  
  #lab tests
  var_lst[["lab"]]<-dbGetQuery(conn,
                               parse_sql("./inst/collect_lab.sql",
                                         cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting LAB info./n")
  
  #diagnosis
  load("./data/ccs_icd_cw.Rdata")
  var_lst[["dx"]]<-dbGetQuery(conn,
                              parse_sql("./inst/collect_dx.sql",
                                        cdm_db_schema=cdm_db_schema)$statement) %>%
    #attach CCS diagnosis grouping
    dplyr::mutate(icd_w_type=paste0("ICD",DX_TYPE,":",DX)) %>%
    left_join(ccs_icd %>% select(-ccs_name),by="icd_w_type") %>%
    dplyr::select(-icd_w_type,-ENCOUNTERID) %>%
    unique %>% filter(!is.na(ccs_code))
  if(verb) cat("finish collecting DIAGNOSIS info./n")
  
  #procedure
  var_lst[["px"]]<-dbGetQuery(conn,
                              parse_sql("./inst/collect_px.sql",
                                        cdm_db_schema=cdm_db_schema)$statement)
  if(verb) cat("finish collecting PROCEDURE info./n")
  
  return(var_lst)
}

