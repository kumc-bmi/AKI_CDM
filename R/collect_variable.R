#### collect variables from CDM tables ####

get_all_var<-function(conn,
                      cdm_db_schema,
                      oracle_temp_schema=cdm_db_schema){
  var_lst<-list()
  
  #demographic
  var_lst[["demo"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                          parse_sql("./inst/collect_demo.sql")))
  
  #encounter
  var_lst[["enc"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                         parse_sql("./inst/collect_enc.sql")))
  
  #vital
  var_lst[["vital"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                           parse_sql("./inst/collect_vital.sql")))
  
  #medication
  var_lst[["med"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                         parse_sql("./inst/collect_med.sql")))
  
  #lab tests
  var_lst[["lab"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                         parse_sql("./inst/collect_lab.sql")))
  
  #diagnosis
  var_lst[["dx"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                        parse_sql("./inst/collect_dx.sql"))) %>%
    #attach CCS diagnosis grouping
    dplyr::mutate(icd_w_type=paste0("ICD",DX_TYPE,":",DX)) %>%
    left_join(ccs_icd %>% select(-ccs_name),by="icd_w_type") %>%
    dplyr::select(-icd_w_type,-ENCOUNTERID) %>%
    unique %>% filter(!is.na(ccs_code))
  
  #procedure
  var_lst[["px"]]<-dbGetQuery(conn,gsub("&&PCORNET_CDM",cdm_db_schema,
                                        parse_sql("./inst/collect_px.sql")))
  
  return(var_lst)
}

