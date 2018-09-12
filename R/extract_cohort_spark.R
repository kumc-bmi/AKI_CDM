#### under development ####

#### extract AKI cohort using spark ####

rm(list=ls())
gc()

source("./R/util.R")
require_libraries(c("sparklyr",
                    "spark.sas7bdat", #only for reading SAS data
                    "tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr"))

#set up spark home
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = file.path(spark_install_dir(),
                                    spark_installed_versions()$dir)) #point to the directory where spark was installed
}

#set up CDM db home (various db sources: Oracle, tSQL, sqlite, SAS, PostgreSQL)
#Oracle example
if (nchar(Sys.getenv("ORACLE_HOME")) < 1) {
  path_to_oracle_hm<-"/usr/lib/oracle/11.2/client64/lib"
  Sys.setenv(ORACLE_HOME = path_to_oracle_hm)
}
oracle.jar<-file.path(Sys.getenv("ORACLE_HOME"),"ojdbc6.jar")


#acknowledge spark about the db type it about to connect to
sc_config<-spark_config()
sc_config$`sparklyr.shell.driver-class-path`<-oracle.jar
sc_config$spark.executor.memory<-"30G"


#connect to local spark cluster
sc<-spark_connect(master = "local", config = sc_config,
                  version = spark_installed_versions()$spark)


#specify db connection details
db_config<-read.csv("../db_config.csv",stringsAsFactors = F)
db_config$driver<-"oracle.jdbc.driver.OracleDriver"


#possible ways to partition data sets
#spark_apply -- not working!

sdf_len(sc, 5, repartition = 1) %>%
  spark_apply(function(e) I(e))

sdf_len(sc, 10) %>% spark_apply(function(e) e)

enc_part<-spark_read_jdbc(sc=sc, 
                          name="ENCOUNTER", 
                          memory = F, 
                          overwrite = T,
                          options = c(format='jdbc',
                                      db_config,
                                      dbtable="(select * from XSONG.ENCOUNTER limit 50 as enc_50r")
                          ) %>%
  collect

  spark_apply(function(e) nrow(e), names = "n")


#try connect to oracle
fetchsize<-1000




cdm_db_schema="PCORNET_CDM_C5R1"

extract_cohort_spark<-function(sc,
                               cdm_db_schema,
                               start_date="2010-01-01",
                               end_date="2018-12-31",
                               partition=T,
                               chunk_size=100000,
                               verb=T){
  
  #register demographic table
  pat_num<-spark_read_jdbc(sc=sc, 
                           name="DEMOGRAPHIC", 
                           memory = F, 
                           overwrite = T,
                           options = c(format='jdbc',
                                       db_config,
                                       dbtable=paste0(cdm_db_schema,".DEMOGRAPHIC"))
                            ) %>%
    dplyr::select(PATID,BIRTH_DATE) %>%
    collect
  
  #get number of chunks based on chunk-size
  chk_num<-round(nrow(pat_num)/chunk_size)
  pat_num %<>% mutate(chk_no=cut(1:n(),breaks=chk_num,labels=F))
  
  #save master patient table and remove from cache
  saveRDS(pat_num,file="./data/pat_num.rda")
  rm(pat_num); gc()
  
  pat_demo<-spark_read_jdbc(sc=sc, 
                            name="DEMOGRAPHIC", 
                            memory = F, 
                            overwrite = T,
                            options = c(format='jdbc',
                                        db_config,
                                        dbtable=paste0(cdm_db_schema,".DEMOGRAPHIC"))
                            ) %>%
    dplyr::select(PATID,SEX,RACE) %>%
    mutate(FEMALE_IND=ifelse(SEX=='F',1,0)) %>% dplyr::select(-SEX) %>%
    mutate(RACE_AA_IND=ifelse(RACE=='03',1,0)) %>% dplyr::select(-RACE) %>%
    collect
  
  #save master patient table and remove from cache
  saveRDS(pat_demo,file="./data/pat_demo.rda")
  rm(pat_demo); gc()

  #initialize output table
  Table1<-c()
  consort<-c()
  for(i in seq_len(chk_num)){
    #patient chunk i
    pat_i<-readRDS("./data/pat_num.rda") %>%
      filter(chk_no==i) %>% dplyr::select(-chk_no) %>%
      inner_join(demo<-readRDS("./data/pat_demo.rda"),by="PATID")
    
    #initial cohort
    enc_tbl<-spark_read_jdbc(sc=sc, 
                             name="ENCOUNTER", 
                             memory = F, 
                             overwrite = T,
                             options = c(format='jdbc',
                                         db_config,
                                         dbtable=paste0(cdm_db_schema,".ENCOUNTER"))
                             ) %>%
      dplyr::select(PATID,ENCOUNTERID,ADMIT_DATE,ADMIT_TIME,
                    DISCHARGE_DATE,DISCHARGE_TIME,ENC_TYPE) %>%
      filter((ADMIT_DATE <= DISCHARGE_DATE) & 
             (ADMIT_DATE <= as.Date(end_date) & ADMIT_DATE >= as.Date(start_date)) &
             (ENC_TYPE %in% c('EI','IP','IS'))) %>% collect %>%
      inner_join(pat_i %>% dplyr::select(PATID,BIRTH_DATE),by="PATID") %>%
      mutate(age_at_admit=round(difftime(ADMIT_DATE,BIRTH_DATE,units="days")/365.25)) %>%
      filter(difftime(DISCHARGE_DATE,ADMIT_DATE,units="days")>=2 &
             age_at_admit >= 18)
    
    #collect SCr
    pat_i %<>% inner_join(enc_tbl %>% dplyr::select(PATID,ENCOUNTERID),by="PATID") #attach encounterid 
    scr_all<-spark_read_jdbc(sc=sc, 
                             name="LAB_RESULT_CM", 
                             memory = F, 
                             overwrite = T,
                             options = c(format='jdbc',
                                         db_config,
                                         dbtable=paste0(cdm_db_schema,".LAB_RESULT_CM"))
                             ) %>%
      filter(RESULT_NUM > 0 &
             LAB_LOINC %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8') &
             RESULT_UNIT=="MG/DL" &
             SPECIMEN_SOURCE!="URINE") %>%
      dplyr::select(ENCOUNTERID,SPECIMEN_DATE,SPECIMEN_TIME,RESULT_NUM) %>%
      collect %>%
      inner_join(pat_i,by="ENCOUNTERID") %>%
      mutate(age_at_SCr=round(difftime(SPECIMEN_DATE,BIRTH_DATE,units="days")/365.25)) %>%
      filter(age_at_SCr >= 18) %>%
      group_by(ENCOUNTERID,SPECIMEN_DATE,SPECIMEN_TIME) %>%
      dplyr::summarize(RESULT_NUM=mean(RESULT_NUM,na.rm=T)) %>% ungroup %>%
      mutate(eGFR=175*RESULT_NUM^(-1.154)*age_at_Scr^(-0.203)*(0.742*FEMALE_IND+(1-FEMALE_IND))*(1.212*RACE_AA_IND+(1-RACE_AA_IND)))
      
      

      
      

  }
%>%
 %>%
    filter(,
           )
    inner_join(demo_tbl %>% dplyr::select(PATID,BIRTH_DATE),by="PATID") %>%
    filter

  
  #register all CDM tables of interests
  tbl_lst<-c("ENCOUNTER",
             "DEMOGRAPHIC",
             "LAB_RESULT_CM",
             "DIAGNOSIS",
             "PROCEDURES",
             "VITAL",
             "PRESCRIBING")
  
  for(tbl_nm in tbl_lst){
    
  }
  
  #
  demo_tbl<-sdf_copy_to(sc,DEMOGRAPHIC,repartition=10)
  
  #get all inpatient encounters with LOS >=2 and withing time window [start_date, end_date]
  db_tbl<-sc %>%
    spark_read_jdbc(sc=., 
                    name="ENCOUNTER", 
                    memory = F, 
                    overwrite = T, 
                    options = c(format='jdbc',
                                db_config,
                                dbtable="(select ENCOUNTERID,ADMIT_DATE,ADMIT_TIME,
                                                 DISCHARGE_DATE,DISCHARGE_TIME,
                                                 ENC_TYPE,DISCHARGE_STATUS
                                          from ENCOUNTER
                                          where DISCHARGE_DATE - ADMIT_DATE >= 2 and
                                                ) as IP_ENC")
                    )
  
}


#disconnect spark
spark_disconnect(sc)
  