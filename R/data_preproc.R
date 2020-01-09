############################
#### Data Preprocessing ####
############################
rm(list=ls()); gc()

source("./R/util.R")

require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "Matrix"))


# experimental design parameters
#----prediction ending point
pred_end<-7

#-----prediction point
pred_in_d_opt<-c(1,2)

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')


# collect and format variables on daily basis 
n_chunk<-4

tbl1<-readRDS("./data/Table1.rda") %>%
  dplyr::mutate(yr=as.numeric(format(strptime(ADMIT_DATE, "%Y-%m-%d %H:%M:%S"),"%Y")))

#--by chunks: encounter year
enc_yr<-tbl1 %>%
  dplyr::select(yr) %>%
  unique %>% arrange(yr) %>%
  filter(yr>2009) %>%
  dplyr::mutate(chunk=ceiling((yr-2009)/(n()/n_chunk)))

#--by variable type
var_type<-c("demo","vital","lab","dx","px","med")

for(pred_in_d in pred_in_d_opt){
  #--determine update time window
  tw<-as.double(seq(0,pred_end))
  if(pred_in_d>1){
    tw<-tw[-seq_len(pred_in_d-1)]
  } 
  
  #--save results as array
  for(pred_task in pred_task_lst){
    start_tsk<-Sys.time()
    cat("Start variable collection for task",pred_task,".\n")
    #---------------------------------------------------------------------------------------------
    
    var_by_yr<-list()
    var_bm<-list()
    rsample_idx<-c()
    
    for(i in seq_len(n_chunk)){
      start_i<-Sys.time()
      cat("...start variable collection for year chunk",i,".\n")
      
      #--collect end_points
      yr_i<-enc_yr$yr[enc_yr$chunk==i]
      dat_i<-tbl1 %>% filter(yr %in% yr_i) %>%
        dplyr::select(ENCOUNTERID,yr,
                      NONAKI_SINCE_ADMIT,
                      AKI1_SINCE_ADMIT,
                      AKI2_SINCE_ADMIT,
                      AKI3_SINCE_ADMIT) %>%
        gather(y,dsa_y,-ENCOUNTERID,-yr) %>%
        filter(!is.na(dsa_y)) %>%
        dplyr::mutate(y=recode(y,
                               "NONAKI_SINCE_ADMIT"=0,
                               "AKI1_SINCE_ADMIT"=1,
                               "AKI2_SINCE_ADMIT"=2,
                               "AKI3_SINCE_ADMIT"=3)) %>%
        dplyr::mutate(y=as.numeric(y))
      
      if(pred_task=="stg1up"){
        dat_i %<>%
          dplyr::mutate(y=as.numeric(y>0)) %>%
          group_by(ENCOUNTERID) %>% top_n(n=1L,wt=dsa_y) %>% ungroup
      }else if(pred_task=="stg2up"){
        dat_i %<>%
          group_by(ENCOUNTERID) %>%
          dplyr::filter(max(y)!=1) %>% ungroup %>%   #filter out entire AKI1 encounters
          filter(y!=1) %>%                           #filter AKI1 stages for AKI2 cases
          dplyr::mutate(y=as.numeric(y>=2)) %>%
          group_by(ENCOUNTERID) %>% top_n(n=1L,wt=dsa_y) %>% ungroup
      }else if(pred_task=="stg3"){
        dat_i %<>%
          group_by(ENCOUNTERID) %>%
          filter(max(y) %in% c(1,2)) %>% ungroup %>%   #filter out entire AKI1,2 encounters
          filter(!y %in% c(1,2)) %>%                   #filter out AKI1,2 stages for AKI3 cases
          dplyr::mutate(y=as.numeric(y>2)) %>%
          group_by(ENCOUNTERID) %>% top_n(n=1L,wt=dsa_y) %>% ungroup
      }else{
        stop("prediction task is not valid!")
      }
      
      #--random sampling
      rsample_idx %<>%
        bind_rows(dat_i %>% 
                    dplyr::select(ENCOUNTERID,yr) %>%
                    unique %>%
                    dplyr::mutate(cv10_idx=sample(1:10,n(),replace=T)))
      
      #--ETL variables
      X_surv<-c()
      y_surv<-c()
      var_etl_bm<-c()
      for(v in seq_along(var_type)){
        start_v<-Sys.time()
        
        #extract
        var_v<-readRDS(paste0("./data/AKI_",toupper(var_type[v]),".rda")) %>%
          semi_join(dat_i,by="ENCOUNTERID")
        
        if(var_type[v] != "demo"){
          if(var_type[v] == "med"){
            var_v %<>% 
              transform(value=strsplit(value,","),
                        dsa=strsplit(dsa,",")) %>%
              unnest(value,dsa) %>%
              dplyr::mutate(value=as.numeric(value),
                            dsa=as.numeric(dsa))
          }
          var_v %<>% filter(dsa <= pred_end)
        }
        
        #transform
        var_v<-format_data(dat=var_v,
                           type=var_type[v],
                           pred_end=pred_end)
        
        Xy_surv<-get_dsurv_temporal(dat=var_v,
                                    censor=dat_i,
                                    tw=tw,
                                    pred_in_d=pred_in_d)
        
        #load
        X_surv %<>% bind_rows(Xy_surv$X_surv) %>% unique
        y_surv %<>% bind_rows(Xy_surv$y_surv) %>% unique
        
        lapse_v<-Sys.time()-start_v
        var_etl_bm<-c(var_etl_bm,paste0(lapse_v,units(lapse_v)))
        cat("\n......finished ETL",var_type[v],"for year chunk",i,"in",lapse_v,units(lapse_v),".\n")
      }
      var_by_yr[[i]]<-list(X_surv=X_surv,
                           y_surv=y_surv)
      
      lapse_i<-Sys.time()-start_i
      var_etl_bm<-c(var_etl_bm,paste0(lapse_i,units(lapse_i)))
      cat("\n...finished variabl collection for year chunk",i,"in",lapse_i,units(lapse_i),".\n")
      
      var_bm[[i]]<-data.frame(bm_nm=c(var_type,"overall"),
                              bm_time=var_etl_bm,
                              stringsAsFactors = F)
    }
    
    #--save preprocessed data
    saveRDS(rsample_idx,file=paste0("./data/preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
    saveRDS(var_by_yr,file=paste0("./data/preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))
    saveRDS(var_bm,file=paste0("./data/preproc/",pred_in_d,"d_var_bm",pred_task,".rda"))
    
    #---------------------------------------------------------------------------------------------
    lapse_tsk<-Sys.time()-start_tsk
    cat("\nFinish variable ETL for task:",pred_task,"in",pred_in_d,"days",",in",lapse_tsk,units(lapse_tsk),".\n")
  }
}

