rm(list=ls()); gc()
source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection",
                    "pROC",
                    "ROCR",
                    "PRROC",
                    "xgboost"
))

#-----prediction point
pred_in_d_opt<-c(1,2)

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----feature selection type
# fs_type_opt<-c("no_fs","rm_scr_bun")
fs_type_opt<-"rm_scr_bun"
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')

# collect and format variables on daily basis 
n_chunk<-4

for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    bm<-c()
    bm_nm<-c()
    
    start_tsk<-Sys.time()
    cat("Start interpret model for",pred_task,"in",pred_in_d,"days",".\n")
    #---------------------------------------------------------------------------------------------
    
    start_tsk_i<-Sys.time()
    #--prepare testing set
    X_ts<-c()
    y_ts<-c()
    rsample_idx<-readRDS(paste0("./data/preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
    var_by_task<-readRDS(paste0("./data/preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))
    for(i in seq_len(n_chunk)){
      var_by_yr<-var_by_task[[i]]
      
      X_ts %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
        semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                  by="ENCOUNTERID")
      
      y_ts %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                            left_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                                      by="ENCOUNTERID"))
    }
    lapse_i<-Sys.time()-start_tsk_i
    bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
    bm_nm<-c(bm_nm,"prepare data")
    
    #-----------------------
    y_ts %<>%
      filter(!is.na(cv10_idx))
    
    for(fs_type in fs_type_opt){
      start_tsk_i<-Sys.time()
      
      #--pre-filter
      if(fs_type=="rm_scr_bun"){
        X_ts %<>%
          filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
      }
      
      #--collect variables used in training
      gbm_ctnr<-readRDS(paste0("./data/model_kumc/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))
      tr_key<-data.frame(key = gbm_ctnr$model$feature_names,
                         stringsAsFactors = F)
      
      #--transform testing matrix
      y_ts_sp<-y_ts %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      X_ts_sp<-X_ts %>% 
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        semi_join(y_ts_sp,by="ROW_ID") %>%
        semi_join(tr_key,by="key")
      
      x_add<-tr_key %>%
        anti_join(data.frame(key = unique(X_ts_sp$key),
                             stringsAsFactors = F),
                  by="key")
      
      #align with training
      if(nrow(x_add)>0){
        X_ts_sp %<>%
          arrange(ROW_ID) %>%
          bind_rows(data.frame(ROW_ID = rep("0_0",nrow(x_add)),
                               dsa = -99,
                               key = x_add$key,
                               value = 0,
                               stringsAsFactors=F))
      }
      X_ts_sp %<>%
        long_to_sparse_matrix(df=.,
                              id="ROW_ID",
                              variable="key",
                              val="value")
      if(nrow(x_add)>0){
        X_ts_sp<-X_ts_sp[-1,]
      }
      
      #check alignment
      if(!all(row.names(X_ts_sp)==y_ts_sp$ROW_ID)){
        stop("row ids of testing set don't match!")
      }
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"transform data")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish formatting testing sets.\n")
      
      ##------------------------------------------------------------------------------------
      #load trained model
      gbm_model<-gbm_ctnr$model

      #bootstrap CI for SHAP values
      boots<-30
      ns<-5000
      
      pred_brkdn_b<-c()
      x_val_b<-c()
      
      for(b in 1:boots){
        start_b<-Sys.time()
        
        #smaller sample
        n<-nrow(X_ts_sp)
        idxset<-sample(1:n,ns,replace=F)
        contr <- predict(gbm_model,
                         X_ts_sp[idxset,],
                         predcontrib = TRUE)
        
        shap<-xgb.plot.shap(data=X_ts_sp[idxset,],
                            shap_contrib=contr,
                            model = gbm_model,
                            top_n = 20,
                            plot=F)
        
        pred_brkdn_b %<>%
          bind_rows(cbind(as.data.frame(shap$shap_contrib),
                          boot=b,idx=idxset))
        
        x_val_b %<>%
          bind_rows(cbind(as.data.frame(as.matrix(X_ts_sp[idxset,which(colnames(X_ts_sp) %in% colnames(shap$shap_contrib))])),
                          boot=b,idx=idxset))
        
        lapse<-Sys.time()-start_b
        cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
            "...finish bootstrapped sample",b,"in",lapse,units(lapse),".\n")
      }
      
      pred_brkdn<-c()
      var_lst<-colnames(shap$shap_contrib)
      for(v in seq_along(var_lst)){
        pred_brkdn %<>%
          bind_rows(pred_brkdn_b %>%
                      dplyr::select(var_lst[v],boot,idx) %>%
                      dplyr::mutate(val=round(x_val_b[,var_lst[v]],2)) %>%
                      group_by(boot,val) %>%
                      dplyr::summarise(effect=mean(get(var_lst[v]))) %>%
                      ungroup %>%
                      dplyr::mutate(var=var_lst[v]))
      }
      
      saveRDS(pred_brkdn,file=paste0("./data/model_explain/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))
    }
  }
}

      

