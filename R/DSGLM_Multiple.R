##################################
#### Bayesion Opt DSGBM Model ####
##################################
rm(list=ls())
gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "Matrix",
                    "sparsio",
                    "h2o",
                    "ggplot2"))

# experimental design parameters
#-----prediction point
pred_in_d_opt<-c(2,1)

#-----prediction tasks
pred_task_lst<-c("stg2up",
                 "stg1up",
                 "stg3")

#-----feature selection type
fs_type_opt<-c("full","rm")
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')

#--initialize h2o
h2o.init(nthreads=-1)

for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    bm<-c()
    bm_nm<-c()
    
    start_tsk<-Sys.time()
    cat("Start build reference model for task",pred_task,"in",pred_in_d,"days",".\n")
    #---------------------------------------------------------------------------------------------
    dat_ds<-readRDS(paste0("./data/preproc/data_ds_",pred_in_d,"d_",pred_task,".rda"))
    
    start_tsk_i<-Sys.time()
    #--prepare training and testing set
    X_tr<-dat_ds[[2]][["X_surv"]] %>%
      semi_join(dat_ds[[1]] %>% filter(cv10_idx<=7 & yr<2017),
                by="ENCOUNTERID")
    
    y_tr<-dat_ds[[2]][["y_surv"]] %>%
      inner_join(dat_ds[[1]] %>% filter(cv10_idx<=7 & yr<2017),
                 by="ENCOUNTERID")
    
    X_ts<-dat_ds[[2]][["X_surv"]] %>%
      semi_join(dat_ds[[1]] %>% filter(cv10_idx>7 | yr>=2017),
                by="ENCOUNTERID")
    
    y_ts<-dat_ds[[2]][["y_surv"]] %>%
      inner_join(dat_ds[[1]] %>% filter(cv10_idx>7 | yr>=2017),
                 by="ENCOUNTERID")
    
    #--pre-filter
    if(fs_type=="rm"){
      X_tr %<>%
        filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
      
      X_ts %<>%
        filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
    }
      
    lapse_i<-Sys.time()-start_tsk_i
    bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
    bm_nm<-c(bm_nm,"prepare data")
    
    #-----------------------
    y_tr %<>% 
      dplyr::mutate(row_idx = 1:n()) %>%
      filter(!is.na(cv10_idx))
    
    for(fs_type in fs_type_opt){
      start_tsk_i<-Sys.time()
      
      #--pre-filter
      if(fs_type=="rm_scr_bun"){
        X_tr %<>%
          filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
        
        X_ts %<>%
          filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
      }
      
      #--transform training matrix
      y_tr_sp<-y_tr %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      X_tr_sp<-X_tr %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        semi_join(y_tr_sp,by="ROW_ID") %>%
        long_to_sparse_matrix(df=.,
                              id="ROW_ID",
                              variable="key",
                              val="value")
      
      #--collect variables used in training
      tr_key<-data.frame(key = unique(colnames(X_tr_sp)),
                         stringsAsFactors = F)
      
      #--transform testing matrix
      y_ts_sp<-y_ts %>%
        filter(!is.na(cv10_idx)) %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      X_ts_sp<-X_ts %>% 
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        semi_join(y_ts_sp,by="ROW_ID") %>%
        semi_join(tr_key,by="key")
      
      x_add<-tr_key %>%
        anti_join(data.frame(key = unique(X_ts$key),
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
      if(!all(row.names(X_tr_sp)==y_tr_sp$ROW_ID)){
        stop("row ids of traning set don't match!")
      }
      if(!all(row.names(X_ts_sp)==y_ts_sp$ROW_ID)){
        stop("row ids of testing set don't match!")
      }
      if(!all(colnames(X_tr_sp)==colnames(X_ts_sp))){
        stop("feature names don't match!")
      }
      
      #--covert to xgb data frame
      train_mt<-cbind(X_tr_sp,fold=y_tr_sp$cv10_idx)
      test_mt<-cbind(X_ts_sp,fold=y_ts_sp$cv10_idx)
      
      #key indices
      pred_idx<-which(!colnames(train_mt) %in% c("fold"))
      fold_idx<-which(colnames(train_mt)=="fold")
      
      #foreign symbol in variable names may cause issue
      col_encode<-data.frame(col_name=colnames(train_mt),
                             col_code=paste0("C",1:ncol(train_mt)),
                             stringsAsFactors = F)
      colnames(train_mt)<-col_encode$col_code
      colnames(test_mt)<-col_encode$col_code
      
      #for more efficient conversion to h2o format
      write_svmlight(x = train_mt, y=y_tr_sp$y,
                     file = "./data/train_svmlite.txt",
                     zero_based = FALSE)
      write_svmlight(x = test_mt, y=y_ts_sp$y,
                     file = "./data/test_svmlite.txt",
                     zero_based = FALSE)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"transform data")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish formatting training and testing sets.\n")
      
      #-----------------------
      start_tsk_i<-Sys.time()
      
      #--training
      h2o.removeAll()
      train_h2o<-h2o.importFile("./data/train_svmlite.txt", parse = TRUE)
      fit_lasso<-h2o.glm(x=pred_idx+1,
                         y=1,  
                         training_frame=train_h2o,
                         family="binomial",
                         solver="COORDINATE_DESCENT",   #same optimization method as glmnet
                         fold_column = paste0("C",fold_idx+1),
                         ignore_const_cols = TRUE,
                         lambda_search=TRUE,
                         early_stopping = TRUE,
                         standardize = TRUE,
                         alpha=1, #lasso
                         # missing_values_handling="Skip",
                         remove_collinear_columns=TRUE)
      
      #-----------------------
      start_tsk_i<-Sys.time()  
      #--validation
      test_h2o<-h2o.importFile("./data/test_svmlite.txt", parse = TRUE)
      fitted<-h2o.predict(fit_lasso,newdata=test_h2o[,-1])
      valid<-data.frame(y_ts_sp,
                        pred = as.data.frame(fitted)$p1,
                        stringsAsFactors = F)
      
      #--feature importance
      feat_imp<-h2o.getModel(fit_lasso@model_id)@model$coefficients_table %>%
        inner_join(h2o.varimp(fit_lasso) %>% select(variable,scaled_importance) %>%
                     mutate(variable=as.character(variable)),
                   by=c("names"="variable")) %>%
        left_join(col_encode,by=c("names"="col_code")) %>%
        dplyr::filter(coefficients != 0) %>%
        dplyr::select(col_name,scaled_importance,
                      coefficients,standardized_coefficients) %>%
        dplyr::rename(Feature=col_name) %>%
        arrange(desc(scaled_importance)) %>%
        dplyr::mutate(rank=1:n())
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"validate model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model validating.\n")
      
      #-----------------------
      #--save model and other results
      result<-list(model=fit_lasso,
                   valid=valid,
                   feat_imp=feat_imp)
      
      saveRDS(result,file=paste0("./data/model_glm/",pred_in_d,"d_",pred_task,"_",fs_type,".rda"))
      
      #-------------------------------------------------------------------------------------------------------------
      lapse_tsk<-Sys.time()-start_tsk
      bm<-c(bm,paste0(round(lapse_tsk,1),units(lapse_tsk)))
      bm_nm<-c(bm_nm,"complete task")
      
      cat("\nFinish building reference models for task:",pred_task,"in",pred_in_d,"with",fs_type,",in",lapse_tsk,units(lapse_tsk),
          ".\n--------------------------\n")
    }
    
    #benchmark
    bm<-data.frame(bm_nm=bm_nm,bm_time=bm,
                   stringsAsFactors = F)
    saveRDS(bm,file=paste0("./data/model_glm/",pred_in_d,"d_bm_gbm_",pred_task,".rda"))
  }
}

h2o.shutdown(prompt = FALSE)



