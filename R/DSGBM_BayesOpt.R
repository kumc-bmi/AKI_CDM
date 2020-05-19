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
                    "xgboost",
                    "ggplot2",
                    "rBayesianOptimization"))

# experimental design parameters
#----prediction ending point
pred_end<-7

#-----prediction point
pred_in_d_opt<-c(2,1)

#-----prediction tasks
pred_task_lst<-c("stg2up","stg1up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')

#-----number of data chunks
n_chunk<-4

for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    bm<-c()
    bm_nm<-c()
    
    start_tsk<-Sys.time()
    cat("Start build reference model for task",pred_task,"in",pred_in_d,"days",".\n")
    #---------------------------------------------------------------------------------------------
    
    start_tsk_i<-Sys.time()
    #--prepare training and testing set
    X_tr<-c()
    X_ts<-c()
    y_tr<-c()
    y_ts<-c()
    rsample_idx<-readRDS(paste0("./data/preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
    var_by_task<-readRDS(paste0("./data/preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))
    for(i in seq_len(n_chunk)){
      var_by_yr<-var_by_task[[i]]
      
      X_tr %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
        semi_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                  by="ENCOUNTERID")
      
      y_tr %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                            inner_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                                      by="ENCOUNTERID"))
      
      X_ts %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
        semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                  by="ENCOUNTERID")
      
      y_ts %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                            inner_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                                      by="ENCOUNTERID"))
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
      dtrain<-xgb.DMatrix(data=X_tr_sp,label=y_tr_sp$y)
      dtest<-xgb.DMatrix(data=X_ts_sp,label=y_ts_sp$y)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"transform data")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish formatting training and testing sets.\n")
      
      #-----------------------
      start_tsk_i<-Sys.time()
      #--get indices for k folds
      folds<-list()
      for(fd in seq_len(max(y_tr$cv10_idx))){
        fd_df<-y_tr %>% 
          filter(cv10_idx==fd) %>%
          dplyr::select(row_idx)
        folds[[fd]]<-fd_df$row_idx
      }
      
      #--tune hyperparameter (less rounds, early stopping)
      xgb_cv_bayes <- function(max_depth, min_child_weight, subsample, 
                               eta=0.05,colsample_bytree=0.8,lambda=1,alpha=0,gamma=1) {
        cv <- xgb.cv(params = list(booster = "gbtree",
                                   max_depth = max_depth,
                                   min_child_weight = min_child_weight,
                                   subsample = subsample, 
                                   eta = eta,
                                   colsample_bytree = colsample_bytree,
                                   lambda = lambda,
                                   alpha = alpha,
                                   gamma = gamma,
                                   objective = "binary:logistic",
                                   eval_metric = "auc"),
                     data = dtrain,
                     nround = 100,
                     folds = folds,
                     prediction = TRUE,
                     showsd = TRUE,
                     early_stopping_rounds = 5,
                     maximize = TRUE,
                     verbose = 0)
        
        list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
             Pred = cv$pred)
      }
      
      OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                      bounds = list(max_depth = c(4L,10L),
                                                    min_child_weight = c(1L,10L),
                                                    subsample = c(0.5,0.8)),
                                      init_grid_dt = NULL,
                                      init_points = 10,
                                      n_iter = 100,
                                      acq = "ucb",
                                      kappa = 2.576,
                                      eps = 0.0,
                                      verbose = TRUE)
      saveRDS(OPT_Res,file="./data/bayes_opt_eta.rda")
      
      #--determine number of trees, or steps (more rounds, early stopping)
      bst <- xgb.cv(param=data.frame(max_depth=OPT_Res$Best_Par[1],
                                     min_child_weight=OPT_Res$Best_Par[2],
                                     subsample=OPT_Res$Best_Par[3],
                                     eta=0.05,
                                     colsample_bytree=0.8,
                                     lambda=1,
                                     alpha=0,
                                     gamma=1),
                    dtrain,
                    objective = "binary:logistic",
                    metrics = "auc",
                    maximize = TRUE,
                    nrounds=1000,
                    folds = folds,
                    early_stopping_rounds = 50,
                    print_every_n = 50,
                    prediction = F) 
      steps<-which(bst$evaluation_log$test_auc_mean==max(bst$evaluation_log$test_auc_mean))

      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"tune model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model tunning.\n")
      
      #-----------------------
      start_tsk_i<-Sys.time()  
      #--validation
      xgb_tune<-xgb.train(data=dtrain,
                          max_depth=OPT_Res$Best_Par[1],
                          min_child_weight=OPT_Res$Best_Par[2],
                          subsample=OPT_Res$Best_Par[3],
                          maximize = TRUE,
                          eta=0.01,
                          nrounds=steps,
                          eval_metric="auc",
                          objective="binary:logistic",
                          verbose = 0)
      
      valid<-data.frame(y_ts,
                        pred = predict(xgb_tune,dtest),
                        stringsAsFactors = F)
      
      #--feature importance
      feat_imp<-xgb.importance(colnames(X_tr_sp),model=xgb_tune)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"validate model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model validating.\n")
      
      #-----------------------
      #--save model and other results
      result<-list(hyper=c(OPT_Res$Best_Par,steps),
                   model=xgb_tune,
                   valid=valid,
                   feat_imp=feat_imp)
      saveRDS(result,file=paste0("./data/model_ref/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))
      
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
    saveRDS(bm,file=paste0("./data/model_ref/",pred_in_d,"d_bm_gbm_",pred_task,".rda"))
  }
}



