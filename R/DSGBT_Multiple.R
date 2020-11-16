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
                    "rBayesianOptimization",
                    "ParBayesianOptimization",
                    "doParallel"))

#-----cluster parameter
N_CL<-4

# experimental design parameters
#-----prediction point
pred_in_d_opt<-c(2,1)

#-----prediction tasks
pred_task_lst<-c("stg2up",
                 "stg1up",
                 "stg3")

#-----feature pre-selection type
fs_type_opt<-c("full","rm")
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '48642-3','48643-1', #eGFR
          '3097-3','44734-2','BUN_SCR')

#-----model spec
bounds <- list(
  max_depth = c(4L, 10L),
  min_child_weight = c(2L,10L),
  # subsample = c(0.5,0.8),
  colsample_bytree=c(0.3,0.8)
  # eta=c(0.05,0.1)
)


for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    bm<-c()
    bm_nm<-c()
    
    start_tsk<-Sys.time()
    cat("Start build reference model for task",pred_task,"in",pred_in_d,"days",".\n")
    #---------------------------------------------------------------------------------------------
    
    dat_ds<-readRDS(paste0("./data/preproc/data_ds_",pred_in_d,"d_",pred_task,".rda"))
    
    for(fs_type in fs_type_opt){
      #-----------training------------
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
      
      #--transform training matrix
      start_tsk_i<-Sys.time()
      
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
      y_tr_sp %<>% dplyr::mutate(row_idx = 1:n())
      for(fd in seq_len(max(y_tr_sp$cv10_idx))){
        fd_df<-y_tr_sp %>% 
          filter(cv10_idx==fd&!is.na(row_idx)) %>%
          dplyr::select(row_idx)
        folds[[fd]]<-fd_df$row_idx
      }
      folds<-folds[length(folds)>0]
      
      #--parallelization
      cl <- makeCluster(N_CL)
      registerDoParallel(cl)
      clusterExport(cl,c('folds','X_tr_sp','y_tr_sp')) # copying data to clusters (note:xgb.DMatrix is not compatible with parallelization)
      clusterEvalQ(cl,expr= {               # copying model to clusters
        library(xgboost)
      })
      
      #--tune hyperparameter (less rounds, early stopping)
      xgb_cv_bayes <- function(max_depth=10L, min_child_weight=1L, subsample=0.7,
                               eta=0.05,colsample_bytree=0.8,lambda=1,alpha=0,gamma=1) {
        
        dtrain<-xgb.DMatrix(data=X_tr_sp,label=y_tr_sp$y)
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
                     prediction = FALSE,
                     # showsd = TRUE,
                     early_stopping_rounds = 5,
                     maximize = TRUE,
                     verbose = 0)
        
        return(list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration]))
      }
      
      OPT_Res <- bayesOpt(
        FUN = xgb_cv_bayes,
        bounds = bounds,
        initPoints = 5,
        iters.n = 50,
        iters.k = N_CL,
        parallel = TRUE,
        acq = "ucb",
        kappa = 2.576,
        eps = 0.0,
        otherHalting = list(timeLimit = 1800) #--limit maximal running time for better efficiency-- <5hr
      )
      
      Best_Par<-getBestPars(OPT_Res)
      
      #--stop cluster
      stopCluster(cl)
      registerDoSEQ()
      
      #--determine number of trees, or steps (more rounds, early stopping)
      bst <- xgb.cv(params = list(booster = "gbtree",
                                  max_depth = Best_Par$max_depth,
                                  min_child_weight = Best_Par$min_child_weight,
                                  colsample_bytree = Best_Par$colsample_bytree,
                                  subsample=0.7,
                                  eta=0.05,
                                  lambda=1,
                                  alpha=0,
                                  gamma=1,
                                  objective = "binary:logistic",
                                  eval_metric = "auc"),
                    data = dtrain,
                    nround = 500,
                    folds = folds,
                    # nfold=5,
                    prediction = FALSE,
                    # showsd = TRUE,
                    early_stopping_rounds = 50,
                    maximize = TRUE,
                    verbose = 1) 
      
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
                          max_depth = Best_Par$max_depth,
                          min_child_weight = Best_Par$min_child_weight,
                          colsample_bytree = Best_Par$colsample_bytree,
                          subsample=0.7,
                          eta=0.05,
                          maximize = TRUE,
                          nrounds=steps,
                          eval_metric="auc",
                          objective="binary:logistic",
                          verbose = 0)
      
      test_mt<-read_svmlight(paste0('./data/preproc/',pred_task,'_',pred_in_d,'d_',fs_type,'_test_svmlite.txt'))
      dtest<-xgb.DMatrix(data=test_mt$x[,pred_idx],label=test_mt$y)
      valid<-data.frame(auxRow %>% filter(part_idx=="V") %>% select(-row_idx),
                        pred = predict(xgb_tune,dtest),
                        stringsAsFactors = F)
      
      #--feature importance
      feat_imp<-xgb.importance(auxCol$key[!auxCol$key %in% c("fold")],model=xgb_tune)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"validate model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model validating.\n")
      
      #-----------------------
      #--save model and other results
      result<-list(hyper_full=OPT_Res$scoreSummary,
                   hyper_opt=c(Best_Par,steps),
                   model=xgb_tune,
                   valid=valid,
                   feat_imp=feat_imp)
      
      saveRDS(result,file=paste0("./data/model_ref/",pred_in_d,"d_",pred_task,"_",fs_type,".rda"))
      
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

