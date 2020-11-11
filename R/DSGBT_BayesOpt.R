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
# pred_in_d_opt<-c(2,1)
pred_in_d_opt<-2

#-----prediction tasks
pred_task_lst<-c("stg2up",
                 "stg1up",
                 "stg3")
# pred_task_lst<-"stg2up"

#-----feature selection type
# fs_type_opt<-c("full","reduced")
fs_type_opt<-"full"

#-----model spec
bounds <- list(
  max_depth = c(4L, 10L),
  min_child_weight = c(2L,10L),
  # subsample = c(0.5,0.8),
  colsample_bytree=c(0.3,0.8)
  # eta=c(0.05,0.1)
)

#-----number of data chunks
for(pred_in_d in pred_in_d_opt){
  
  for(pred_task in pred_task_lst){
    bm<-c()
    bm_nm<-c()
    
    start_tsk<-Sys.time()
    cat("Start build reference model for task",pred_task,"in",pred_in_d,"days",".\n")
    #---------------------------------------------------------------------------------------------
    
    for(fs_type in fs_type_opt){
      #-----------training------------
      start_tsk_i<-Sys.time()
      
      auxCol<-read.csv(paste0('./data/preproc/',pred_task,'_',pred_in_d,'d_',fs_type,'_auxCol_svmlite.csv'),stringsAsFactors = F)
      pred_idx<-which(!auxCol$key %in% c("fold"))
      train_mt<-read_svmlight(paste0('./data/preproc/',pred_task,'_',pred_in_d,'d_',fs_type,'_train_svmlite.txt'),
                              zero_based = FALSE)
      
      #--get indices for k folds
      auxRow<-read.csv(paste0('./data/preproc/',pred_task,'_',pred_in_d,'d_',fs_type,'_auxRow.csv'),stringsAsFactors = F)
      folds<-list()
      for(fd in seq_len(max(auxRow$cv10_idx))){
        fd_df<-auxRow %>% 
          filter(cv10_idx==fd&!is.na(row_idx)) %>%
          dplyr::select(row_idx)
        folds[[fd]]<-fd_df$row_idx
      }
      folds<-folds[length(folds)>0]
      
      #--parallelization
      cl <- makeCluster(N_CL)
      registerDoParallel(cl)
      clusterExport(cl,c('folds','train_mt','pred_idx')) # copying data to clusters (note:xgb.DMatrix is not compatible with parallelization)
      clusterEvalQ(cl,expr= {                            # copying model to clusters
        library(xgboost)
      })
      
      #--tune hyperparameter (less rounds, early stopping)
      xgb_cv_bayes <- function(max_depth=10L, min_child_weight=1L, subsample=0.7,
                               eta=0.05,colsample_bytree=0.8,lambda=1,alpha=0,gamma=1) {
        
        dtrain<-xgb.DMatrix(data=train_mt$x[,pred_idx],label=as.matrix(train_mt$y))
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
        otherHalting = list(timeLimit = 18000) #--limit maximal running time for better efficiency-- <5hr
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

