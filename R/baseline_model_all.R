#### survival-like prediction model ####
rm(list=ls()); gc()

source("./R/util.R")
source("./R/var_etl_surv.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "Matrix",
                    "xgboost",
                    "ROCR",
                    "PRROC"))

#choose task parameters
#-----prediction point
pred_in_d<-1
# pred_in_d<-2

#-----feature selection type
fs_type<-"no_fs"
# fs_type<-"rm_scr_bun"
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '3097-3','44734-2','BUN_SCR')

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----whether values should be carried over?
carry_over<-T

############################## baseline GBM model ######################################
for(pred_task in pred_task_lst){
  bm<-c()
  bm_nm<-c()
  
  start_tsk<-Sys.time()
  cat("Start build reference model for task",pred_task,".\n")
  #---------------------------------------------------------------------------------------------
  
  start_tsk_i<-Sys.time()
  #--prepare training and testing set
  yr_rg<-seq(2010,2018)
  X_tr<-c()
  X_ts<-c()
  y_tr<-c()
  y_ts<-c()
  rsample_idx<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_rsample_idx_",pred_task,"_co",carry_over,".rda")) %>%
    dplyr::mutate(cv5_idx=ceiling(cv10_idx/2))
    
  for(i in seq_along(yr_rg)){
    var_by_yr<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_var_by_yr_",pred_task,"_co",carry_over,".rda"))[[i]]
    
    X_tr %<>% bind_rows(var_by_yr[["X_surv"]])
    y_tr %<>% bind_rows(var_by_yr[["y_surv"]]) %>%
      left_join(rsample_idx,by="ENCOUNTERID")

    cat("...finish stack data of encounters from",yr_rg[i],".\n")
  }
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"prepare data")
  
  start_tsk_i<-Sys.time()
  #--pre-filter
  if(fs_type=="rm_scr_bun"){
    X_tr %<>%
      filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
  }
  
  #--transform training matrix
  y_tr %<>%
    filter(!is.na(cv10_idx)) %>%
    arrange(ENCOUNTERID,dsa_y) %>%
    unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
    arrange(ROW_ID) %>%
    unique
  
  X_tr %<>%
    arrange(ENCOUNTERID,dsa_y) %>%
    unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
    semi_join(y_tr,by="ROW_ID") %>%
    long_to_sparse_matrix(df=.,
                          id="ROW_ID",
                          variable="key",
                          val="value")
  
  #--collect variables used in training
  tr_key<-data.frame(key = unique(colnames(X_tr)),
                     stringsAsFactors = F)

  
  #check alignment
  all(row.names(X_tr)==y_tr$ROW_ID)

  #--covert to xgb data frame
  dtrain<-xgb.DMatrix(data=X_tr,label=y_tr$y)

  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"transform data")
  
  cat("...finish formatting training and testing sets.\n")
  
  start_tsk_i<-Sys.time()
  #--get indices for k folds
  y_tr %<>% dplyr::mutate(row_idx = 1:n())
  folds<-list()
  for(fd in seq_len(max(y_tr$cv5_idx))){
    fd_df<-y_tr %>% 
      filter(cv5_idx==fd) %>%
      dplyr::select(row_idx)
    folds[[fd]]<-fd_df$row_idx
  }
  
  #--tune hyperparameter
  #hyper-parameter grid for xgboost
  eval_metric<-"auc"
  objective<-"binary:logistic"
  grid_params<-expand.grid(
    max_depth=c(4,10),
    # max_depth=10,
    # eta=c(0.3,0.1,0.01),
    eta=0.02,
    min_child_weight=c(1,10),
    subsample=0.8,
    colsample_bytree=0.8, 
    gamma=1
  )
  
  verb<-TRUE
  bst_grid<-c()
  bst_grid_cv<-c()
  metric_name<-paste0("test_", eval_metric,"_mean")
  metric_sd_name<-paste0("test_", eval_metric,"_std")
  
  for(i in seq_len(dim(grid_params)[1])){
    start_i<-Sys.time()
    param<-as.list(grid_params[i,])
    # param$scale_pos_weight=mean(train$y_train$DKD_IND_additive) #inbalance sampling
    param$scale_pos_weight=1 #balance sampling
    
    bst <- xgb.cv(param,
                  dtrain,
                  objective = objective,
                  metrics = eval_metric,
                  maximize = TRUE,
                  nrounds=2000,
                  # nfold = 5,
                  folds = folds,
                  early_stopping_rounds = 100,
                  print_every_n = 100,
                  prediction = T) #keep cv results
    
    bst_grid<-rbind(bst_grid, cbind(grid_params[i,],
                                    metric=max(bst$evaluation_log[[metric_name]]),
                                    steps=which(bst$evaluation_log[[metric_name]]==max(bst$evaluation_log[[metric_name]]))[1]))
    
    bst_grid_cv<-cbind(bst_grid_cv,bst$pred)
    
    if(verb){
      cat('...finished train case:',paste0(paste0(c(colnames(grid_params),"scale_pos_weight"),"="),param,collapse="; "),
          'in',Sys.time()-start_i,units(Sys.time()-start_i),"\n")
      start_i<-Sys.time()
    }
  }
  hyper_param<-bst_grid[which.max(bst_grid$metric),]
  
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"tune model")
  
  cat("...finish model tunning.\n")

  start_tsk_i<-Sys.time()  
  #--validation
  xgb_tune<-xgb.train(data=dtrain,
                      max_depth=hyper_param$max_depth,
                      maximize = TRUE,
                      eta=hyper_param$eta,
                      nrounds=hyper_param$steps,
                      eval_metric="auc",
                      objective="binary:logistic",
                      print_every_n = 100)
  
  #--feature importance
  feat_imp<-xgb.importance(colnames(X_tr),model=xgb_tune)
  
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"validate model")
  
  cat("...finish model validating.\n")
  
  #--save model and other results
  saveRDS(xgb_tune,file=paste0("./data_local/model_ref2/pred_in_",pred_in_d,"d_model_gbm_",fs_type,"_",pred_task,".rda"))
  saveRDS(bst_grid,file=paste0("./data_local/model_ref2/pred_in_",pred_in_d,"d_hyperpar_gbm_",fs_type,"_",pred_task,".rda"))
  saveRDS(feat_imp,file=paste0("./data_local/model_ref2/pred_in_",pred_in_d,"d_varimp_gbm_",fs_type,"_",pred_task,".rda"))
  
  #-------------------------------------------------------------------------------------------------------------
  lapse_tsk<-Sys.time()-start_tsk
  bm<-c(bm,paste0(lapse_tsk,units(lapse_tsk)))
  bm_nm<-c(bm_nm,"complete task")
  
  cat("\nFinish building reference models for task",pred_task,"in",lapse_tsk,units(lapse_tsk),".\n")
  
  #benchmark
  bm<-data.frame(bm_nm=bm_nm,bm_time=bm,
                 stringsAsFactors = F)
  saveRDS(bm,file=paste0("./data_local/model_ref2/pred_in_",pred_in_d,"d_bm_gbm_",fs_type,"_",pred_task,".rda"))
}

