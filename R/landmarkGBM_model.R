#### temporal model ####
rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "Matrix",
                    "xgboost",
                    # "CMake",
                    # "LightGBM",
                    # "catboost",
                    "ROCR",
                    "PRROC"))

#choose task parameters
#-----prediction point
pred_in_d<-1

#-----feature selection type
fs_type<-"no_fs"
# fs_type<-"rm_scr_bun"
# rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
#           '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
#           '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
#           '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
#           '11065-0','3094-0','35234-4','14937-7',
#           '3097-3','44734-2','BUN_SCR')

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----update frequency
time_iterv<-1

#----follow-up period
period<-4

#----keep past predictions
past<-TRUE

############################## Landmark GBM model ######################################
tr_idx<-sample(1:10,6) #for resampling
for(pred_task in pred_task_lst){
  bm<-c()
  bm_nm<-c()
  
  start_tsk<-Sys.time()
  cat("Start build landmark GBM model for task",pred_task,".\n")
  #---------------------------------------------------------------------------------------------
  
  start_tsk_i<-Sys.time()
  #--prepare training and testing set
  yr_rg<-seq(2010,2018)
  X_tr<-c()
  X_ts<-c()
  y_tr<-c()
  y_ts<-c()
  feat_sel<-readRDS(paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_varimp_gbm_",fs_type,"_",pred_task,".rda"))
  rsample_idx<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
  for(i in seq_along(yr_rg)){
    var_by_yr<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))[[i]]
    
    X_tr %<>% bind_rows(var_by_yr[["X_surv"]] %>%
                          semi_join(rsample_idx %>% filter(cv10_idx %in% tr_idx & yr<2017),
                                    by="ENCOUNTERID") %>%
                          semi_join(feat_sel,by=c("key"="Feature")))
    
    y_tr %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                          inner_join(rsample_idx %>% filter(cv10_idx %in% tr_idx & yr<2017),
                                    by="ENCOUNTERID"))
    
    X_ts %<>% bind_rows(var_by_yr[["X_surv"]] %>%
                          semi_join(rsample_idx %>% filter(!(cv10_idx %in% tr_idx) | yr>=2017),
                                    by="ENCOUNTERID") %>%
                          semi_join(feat_sel,by=c("key"="Feature")))
    
    y_ts %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                          inner_join(rsample_idx %>% filter(!(cv10_idx %in% tr_idx) | yr>=2017),
                                    by="ENCOUNTERID"))
    
    cat("...finish stack data of encounters from",yr_rg[i],".\n")
  }
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"prepare data")
  
  #--pre-filter
  if(fs_type=="rm_scr_bun"){
    X_tr %<>%
      filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
    
    X_ts %<>%
      filter(!(key %in% c(rm_key,paste0(rm_key,"_change"))))
  }

  #--build landmark temporal model
  model_ep<-list()    #hold time-specific models
  model_roc<-list()   #hold time-specific model performance
  for(ep in 0:period){
    start_ep<-Sys.time()
    
    start_tsk_i<-Sys.time()
    
    #--transform training matrix
    if(past){
      y_tr_ep<-y_tr %>%
        filter(dsa_y<=ep) %>% # target outcome at ep
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      if(ep>0){
        X_tr_ep<-X_tr %>% 
          bind_rows(X_ts %>% filter(dsa_y<ep & dsa < ep-1))
      }
      
    }else{
      y_tr_ep<-y_tr %>%
        filter(!is.na(cv10_idx)) %>% # remove learned knowledge for testing points
        filter(dsa_y==ep) %>% # target outcome at ep
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
    }
    
    X_tr_ep<-X_tr %>%
      filter(dsa_y<=ep & dsa < ep) %>% # predictors strictly before ep
      unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
      semi_join(y_tr_ep,by="ROW_ID") %>%
      long_to_sparse_matrix(df=.,
                            id="ROW_ID",
                            variable="key",
                            val="value")
    
    #--collect variables used in training
    tr_key<-data.frame(key = unique(colnames(X_tr_ep)),
                       stringsAsFactors = F)
    
    #--transform testing matrix
    y_ts_ep<-y_ts %>%
      filter(!is.na(cv10_idx)) %>%
      filter(dsa_y==ep) %>% 
      unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
      arrange(ROW_ID) %>%
      unique
    
    X_ts_ep<-X_ts %>% 
      filter(dsa < ep) %>% 
      unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
      semi_join(y_ts_ep,by="ROW_ID") %>%
      semi_join(tr_key,by="key")
    
    x_add<-tr_key %>%
      anti_join(data.frame(key = unique(X_ts_ep$key),
                           stringsAsFactors = F),
                by="key")
    
    #align with training
    if(nrow(x_add)>0){
      X_ts_ep %<>%
        arrange(ROW_ID) %>%
        bind_rows(data.frame(ROW_ID = rep("0_0",nrow(x_add)),
                             dsa = -99,
                             key = x_add$key,
                             value = 0,
                             stringsAsFactors=F))
    }
    X_ts_ep %<>%
      long_to_sparse_matrix(df=.,
                            id="ROW_ID",
                            variable="key",
                            val="value")
    if(nrow(x_add)>0){
      X_ts_ep<-X_ts_ep[-1,]
    }
    
    #check alignment
    all(row.names(X_tr_ep)==y_tr_ep$ROW_ID)
    all(row.names(X_ts_ep)==y_ts_ep$ROW_ID)
    all(colnames(X_tr_ep)==colnames(X_ts_ep))
    
    #--covert to xgb data frame
    dtrain<-xgb.DMatrix(data=X_tr_ep,label=y_tr_ep$y)
    dtest<-xgb.DMatrix(data=X_ts_ep,label=y_ts_ep$y)
    
    lapse_i<-Sys.time()-start_tsk_i
    bm<-c(bm,paste0(lapse_i,units(lapse_i)))
    bm_nm<-c(bm_nm,paste0(ep,"_",past,"_transform data"))
    
    cat(ep,past,"...finish formatting training and testing sets.\n")
    
    if(ep>0){
      os<-data.frame(ROW_ID=y_tr_ep$ROW_ID,stringsAsFactors = F) %>%
        left_join(model_roc[[paste0((ep-1),"d_since_adm_",past)]] %>% 
                    dplyr::select(ROW_ID,pred),
                  by="ROW_ID") %>%
        dplyr::mutate(pred_imp=mean(pred,na.rm=T)) %>%
        dplyr::mutate(offset=ifelse(is.na(pred),
                                    log(pred_imp/(1-pred_imp)),
                                    log(pred/(1-pred))))
      
      setinfo(dtrain,"base_margin",os$offset) #use last prediction as baseline
    }
    
    lapse_i<-Sys.time()-start_i
    cat(ep,past,"...finish handling temporal data in",lapse_i,units(lapse_i),"\n")
    bm<-c(bm,paste0(round(lapse_i,4),units(lapse_i)))
    bm_nm<-c(bm_nm,paste0(ep,"_",past,"_build temporal traning set"))
    
    #==get indices for k folds
    folds<-list()
    train_dt<-y_tr_ep %>%
      dplyr::select(ROW_ID,cv10_idx) %>% unique %>%
      dplyr::mutate(row_idx = 1:n())
    
    for(fd in seq_along(unique(train_dt$cv10_idx))){
      fd_df<-train_dt %>% 
        filter(cv10_idx==as.character(unique(train_dt$cv10_idx)[fd])) %>%
        dplyr::select(row_idx)
      folds[[fd]]<-fd_df$row_idx
    }
    
    #==tune hyperparameter
    start_tsk_i<-Sys.time()
    
    #hyper-parameter grid for xgboost
    eval_metric<-"auc"
    objective<-"binary:logistic"
    grid_params_tree<-expand.grid(
      # max_depth=8,
      max_depth=c(4,6,8,10),
      eta=0.02,
      # eta=c(0.02,0.01),
      min_child_weight=1,
      subsample=0.8,
      colsample_bytree=0.8, 
      gamma=1
    )
    
    verb<-TRUE
    bst_grid<-c()
    bst_grid_cv<-c()
    metric_name<-paste0("test_", eval_metric,"_mean")
    metric_sd_name<-paste0("test_", eval_metric,"_std")
    # grid_params<-grid_params_reg
    grid_params<-grid_params_tree
    
    for(i in seq_len(dim(grid_params)[1])){
      start_i<-Sys.time()
      param<-as.list(grid_params[i,])
      # param$scale_pos_weight=mean(y_tr_ep$y) #inbalance sampling
      param$scale_pos_weight=1 #balance sampling
      
      bst <- xgb.cv(param,
                    dtrain,
                    objective = objective,
                    metrics = eval_metric,
                    maximize = TRUE,
                    nrounds=1000,
                    # nfold = 5,
                    folds = folds,
                    early_stopping_rounds = 50,
                    print_every_n = 100,
                    prediction = T) #keep cv results
      
      bst_grid<-rbind(bst_grid, cbind(grid_params[i,],
                                      metric=max(bst$evaluation_log[[metric_name]]),
                                      steps=which(bst$evaluation_log[[metric_name]]==max(bst$evaluation_log[[metric_name]]))[1]))
      
      bst_grid_cv<-cbind(bst_grid_cv,bst$pred)
      
      if(verb){
        cat(ep,past,'......finished train case:',paste0(paste0(c(colnames(grid_params),"scale_pos_weight"),"="),param,collapse="; "),
            'in',Sys.time()-start_i,units(Sys.time()-start_i),"\n")
        start_i<-Sys.time()
      }
    }
    hyper_param<-bst_grid[which.max(bst_grid$metric),]
    
    valid_cv<-data.frame(ROW_ID = y_tr_ep$ROW_ID,
                         pred = bst_grid_cv[,which.max(bst_grid$metric)],
                         real = getinfo(dtrain,"label"),
                         stringsAsFactors = F)
    
    lapse_i<-Sys.time()-start_tsk_i
    cat(ep,past,"...finish tunning hyperparameter in",lapse_i,units(lapse_i),"\n")
    bm<-c(bm,paste0(round(lapse_i,4),units(lapse_i)))
    bm_nm<-c(bm_nm,paste0(ep,"_",past,"_tune hyperparameters"))
    
    ##==re-train and validate model
    start_tsk_i<-Sys.time()
    
    xgb_tune<-xgb.train(data=dtrain,
                        max_depth=hyper_param$max_depth,
                        maximize = TRUE,
                        eta=hyper_param$eta,
                        nrounds=hyper_param$steps,
                        # watchlist=watchlist,
                        eval_metric="auc",
                        objective="binary:logistic",
                        print_every_n = 100)
    
    valid<-data.frame(ROW_ID = y_ts_ep$ROW_ID,
                      pred = predict(xgb_tune,newdata=dtest),
                      real = getinfo(dtest,"label"),
                      stringsAsFactors = F)
    
    lapse_i<-Sys.time()-start_tsk_i
    cat(ep,past,"...finish validating in",lapse_i,units(lapse_i),"\n")
    bm<-c(bm,paste0(round(lapse_i,4),units(lapse_i)))
    bm_nm<-c(bm_nm,paste0(ep,"_",past,"_validation"))
    
    ##==save results
    model_ep[[paste0(ep,"d_since_adm_",past)]]<-xgb_tune
    model_roc[[paste0(ep,"d_since_adm_",past)]]<-valid_cv %>% mutate(val_type="cv") %>% 
      bind_rows(valid %>% mutate(val_type="holdout"))
    
    lapse_ep<-Sys.time()-start_ep
    cat(ep,past,"...finish modeling in",lapse_ep,units(lapse_ep),"\n")
    bm<-c(bm,paste0(round(lapse_ep,4),units(lapse_ep)))
    bm_nm<-c(bm_nm,paste0(ep,"_",past,"_validation"))
  }
  
  #---------------------------------------------------------------------------------------------
  lapse_tsk<-Sys.time()-start_tsk
  cat("\nFinish modeling for task",pred_task,"in",lapse_tsk,units(lapse_tsk),".\n")
  bm<-c(bm,paste0(round(lapse_tsk,4),units(lapse_tsk)))
  bm_nm<-c(bm_nm,"full_task")
  
  out<-list(model_ep = model_ep,
            model_roc = model_roc,
            bm = data.frame(bm_nm=bm_nm,bm=bm,stringsAsFactors=T))
  
  saveRDS(out,file=paste0("./data_local/",
                          pred_in_d,"_",
                          fs_type,"_",
                          pred_task,"_",
                          past,"_LMgbm.rda"))
}


############################## benchmark performance ##############################
#-----prediction point
pred_in_d<-1
# pred_in_d<-2
# pred_in_d<-3

#-----feature selection type
fs_type<-"no_fs"
# fs_type<-"rm_scr_bun"

bm<-c()
for(pred_task in c("stg1up","stg2up","stg3")){
  proc_bm<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_var_bm",pred_task,".rda"))
  bm2<-c()
  for(i in seq_along(seq(2010,2018))){
    bm2 %<>%
      bind_rows(proc_bm[[i]] %>% 
                  filter(bm_nm=="overall") %>%
                  dplyr::mutate(bm_nm=paste0(bm_nm,"_",seq(2010,2018)[i])))
  }
  
  bm %<>% 
    bind_rows(bind_rows(bm2,
                        readRDS(paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_bm_gbm_",fs_type,"_",pred_task,".rda"))) %>%
                dplyr::mutate(outcome=pred_task))
}
bm %<>% spread(outcome,bm_time)

