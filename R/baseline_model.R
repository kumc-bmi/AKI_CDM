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
                    # "CMake",
                    # "LightGBM",
                    # "catboost",
                    "ROCR",
                    "PRROC"))

#choose task parameters
#-----prediction point
pred_in_d<-1
# pred_in_d<-2
# pred_in_d<-3

#-----feature selection type
# fs_type<-"no_fs"
fs_type<-"rm_scr_bun"
rm_key<-c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9',
          '12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
          '11065-0','3094-0','35234-4','14937-7',
          '3097-3','44734-2','BUN_SCR')

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

############################## collect and format variables on daily basis ######################
tbl1<-readRDS("./data_local/data_raw/Table1.rda") %>%
  dplyr::mutate(yr=as.numeric(format(ADMIT_DATE,"%Y")))

onset_dt<-c(tbl1$AKI1_SINCE_ADMIT,tbl1$AKI2_SINCE_ADMIT,tbl1$AKI3_SINCE_ADMIT)
quantile(onset_dt,probs=0:20/20,na.rm=T)
pred_end<-quantile(onset_dt,probs=0.7,na.rm=T)
tw<-as.double(seq(0,pred_end)) 
if(pred_in_d>1){
  tw<-tw[-seq_len(pred_in_d-1)]
} 

#--by chunks: encounter year
enc_yr<-tbl1 %>%
  dplyr::select(yr) %>%
  unique %>% arrange(yr) %>%
  filter(yr>2009) %>%
  unlist

#--by variable type
var_type<-c("demo","vital","lab","dx","px","med")

#--save results as array
for(pred_task in pred_task_lst){
  start_tsk<-Sys.time()
  cat("Start variable collection for task",pred_task,".\n")
  #---------------------------------------------------------------------------------------------
  
  var_by_yr<-list()
  var_bm<-list()
  rsample_idx<-c()
  for(i in seq_along(enc_yr)){
    start_i<-Sys.time()
    cat("...start variable collection for year",enc_yr[i],".\n")
    
    #--collect end_points
    dat_i<-tbl1 %>% filter(yr==enc_yr[i]) %>%
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
        # filter(y!=1) %>% # remove stage 1
        dplyr::mutate(y=as.numeric(y>1)) %>%
        group_by(ENCOUNTERID) %>% top_n(n=1L,wt=dsa_y) %>% ungroup
    }else if(pred_task=="stg3"){
      dat_i %<>%
        # filter(!(y %in% c(1,2))) %>% # remove stage 1,2
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
      var_v<-readRDS(paste0("./data_local/data_raw/AKI_",toupper(var_type[v]),".rda")) %>%
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
      cat("\n......finished ETL",var_type[v],"for year",enc_yr[i],"in",lapse_v,units(lapse_v),".\n")
    }
    var_by_yr[[i]]<-list(X_surv=X_surv,
                         y_surv=y_surv)
    
    lapse_i<-Sys.time()-start_i
    var_etl_bm<-c(var_etl_bm,paste0(lapse_i,units(lapse_i)))
    cat("\n...finished variabl collection for year",enc_yr[i],"in",lapse_i,units(lapse_i),".\n")
    
    var_bm[[i]]<-data.frame(bm_nm=c(var_type,"overall"),
                            bm_time=var_etl_bm,
                            stringsAsFactors = F)
  }
  #--save preprocessed data
  saveRDS(rsample_idx,file=paste0("./data_local/data_preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
  saveRDS(var_by_yr,file=paste0("./data_local/data_preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))
  saveRDS(var_bm,file=paste0("./data_local/data_preproc/",pred_in_d,"d_var_bm",pred_task,".rda"))
  
  #---------------------------------------------------------------------------------------------
  lapse_tsk<-Sys.time()-start_tsk
  cat("\nFinish variable ETL for task",pred_task,"in",lapse_tsk,units(lapse_tsk),".\n")
}


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
  rsample_idx<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_rsample_idx_",pred_task,".rda"))
  for(i in seq_along(yr_rg)){
    var_by_yr<-readRDS(paste0("./data_local/data_preproc/",pred_in_d,"d_var_by_yr_",pred_task,".rda"))[[i]]
    
    X_tr %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
      semi_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                by="ENCOUNTERID")
    
    y_tr %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                          left_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                                    by="ENCOUNTERID"))
    
    X_ts %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
      semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                by="ENCOUNTERID")
    
    y_ts %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                          left_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                                    by="ENCOUNTERID"))
    
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
    
    X_ts %<>%
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
  
  #--transform testing matrix
  y_ts %<>%
    filter(!is.na(cv10_idx)) %>%
    arrange(ENCOUNTERID,dsa_y) %>%
    unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
    arrange(ROW_ID) %>%
    unique
  
  X_ts %<>% 
    unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
    semi_join(y_ts,by="ROW_ID") %>%
    semi_join(tr_key,by="key")
  
  x_add<-tr_key %>%
    anti_join(data.frame(key = unique(X_ts$key),
                         stringsAsFactors = F),
              by="key")
  
  #align with training
  if(nrow(x_add)>0){
    X_ts %<>%
      arrange(ROW_ID) %>%
      bind_rows(data.frame(ROW_ID = rep("0_0",nrow(x_add)),
                           dsa = -99,
                           key = x_add$key,
                           value = 0,
                           stringsAsFactors=F))
  }
  X_ts %<>%
    long_to_sparse_matrix(df=.,
                          id="ROW_ID",
                          variable="key",
                          val="value")
  if(nrow(x_add)>0){
    X_ts<-X_ts[-1,]
  }
  
  #check alignment
  all(row.names(X_tr)==y_tr$ROW_ID)
  all(row.names(X_ts)==y_ts$ROW_ID)
  all(colnames(X_tr)==colnames(X_ts))
  
  #--covert to xgb data frame
  dtrain<-xgb.DMatrix(data=X_tr,label=y_tr$y)
  dtest<-xgb.DMatrix(data=X_ts,label=y_ts$y)
  
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"transform data")
  
  cat("...finish formatting training and testing sets.\n")
  
  start_tsk_i<-Sys.time()
  #--get indices for k folds
  y_tr %<>% dplyr::mutate(row_idx = 1:n())
  folds<-list()
  for(fd in seq_len(max(y_tr$cv10_idx))){
    fd_df<-y_tr %>% 
      filter(cv10_idx==fd) %>%
      dplyr::select(row_idx)
    folds[[fd]]<-fd_df$row_idx
  }
  
  #--tune hyperparameter
  #hyper-parameter grid for xgboost
  eval_metric<-"auc"
  objective<-"binary:logistic"
  grid_params<-expand.grid(
    max_depth=c(4,6,10),
    eta=c(0.3,0.1,0.01),
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
  
  valid<-data.frame(y_ts,
                    pred = predict(xgb_tune,dtest),
                    stringsAsFactors = F)
  
  #--feature importance
  feat_imp<-xgb.importance(colnames(X_tr),model=xgb_tune)
  
  lapse_i<-Sys.time()-start_tsk_i
  bm<-c(bm,paste0(lapse_i,units(lapse_i)))
  bm_nm<-c(bm_nm,"validate model")
  
  cat("...finish model validating.\n")
  
  #--save model and other results
  saveRDS(xgb_tune,file=paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_model_gbm_",fs_type,"_",pred_task,".rda"))
  saveRDS(bst_grid,file=paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_hyperpar_gbm_",fs_type,"_",pred_task,".rda"))
  saveRDS(valid,file=paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_valid_gbm_",fs_type,"_",pred_task,".rda"))
  saveRDS(feat_imp,file=paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_varimp_gbm_",fs_type,"_",pred_task,".rda"))
  
  #-------------------------------------------------------------------------------------------------------------
  lapse_tsk<-Sys.time()-start_tsk
  bm<-c(bm,paste0(lapse_tsk,units(lapse_tsk)))
  bm_nm<-c(bm_nm,"complete task")
  
  cat("\nFinish building reference models for task",pred_task,"in",lapse_tsk,units(lapse_tsk),".\n")
  
  #benchmark
  bm<-data.frame(bm_nm=bm_nm,bm_time=bm,
                 stringsAsFactors = F)
  saveRDS(bm,file=paste0("./data_local/model_ref/pred_in_",pred_in_d,"d_bm_gbm_",fs_type,"_",pred_task,".rda"))
}

############################## benchmark performance ##############################
#-----prediction point
# pred_in_d<-1
pred_in_d<-2
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

