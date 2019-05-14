#' ---
#' title: "Building and Validating Predictive Models for Acute Kidney Injury (AKI) using PCORnet CDM (Part II.1)"
#' author: "xing song"
#' date: "Feburary 09, 2019"
#' output: html_document
#' ---
#' ### Stage 2.2: Predictive Models Validation (Retrain)
#' 
#' In this experiment, we will retrain the benchmark predictive model by quasi-replicating the model by [*Koyner et al*] for AKI risk prediction on the adult inpatients at each GPC site using PCORnet common data model. The model will be trained on 70% of the site's local data and validated on the remaining 30%.
#' 
#' [*Koyner et al*] https://www.ncbi.nlm.nih.gov/pubmed/29596073

#source utility functions
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
                    "PRROC",
                    "ResourceSelection",
                    "knitr",
                    "kableExtra",
                    "ggplot2",
                    "openxlsx"))


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
          '3097-3','44734-2','BUN_SCR')


#' #### Preparation    
#' 
#' By running `Part I` of "render_report.R", the raw data tables should have already been collected and saved in the local `./data` folder (Note: these data tables are not visiable in the github ./data folder, but should be visible in the corresponding folder locally), that are
#' 
#' * `Table1.rda`: AKI patieht cohort table;       
#' 
#' * `AKI_DEMO.rda`: CDM demographic table cut for AKI cohort;   
#' 
#' * `AKI_VITAL.rda`: CDM vital table cut for AKI cohort;    
#' 
#' * `AKI_LAB.rda`: CDM lab table cut for AKI cohort;    
#' 
#' * `AKI_DX.rda`: CDM diagnosis table cut for AKI cohort;   
#' 
#' * `AKI_PX.rda`: CDM procedure table cut for AKI cohort;   
#' 
#' * `AKI_MED.rda`: CDM prescribing medication table cut for AKI cohort;   


#' #### Objective 2.1: Data Cleaning and Representation

#' In this section, the raw data tables will be cleaned and transformed to a discrete-survival-like representation, which will be used in the final modeling stage. To reduce the burden on memory requirments, the ETL (extract, transform, load) process will be performed in chunks with respect to **distinct prediction task, encounter years and variable types**. Meanwhile, indices for random paritioning will be assigned to each encounter. The ETL progress will be reported as follows:


# collect and format variables on daily basis 
n_chunk<-4

tbl1<-readRDS("./data//Table1.rda") %>%
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


# The final preprocessed intermediate tables from this code chunk should be found in the `./data/preproc/...` folder as the following intermediate data tables for different prediction tasks:   
# 
# * For AKI stage ≥ 1 in 24 hours: `1d_rsample_idx_stg1up.rda`, `1d_var_by_yr_stg1up.rda`, `1d_var_bm_stg1up.rda`   
# 
# * For AKI stage ≥ 2 in 24 hours: `1d_rsample_idx_stg2up.rda`, `1d_var_by_yr_stg2up.rda`, `1d_var_bm_stg2up.rda`   
#  
# * For AKI stage = 3 in 24 hours: `1d_rsample_idx_stg3.rda`, `1d_var_by_yr_stg3.rda`, `1d_var_bm_stg3.rda`   
# 
# * For AKI stage ≥ 1 in 48 hours: `2d_rsample_idx_stg1up.rda`, `2d_var_by_yr_stg1up.rda`, `1d_var_bm_stg1up.rda`   
#  
# * For AKI stage ≥ 2 in 48 hours: `2d_rsample_idx_stg2up.rda`, `2d_var_by_yr_stg2up.rda`, `1d_var_bm_stg2up.rda`   
#  
# * For AKI stage = 3 in 48 hours: `2d_rsample_idx_stg3.rda`, `2d_var_by_yr_stg3.rda`, `1d_var_bm_stg3.rda`   
#  



#' #### Objective 2.2: Benchmark Model Development
#' 
#' We will adopt the AKI prediction model by [*Koyner et al*] using all variables from each site's CDM Demographic, Vital, Diagnosis, Procedure and Prescribing Medication tables.The same strategy as in Koyner et al for outlier removal and aggregation of repeated values have been followed. Training/Validation sets are partitioned based on pre-assigned indices in the files "..._rsample_idx_..." from previous part.  The model development progress will be reported as follows:

#hyper-parameter grid for xgboost
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params<-expand.grid(
  max_depth=10,
  eta=c(0.05,0.01),
  min_child_weight=1,
  subsample=0.8,
  colsample_bytree=0.8, 
  gamma=1
)

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
                            left_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                                      by="ENCOUNTERID"))
      
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
      y_tr %<>%
        filter(!is.na(cv10_idx)) %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      X_tr_sp<-X_tr %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        semi_join(y_tr,by="ROW_ID") %>%
        long_to_sparse_matrix(df=.,
                              id="ROW_ID",
                              variable="key",
                              val="value")
      
      #--collect variables used in training
      tr_key<-data.frame(key = unique(colnames(X_tr_sp)),
                         stringsAsFactors = F)
      
      #--transform testing matrix
      y_ts %<>%
        filter(!is.na(cv10_idx)) %>%
        arrange(ENCOUNTERID,dsa_y) %>%
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        arrange(ROW_ID) %>%
        unique
      
      X_ts_sp<-X_ts %>% 
        unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
        semi_join(y_ts,by="ROW_ID") %>%
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
      if(!all(row.names(X_tr_sp)==y_tr$ROW_ID)){
        stop("row ids of traning set don't match!")
      }
      if(!all(row.names(X_ts_sp)==y_ts$ROW_ID)){
        stop("row ids of testing set don't match!")
      }
      if(!all(colnames(X_tr_sp)==colnames(X_ts_sp))){
        stop("feature names don't match!")
      }
      
      #--covert to xgb data frame
      dtrain<-xgb.DMatrix(data=X_tr_sp,label=y_tr$y)
      dtest<-xgb.DMatrix(data=X_ts_sp,label=y_ts$y)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"transform data")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish formatting training and testing sets.\n")
      
      #-----------------------
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
                      nrounds=1000,
                      # nfold = 5,
                      folds = folds,
                      early_stopping_rounds = 50,
                      print_every_n = 50,
                      prediction = T) #keep cv results
        
        bst_grid<-rbind(bst_grid, cbind(grid_params[i,],
                                        metric=max(bst$evaluation_log[[metric_name]]),
                                        steps=which(bst$evaluation_log[[metric_name]]==max(bst$evaluation_log[[metric_name]]))[1]))
        
        bst_grid_cv<-cbind(bst_grid_cv,bst$pred)
        
        if(verb){
          cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
              '...finished train case:',paste0(paste0(c(colnames(grid_params),"scale_pos_weight"),"="),param,collapse="; "),
              'in',Sys.time()-start_i,units(Sys.time()-start_i),"\n")
          start_i<-Sys.time()
        }
      }
      hyper_param<-bst_grid[which.max(bst_grid$metric),]
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"tune model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model tunning.\n")
      
      #-----------------------
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
      feat_imp<-xgb.importance(colnames(X_tr_sp),model=xgb_tune)
      
      lapse_i<-Sys.time()-start_tsk_i
      bm<-c(bm,paste0(round(lapse_i,1),units(lapse_i)))
      bm_nm<-c(bm_nm,"validate model")
      
      cat(paste0(c(pred_in_d,pred_task,fs_type),collapse = ","),
          "...finish model validating.\n")
      
      #-----------------------
      #--save model and other results
      result<-list(hyper=bst_grid,
                   model=xgb_tune,
                   valid=valid,
                   feat_imp=feat_imp)
      saveRDS(result,file=paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))

      #-------------------------------------------------------------------------------------------------------------
      lapse_tsk<-Sys.time()-start_tsk
      bm<-c(bm,paste0(round(lapse_tsk,1),units(lapse_tsk)))
      bm_nm<-c(bm_nm,"complete task")
      
      cat("\nFinish building reference models for task:",pred_task,"in",pred_in_d,"with",fs_type,",in",lapse_tsk,units(lapse_tsk),
          ".\n--------------------------\n")
      
      #benchmark
      bm<-data.frame(bm_nm=bm_nm,bm_time=bm,
                     stringsAsFactors = F)
      saveRDS(bm,file=paste0("./data/model_ref/pred_in_",pred_in_d,"d_bm_gbm_",fs_type,"_",pred_task,".rda"))
    }
  }
}


#' For each prediction task, defined as "predict AKI stage X in Y days, with/without Scr", 4 intermedicate data files have been generated and saved in `./data/model_ref/...`, which are:  
#' 
#' * `..._hyperpar_gbm_...rda`: the final hyper-parameter sets after tunning;   
#' 
#' * `..._model_gbm_...rda`: the final gbm model after tunning;   
#' 
#' * `..._valid_gbm_...rda`: the predictted probability on validataion set;      
#'  
#' * `..._varimp_gbm_...rda`: the final list of variable importance     
#' 


#' #### Objective 2.3: Performance Evaluations for Benchmark Model
rm(list=ls()[!(ls() %in% c("pred_in_d_opt","fs_type_opt",
                           "get_perf_summ","get_calibr"))]);
gc() #release some memory

for(pred_in_d in pred_in_d_opt){
  
  for(fs_type in fs_type_opt){
    
    perf_tbl_full<-c()
    perf_tbl<-c()
    calib_tbl<-c()
    varimp_tbl<-c()
    for(i in seq_along(pred_task_lst)){
      valid_out<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_",pred_task_lst[i],".rda"))
      valid<-valid_out$valid
      
      #overall summary
      perf_summ<-get_perf_summ(pred=valid$pred,
                               real=valid$y,
                               keep_all_cutoffs=T)
      perf_tbl_full %<>% 
        bind_rows(perf_summ$perf_at %>% 
                    dplyr::mutate(pred_task=pred_task_lst[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      perf_tbl %<>% 
        bind_rows(perf_summ$perf_summ %>% 
                    dplyr::mutate(pred_task=pred_task_lst[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      #calibration
      calib<-get_calibr(pred=valid$pred,
                       real=valid$y,
                       n_bin=20)
      
      calib_tbl %<>% 
        bind_rows(calib %>% 
                    dplyr::mutate(pred_task=pred_task_lst[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      #variable
      varimp<-valid_out$feat_imp %>%
        dplyr::mutate(rank=1:n(),
                      Gain_rescale=round(Gain/Gain[1]*100)) %>%
        dplyr::select(rank,Feature,Gain_rescale)
      
      varimp_tbl %<>% 
        bind_rows(varimp %>% 
                    mutate(pred_task=pred_task_lst[i],pred_in_d=pred_in_d,fs_type=fs_type,tot_feature=nrow(varimp)))
    }
    
    perf_out<-list(perf_tbl_full=perf_tbl_full,
                   perf_tbl=perf_tbl,
                   calib_tbl=calib_tbl,
                   varimp_tbl=varimp_tbl)
    
    #save results as r data.frame
    saveRDS(perf_out,file=paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_baseline_model_perf.rda"))
  }
}

