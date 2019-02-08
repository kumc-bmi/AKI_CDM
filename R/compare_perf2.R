############################## evaluation prediction performance ###################################
rm(list=ls())

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",
                    "xgboost",
                    "ResourceSelection",
                    "ROCR",
                    "PRROC"
))


#-----prediction point
pred_in_d<-1
# pred_in_d<-2
# pred_in_d<-3

#-----feature selection type
fs_type<-"no_fs"
# fs_type<-"rm_scr_bun"

#----keep past predictions
past<-TRUE


##=================aggregate baseline prediction results=====================
# load data
pred_task<-c(
  "stg1up"
  ,"stg2up"
  ,"stg3"
)
n_bin<-20
perf_tbl_full<-c()
perf_tbl<-c()
calib_tbl<-c()
varimp_tbl<-c()
for(i in seq_along(pred_task)){
  valid_i<-readRDS(paste0("./data_local/model_ref/pred_in_",
                          pred_in_d,"d_valid_gbm_",
                          fs_type,"_",pred_task[i],".rda")) %>%
    dplyr::mutate(episode=as.numeric(gsub(".*_","",ROW_ID)))
  
  for(j in 0:4){
    valid<-valid_i %>% dplyr::filter(episode==j)
    
    # various performace table
    pred<-ROCR::prediction(valid$pred,valid$y)
    
    prc<-performance(pred,"prec","rec")
    roc<-performance(pred,"sens","spec")
    nppv<-performance(pred,"ppv","npv")
    pcfall<-performance(pred,"pcfall")
    acc<-performance(pred,"acc")
    fscore<-performance(pred,"f")
    mcc<-performance(pred,"phi")
    
    perf_at<-data.frame(cutoff=prc@alpha.values[[1]],
                        prec=prc@y.values[[1]],
                        rec_sens=prc@x.values[[1]],
                        stringsAsFactors = F) %>% 
      arrange(cutoff) %>%
      left_join(data.frame(cutoff=nppv@alpha.values[[1]],
                           ppv=nppv@y.values[[1]],
                           npv=nppv@x.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      dplyr::mutate(prec_rec_dist=abs(prec-rec_sens)) %>%
      left_join(data.frame(cutoff=fscore@x.values[[1]],
                           fscore=fscore@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=roc@alpha.values[[1]],
                           spec=roc@x.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      dplyr::mutate(Euclid_meas=sqrt((1-rec_sens)^2+(0-(1-spec))^2),
                    Youden_meas=rec_sens+spec-1) %>%
      left_join(data.frame(cutoff=pcfall@x.values[[1]],
                           pcfall=pcfall@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=acc@x.values[[1]],
                           acc=acc@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=mcc@x.values[[1]],
                           mcc=mcc@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      filter(prec > 0 & rec_sens > 0 & spec > 0)
    
    #----performance table (all cutoff points)---
    perf_tbl_full %<>% 
      bind_rows(perf_at %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
    
    # performance summary
    lab1<-valid$pred[valid$y==1]
    lab0<-valid$pred[valid$y==0]
    pr<-pr.curve(scores.class0 = lab1,
                 scores.class1 = lab0,curve=F)
    roc_ci<-pROC::ci.auc(valid$y,valid$pred)
    
    perf_summ<-data.frame(overall_meas=c("roauc_low",
                                         "roauc",
                                         "roauc_up",
                                         "opt_thresh",
                                         "opt_sens",
                                         "opt_spec",
                                         "opt_ppv",
                                         "opt_npv",
                                         "prauc1",
                                         "prauc2",
                                         "opt_prec",
                                         "opt_rec",
                                         "opt_fscore",
                                         "size"),
                          meas_val=c(roc_ci[[1]],
                                     roc_ci[[2]],
                                     roc_ci[[3]],
                                     perf_at$cutoff[which.min(perf_at$Euclid_meas)],
                                     perf_at$rec_sens[which.min(perf_at$Euclid_meas)],
                                     perf_at$spec[which.min(perf_at$Euclid_meas)],
                                     perf_at$ppv[which.min(perf_at$Euclid_meas)],
                                     perf_at$npv[which.min(perf_at$Euclid_meas)],
                                     pr$auc.integral,
                                     pr$auc.davis.goadrich,
                                     perf_at$prec[which.min(perf_at$prec_rec_dist)],
                                     perf_at$rec_sens[which.min(perf_at$prec_rec_dist)],
                                     perf_at$fscore[which.min(perf_at$prec_rec_dist)],
                                     length(unique(valid$ROW_ID))),
                          stringsAsFactors = F) %>%
      bind_rows(perf_at %>% 
                  dplyr::summarize(prec=mean(prec,na.rm=T),
                                   sens=mean(rec_sens,na.rm=T),
                                   spec=mean(spec,na.rm=T),
                                   ppv=mean(ppv,na.rm=T),
                                   npv=mean(npv,na.rm=T),
                                   acc=mean(acc,na.rm=T),
                                   fscore=mean(fscore,na.rm=T),
                                   mcc=mean(mcc,na.rm=T)) %>%
                  gather(overall_meas,meas_val))
    
    #------aggregated performance table----------
    perf_tbl %<>% 
      bind_rows(perf_summ %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
    
    #calibration
    calib<-data.frame(pred=valid$pred,
                      y=valid$y) %>%
      arrange(pred) %>%
      dplyr::mutate(pred_bin = cut(pred,
                                   breaks=unique(quantile(pred,0:(n_bin)/(n_bin))),
                                   include.lowest=T,
                                   labels=F)) %>%
      ungroup %>% group_by(pred_bin) %>%
      dplyr::summarize(expos=n(),
                       bin_lower=min(pred),
                       bin_upper=max(pred),
                       bin_mid=median(pred),
                       y_agg = sum(y),
                       pred_p = mean(pred)) %>%
      dplyr::mutate(y_p=y_agg/expos) %>%
      dplyr::mutate(binCI_lower = pmax(0,pred_p-1.96*sqrt(y_p*(1-y_p)/expos)),
                    binCI_upper = pred_p+1.96*sqrt(y_p*(1-y_p)/expos))
    
    #--------------calibration table------------
    calib_tbl %<>% 
      bind_rows(calib %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
  }
  
  #variable
  varimp<-readRDS(paste0("./data_local/model_ref/pred_in_",
                         pred_in_d,"d_varimp_gbm_",fs_type,"_",
                         pred_task[i],".rda")) %>%
    dplyr::mutate(rank=1:n(),
                  Gain_rescale=round(Gain/Gain[1]*100)) %>%
    dplyr::select(rank,Feature,Gain_rescale)
  
  varimp_tbl %<>% 
    bind_rows(varimp %>% mutate(pred_task=pred_task[i],tot_feature=nrow(varimp)))
  
  #----------variable importance table------------
  varimp_tbl %<>% 
    bind_rows(varimp %>% 
                mutate(episode=j,
                       pred_task=pred_task[i],
                       tot_feature=nrow(varimp)))
  #-----------------------------------------------
}

perf_out<-list(perf_tbl_full=perf_tbl_full,
               perf_tbl=perf_tbl,
               calib_tbl=calib_tbl,
               varimp_tbl=varimp_tbl)

saveRDS(perf_out,file=paste0("./data_local/LMGBM_ref/",
                             pred_in_d,"_",fs_type,"_",past,"_perf_base.rda"))


##=================aggregate prediction results=====================
# load data
pred_task<-c(
  "stg1up"
  ,"stg2up"
  ,"stg3"
)
n_bin<-20
perf_tbl_full<-c()
perf_tbl<-c()
calib_tbl<-c()
varimp_tbl<-c()
for(i in seq_along(pred_task)){
  for(j in 0:4){
    valid<-readRDS(paste0("./data_local/LMGBM_ref/",
                          pred_in_d,"_",
                          fs_type,"_",
                          pred_task[i],"_",
                          past,"_LMgbm.rda"))$model_roc[[paste0(j,"d_since_adm_",past)]] %>%
      filter(val_type=="holdout")
    
    # various performace table
    pred<-ROCR::prediction(valid$pred,valid$real)
    
    prc<-performance(pred,"prec","rec")
    roc<-performance(pred,"sens","spec")
    nppv<-performance(pred,"ppv","npv")
    pcfall<-performance(pred,"pcfall")
    acc<-performance(pred,"acc")
    fscore<-performance(pred,"f")
    mcc<-performance(pred,"phi")
    
    perf_at<-data.frame(cutoff=prc@alpha.values[[1]],
                        prec=prc@y.values[[1]],
                        rec_sens=prc@x.values[[1]],
                        stringsAsFactors = F) %>% 
      arrange(cutoff) %>%
      left_join(data.frame(cutoff=nppv@alpha.values[[1]],
                           ppv=nppv@y.values[[1]],
                           npv=nppv@x.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      dplyr::mutate(prec_rec_dist=abs(prec-rec_sens)) %>%
      left_join(data.frame(cutoff=fscore@x.values[[1]],
                           fscore=fscore@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=roc@alpha.values[[1]],
                           spec=roc@x.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      dplyr::mutate(Euclid_meas=sqrt((1-rec_sens)^2+(0-(1-spec))^2),
                    Youden_meas=rec_sens+spec-1) %>%
      left_join(data.frame(cutoff=pcfall@x.values[[1]],
                           pcfall=pcfall@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=acc@x.values[[1]],
                           acc=acc@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      left_join(data.frame(cutoff=mcc@x.values[[1]],
                           mcc=mcc@y.values[[1]],
                           stringsAsFactors = F),
                by="cutoff") %>%
      filter(prec > 0 & rec_sens > 0 & spec > 0)
    
    #----performance table (all cutoff points)---
    perf_tbl_full %<>% 
      bind_rows(perf_at %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
    
    # performance summary
    lab1<-valid$pred[valid$real==1]
    lab0<-valid$pred[valid$real==0]
    pr<-pr.curve(scores.class0 = lab1,
                 scores.class1 = lab0,curve=F)
    roc_ci<-pROC::ci.auc(valid$real,valid$pred)
    
    perf_summ<-data.frame(overall_meas=c("roauc_low",
                                         "roauc",
                                         "roauc_up",
                                         "opt_thresh",
                                         "opt_sens",
                                         "opt_spec",
                                         "opt_ppv",
                                         "opt_npv",
                                         "prauc1",
                                         "prauc2",
                                         "opt_prec",
                                         "opt_rec",
                                         "opt_fscore",
                                         "size"),
                          meas_val=c(roc_ci[[1]],
                                     roc_ci[[2]],
                                     roc_ci[[3]],
                                     perf_at$cutoff[which.min(perf_at$Euclid_meas)],
                                     perf_at$rec_sens[which.min(perf_at$Euclid_meas)],
                                     perf_at$spec[which.min(perf_at$Euclid_meas)],
                                     perf_at$ppv[which.min(perf_at$Euclid_meas)],
                                     perf_at$npv[which.min(perf_at$Euclid_meas)],
                                     pr$auc.integral,
                                     pr$auc.davis.goadrich,
                                     perf_at$prec[which.min(perf_at$prec_rec_dist)],
                                     perf_at$rec_sens[which.min(perf_at$prec_rec_dist)],
                                     perf_at$fscore[which.min(perf_at$prec_rec_dist)],
                                     length(unique(valid$ROW_ID))),
                          stringsAsFactors = F) %>%
      bind_rows(perf_at %>% 
                  dplyr::summarize(prec=mean(prec,na.rm=T),
                                   sens=mean(rec_sens,na.rm=T),
                                   spec=mean(spec,na.rm=T),
                                   ppv=mean(ppv,na.rm=T),
                                   npv=mean(npv,na.rm=T),
                                   acc=mean(acc,na.rm=T),
                                   fscore=mean(fscore,na.rm=T),
                                   mcc=mean(mcc,na.rm=T)) %>%
                  gather(overall_meas,meas_val))
    
    #------aggregated performance table----------
    perf_tbl %<>% 
      bind_rows(perf_summ %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
    
    #calibration
    calib<-data.frame(pred=valid$pred,
                      y=valid$real) %>%
      arrange(pred) %>%
      dplyr::mutate(pred_bin = cut(pred,
                                   breaks=unique(quantile(pred,0:(n_bin)/(n_bin))),
                                   include.lowest=T,
                                   labels=F)) %>%
      ungroup %>% group_by(pred_bin) %>%
      dplyr::summarize(expos=n(),
                       bin_lower=min(pred),
                       bin_upper=max(pred),
                       bin_mid=median(pred),
                       y_agg = sum(y),
                       pred_p = mean(pred)) %>%
      dplyr::mutate(y_p=y_agg/expos) %>%
      dplyr::mutate(binCI_lower = pmax(0,pred_p-1.96*sqrt(y_p*(1-y_p)/expos)),
                    binCI_upper = pred_p+1.96*sqrt(y_p*(1-y_p)/expos))
    
    #--------------calibration table------------
    calib_tbl %<>% 
      bind_rows(calib %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i]))
    #--------------------------------------------
    
    #variable
    model<-readRDS(paste0("./data_local/LMGBM_ref/",
                          pred_in_d,"_",
                          fs_type,"_",
                          pred_task[i],"_",
                          past,"_LMgbm.rda"))$model_ep[[paste0(j,"d_since_adm_",past)]]
    
    varimp<-xgb.importance(model=model) %>%
      dplyr::mutate(rank=1:n(),
                    Gain_rescale=round(Gain/Gain[1]*100)) %>%
      dplyr::select(rank,Feature,Gain_rescale)
    
    #----------variable importance table------------
    varimp_tbl %<>% 
      bind_rows(varimp %>% 
                  mutate(episode=j,
                         pred_task=pred_task[i],
                         tot_feature=nrow(varimp)))
    #-----------------------------------------------
  }
}

perf_out<-list(perf_tbl_full=perf_tbl_full,
               perf_tbl=perf_tbl,
               calib_tbl=calib_tbl,
               varimp_tbl=varimp_tbl)

saveRDS(perf_out,file=paste0("./data_local/LMGBM_ref/",
                             pred_in_d,"_",fs_type,"_",past,"_perf.rda"))

  