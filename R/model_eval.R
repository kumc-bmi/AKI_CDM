#############################################
##### evaluation prediction performance #####
#############################################

rm(list=ls())

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection",
                    "ROCR",
                    "PRROC"
))


##=================aggregate prediction results=====================
tbl1<-readRDS("./data/Table1.rda") %>%
  dplyr::select(ENCOUNTERID,SERUM_CREAT_BASE) %>%
  left_join(readRDS("./data/AKI_DEMO.rda") %>%
              filter(key=="AGE") %>%
              dplyr::select(ENCOUNTERID,key,value) %>%
              spread(key,value) %>%
              mutate(AGE=as.numeric(AGE)),
            by="ENCOUNTERID")

#-----prediction point
pred_in_d_opt<-c(2,1)

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")

#-----performance meatrics
perf_metrics<-c("roauc",
                "prauc1",
                "opt_sens",
                "opt_spec",
                "opt_ppv",
                "opt_npv")

#-----boostrapping parameters
boots<-30

for(pred_in_d in pred_in_d_opt){
  perf_full_bs<-c()
  perf_overall_bs<-c()
  perf_calib_bs<-c()
  
  for(pred_task in pred_task_lst){
    
    for(fs_type in fs_type_opt){
    
      valid_orig<-readRDS(paste0("./data/model_kumc/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))$valid %>%
        mutate(ENCOUNTERID=gsub("_.*","",ROW_ID),
               day_at=as.numeric(gsub(".*_","",ROW_ID))) %>%
        left_join(tbl1,by="ENCOUNTERID") %>%
        dplyr::mutate(age=cut(AGE,breaks=c(0,45,65,Inf),include.lowest=T,right=F)) %>%
        mutate(admit_scr=cut(SERUM_CREAT_BASE,breaks=c(0,1,2,3,Inf),include.lowest=T,right=F))
      
      n<-nrow(valid_orig)
      
      for(b in 1:boots){
        start_i<-Sys.time()
        
        #--bootstrapping
        idxset<-sample(1:n,n,replace=T)
        valid<-valid_orig %>% dplyr::slice(idxset)
        
        #--collect performance metrics
        #full ROC/PRC curve
        perf_full<-get_perf_summ(pred=valid$pred,
                                 real=valid$y,
                                 keep_all_cutoffs=T)$perf_at %>%
          dplyr::select(cutoff,rec_sens,spec,prec) %>%
          mutate(cutoff=round(cutoff,6)) %>%
          group_by(cutoff) %>%
          dplyr::summarise(rec_sens=mean(rec_sens),
                           spec=mean(spec),
                           prec=mean(prec)) %>%
          dplyr::mutate(size=nrow(valid),
                        grp="Overall",
                        pred_task=pred_task)
        
        #overall
        perf_overall<-get_perf_summ(pred=valid$pred,
                                    real=valid$y,
                                    keep_all_cutoffs=F)$perf_summ %>%
          dplyr::filter(overall_meas %in% perf_metrics) %>%
          dplyr::mutate(size=nrow(valid),
                        grp="Overall",
                        pred_task=pred_task)
        
        calib<-get_calibr(pred=valid$pred,
                          real=valid$y) %>%
          mutate(grp="ALL",pred_task=pred_task)
        
        
        #subgroup-by calendar year
        subgrp_calyr_full<-c()
        subgrp_calyr<-c()
        subgrp_calyr_calib<-c()
        for(valid_type in c("int","ext")){
          if(valid_type=="int"){
            valid_grp<-valid %>% filter(yr<=2017)
          }else{
            valid_grp<-valid %>% filter(yr>2017)
          }
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            dplyr::filter(overall_meas %in% perf_metrics)
          
          subgrp_calyr_full %<>%
            bind_rows(get_perf_summ(pred=valid_grp$pred,
                                    real=valid_grp$y,
                                    keep_all_cutoffs=T)$perf_at %>%
                        dplyr::select(cutoff,rec_sens,spec,prec) %>%
                        mutate(cutoff=round(cutoff,6)) %>%
                        group_by(cutoff) %>%
                        dplyr::summarise(rec_sens=mean(rec_sens),
                                         spec=mean(spec),
                                         prec=mean(prec)) %>%
                        dplyr::mutate(grp=paste0("Subgrp_VTYPE:",valid_type),
                                      pred_task=pred_task))
          
          subgrp_calyr %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_VTYPE:",valid_type),
                                      pred_task=pred_task))
          subgrp_calyr_calib %<>%
            bind_rows(get_calibr(pred=valid_grp$pred,
                                 real=valid_grp$y) %>%
                        dplyr::mutate(grp=paste0("Subgrp_VTYPE:",valid_type),
                                      pred_task=pred_task))
        }
        
        #subgroup-by day since admission
        subgrp_day_full<-c()
        subgrp_day<-c()
        subgrp_day_calib<-c()
        for(d in pred_in_d:7){
          valid_grp<-valid %>% filter(day_at==d)
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            dplyr::filter(overall_meas %in% perf_metrics)
          
          subgrp_day_full %<>%
            bind_rows(get_perf_summ(pred=valid_grp$pred,
                                    real=valid_grp$y,
                                    keep_all_cutoffs=T)$perf_at %>%
                        dplyr::select(cutoff,rec_sens,spec,prec) %>%
                        mutate(cutoff=round(cutoff,6)) %>%
                        group_by(cutoff) %>%
                        dplyr::summarise(rec_sens=mean(rec_sens),
                                         spec=mean(spec),
                                         prec=mean(prec)) %>%
                        dplyr::mutate(grp=paste0("Subgrp_DAY:",d),
                                      pred_task=pred_task))
          
          subgrp_day %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_DAY:",d),
                                      pred_task=pred_task))
          subgrp_day_calib %<>%
            bind_rows(get_calibr(pred=valid_grp$pred,
                                 real=valid_grp$y) %>%
                        dplyr::mutate(grp=paste0("Subgrp_DAY:",d),
                                      pred_task=pred_task))
        }
        
        #subgroup-age
        subgrp_age_full<-c()
        subgrp_age<-c()
        subgrp_age_calib<-c()
        grp_vec<-unique(valid$age)
        for(grp in seq_along(grp_vec)){
          valid_grp<-valid %>% filter(age==grp_vec[grp])
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            dplyr::filter(overall_meas %in% perf_metrics)
          
          subgrp_age_full %<>%
            bind_rows(get_perf_summ(pred=valid_grp$pred,
                                    real=valid_grp$y,
                                    keep_all_cutoffs=T)$perf_at %>%
                        dplyr::select(cutoff,rec_sens,spec,prec) %>%
                        mutate(cutoff=round(cutoff,6)) %>%
                        group_by(cutoff) %>%
                        dplyr::summarise(rec_sens=mean(rec_sens),
                                         spec=mean(spec),
                                         prec=mean(prec)) %>%
                        dplyr::mutate(grp=paste0("Subgrp_AGE:",grp_vec[grp]),
                                      pred_task=pred_task))
          
          subgrp_age %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_AGE:",grp_vec[grp]),
                                      pred_task=pred_task))
          
          subgrp_age_calib %<>%
            bind_rows(get_calibr(pred=valid_grp$pred,
                                 real=valid_grp$y) %>%
                        dplyr::mutate(grp=paste0("Subgrp_AGE:",grp_vec[grp]),
                                      pred_task=pred_task))
        }
        
        #subgroup-scr
        subgrp_scr_full<-c()
        subgrp_scr<-c()
        subgrp_scr_calib<-c()
        grp_vec<-unique(valid$admit_scr)
        for(grp in seq_along(grp_vec)){
          valid_grp<-valid %>% filter(admit_scr==grp_vec[grp])
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            filter(overall_meas %in% perf_metrics)
          
          subgrp_scr_full %<>%
            bind_rows(get_perf_summ(pred=valid_grp$pred,
                                    real=valid_grp$y,
                                    keep_all_cutoffs=T)$perf_at %>%
                        dplyr::select(cutoff,rec_sens,spec,prec) %>%
                        mutate(cutoff=round(cutoff,6)) %>%
                        group_by(cutoff) %>%
                        dplyr::summarise(rec_sens=mean(rec_sens),
                                         spec=mean(spec),
                                         prec=mean(prec)) %>%
                        dplyr::mutate(grp=paste0("Subgrp_Scr_Base:",grp_vec[grp]),
                                      pred_task=pred_task))
          
          subgrp_scr %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_Scr_Base:",grp_vec[grp]),
                                      pred_task=pred_task))
          
          subgrp_scr_calib %<>%
            bind_rows(get_calibr(pred=valid_grp$pred,
                                 real=valid_grp$y) %>%
                        dplyr::mutate(grp=paste0("Subgrp_Scr_Base:",grp_vec[grp]),
                                      pred_task=pred_task))
        }
        
        perf_full %<>%
          bind_rows(subgrp_calyr_full) %>%
          bind_rows(subgrp_day_full) %>%
          bind_rows(subgrp_age_full) %>%
          bind_rows(subgrp_scr_full) %>%
          dplyr::mutate(pred_in_d=pred_in_d,fs_type=fs_type)
        
        perf_full_bs %<>%
          bind_rows(perf_full %>% dplyr::mutate(boots=b))
        
        perf_overall %<>%
          bind_rows(subgrp_calyr) %>%
          bind_rows(subgrp_day) %>%
          bind_rows(subgrp_age) %>%
          bind_rows(subgrp_scr) %>%
          dplyr::mutate(pred_in_d=pred_in_d,fs_type=fs_type) %>%
          dplyr::mutate(meas_val=round(meas_val,4))
        
        perf_overall_bs %<>%
          bind_rows(perf_overall %>% dplyr::mutate(boots=b))
        
        calib %<>%
          bind_rows(subgrp_calyr_calib) %>%
          bind_rows(subgrp_day_calib) %>%
          bind_rows(subgrp_age_calib) %>%
          bind_rows(subgrp_scr_calib) %>%
          dplyr::mutate(pred_in_d=pred_in_d,fs_type=fs_type)
        
        perf_calib_bs %<>%
          bind_rows(calib %>% dplyr::mutate(boots=b))
        
        lapse_i<-Sys.time()-start_i
        cat(paste(c(pred_in_d,pred_task,fs_type,b),collapse=","),"...finished in",lapse_i,units(lapse_i),".\n")
      }
    }
  }
  perf_full_site<-perf_full_bs %>%
    gather(meas,meas_val,-pred_in_d,-pred_task,-fs_type,-grp,-size,-boots,-cutoff) %>%
    group_by(pred_in_d,pred_task,fs_type,grp,size,cutoff,meas) %>%
    dplyr::summarize(meas_med=median(meas_val,na.rm=T),
                     meas_lb=quantile(meas_val,probs=0.025,na.rm=T),
                     meas_ub=quantile(meas_val,probs=0.975,na.rm=T))
  
  perf_overall_site<-perf_overall_bs %>%
    group_by(pred_in_d,pred_task,fs_type,grp,overall_meas) %>%
    dplyr::summarize(size=round(mean(size)),
                     meas_med=median(meas_val,na.rm=T),
                     meas_lb=quantile(meas_val,probs=0.025,na.rm=T),
                     meas_ub=quantile(meas_val,probs=0.975,na.rm=T))
  
  perf_calib_site<-perf_calib_bs %>%
    gather(overall_meas,meas_val,-pred_in_d,-pred_task,-fs_type,-pred_bin,-boots,-expos,-grp) %>%
    group_by(pred_in_d,pred_task,fs_type,pred_bin,overall_meas,grp,expos) %>%
    dplyr::summarize(meas_med=median(meas_val,na.rm=T),
                     meas_lb=quantile(meas_val,probs=0.025,na.rm=T),
                     meas_ub=quantile(meas_val,probs=0.975,na.rm=T))
  
  saveRDS(perf_full_site,file=paste0("./data/model_ref/kumc_perffull_",pred_in_d,"d.rda"))
  saveRDS(perf_overall_site,file=paste0("./data/model_ref/kumc_perfsumm_",pred_in_d,"d.rda"))
  saveRDS(perf_calib_site,file=paste0("./data/model_ref/kumc_calib_",pred_in_d,"d.rda"))
}



##--- spot check results ---
readRDS("./data/model_ref/kumc_perfsumm_2d.rda") %>%
  View


