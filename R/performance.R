############################## evaluation prediction performance ###################################
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
pred_in_d_opt<-c(1,2)

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
boots<-100

for(pred_in_d in c(1,2)){
  perf_overall_site<-c()
  perf_calib_site<-c()
  
  for(pred_task in pred_task_lst){
    
    for(fs_type in fs_type_opt){
      
      valid_orig<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))$valid %>%
        mutate(ENCOUNTERID=gsub("_.*","",ROW_ID),
               day_at=as.numeric(gsub(".*_","",ROW_ID))) %>%
        left_join(tbl1,by="ENCOUNTERID") %>%
        dplyr::mutate(age=cut(AGE,breaks=c(0,45,65,Inf),include.lowest=T,right=F)) %>%
        mutate(admit_scr=cut(SERUM_CREAT_BASE,breaks=c(0,1,2,3,Inf),include.lowest=T,right=F))
      
      n<-nrow(valid_orig)
      
      for(b in 1:boots){
        #--bootstrapping
        idxset<-sample(1:n,n,replace=T)
        valid<-valid_orig %>% slice(idxset)
        
        #--collect performance metrics
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
          mutate(pred_in_d=pred_in_d,
                 fs_type=fs_type)
        
        #subgroup-age
        subgrp_age<-c()
        grp_vec<-unique(valid$age)
        for(grp in seq_along(grp_vec)){
          valid_grp<-valid %>% filter(age==grp_vec[grp])
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            dplyr::filter(overall_meas %in% perf_metrics)
          
          subgrp_age %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_AGE:",grp_vec[grp]),
                                      pred_task=pred_task))
        }
        
        #subgroup-scr
        subgrp_scr<-c()
        grp_vec<-unique(valid$admit_scr)
        for(grp in seq_along(grp_vec)){
          valid_grp<-valid %>% filter(admit_scr==grp_vec[grp])
          grp_summ<-get_perf_summ(pred=valid_grp$pred,
                                  real=valid_grp$y,
                                  keep_all_cutoffs=F)$perf_summ %>%
            filter(overall_meas %in% perf_metrics)
          
          subgrp_scr %<>% 
            bind_rows(grp_summ %>% 
                        dplyr::mutate(size=nrow(valid_grp),
                                      grp=paste0("Subgrp_Scr_Base:",grp_vec[grp]),
                                      pred_task=pred_task))
        }
        
        perf_overall %<>%
          bind_rows(subgrp_age) %>%
          bind_rows(subgrp_scr) %>%
          dplyr::mutate(pred_in_d=pred_in_d,fs_type=fs_type) %>%
          dplyr::mutate(meas_val=round(meas_val,4))
        
        perf_overall_site %<>%
          bind_rows(perf_overall %>% dplyr::mutate(boots=b))
        
        perf_calib_site %<>%
          bind_rows(calib %>% dplyr::mutate(boots=b))
      }
    }
  }
  
  perf_overall_site %<>%
    group_by(pred_in_d,pred_task,fs_type,grp,overall_meas) %>%
    dplyr::summarize(size=round(mean(size)),
                     meas_med=median(meas_val,na.rm=T),
                     meas_lb=quantile(meas_val,probs=0.025,na.rm=T),
                     meas_ub==quantile(meas_val,probs=0.975,na.rm=T)) %>%
    ungroup
  
  perf_calib_site %<>%
    gather(overall_meas,meas_val,-pred_in_d,-pred_task,-fs_type,-pred_bin,-boots) %>%
    group_by(pred_in_d,pred_task,fs_type,pred_bin) %>%
    dplyr::summarize(meas_med=median(meas_val,na.rm=T),
                     meas_lb=quantile(meas_val,probs=0.025,na.rm=T),
                     meas_ub==quantile(meas_val,probs=0.975,na.rm=T)) %>%
    ungroup
  
  saveRDS(perf_overall_site,file=paste0("./data/model_ref/kumc_perfsumm_",pred_in_d,"d.rda"))
  saveRDS(perf_calib_site,file=paste0("./data/model_ref/kumc_calib_",pred_in_d,"d.rda"))
}


# perf_overall %<>%
#   bind_rows(subgrp_age) %>%
#   bind_rows(subgrp_scr) %>%
#   dplyr::mutate(pred_in_d=pred_in_d,fs_type=fs_type) %>%
#   dplyr::mutate(pred_task=recode(pred_task,
#                                  `stg1up`="a.AKI>=1",
#                                  `stg2up`="b.AKI>=2",
#                                  `stg3`="c.AKI=3",
#                                  `stg3up`="c.AKI=3"),
#                 overall_meas=recode(overall_meas,
#                                     roauc="1.ROAUC",
#                                     prauc1="2.PRAUC",
#                                     opt_sens="3.Optimal Sensitivity",
#                                     opt_spec="4.Optimal Specificity",
#                                     opt_ppv="5.Optimal Positive Predictive Value",
#                                     opt_npv="6.Optimal Negative Predictive Value"),
#                 meas_val=round(meas_val,4)) %>%
#   spread(overall_meas,meas_val) %>%
#   dplyr::mutate(roauc_up=paste0(roauc_up,")")) %>%
#   unite("1.ROAUC_CI",c("roauc_low","roauc_up"),sep=",") %>%
#   unite("1.ROAUC",c("1.ROAUC","1.ROAUC_CI"),sep=" (") 
