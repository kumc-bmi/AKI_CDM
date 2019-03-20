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
pred_in_d_opt<-c(1,2)
fs_type_opt<-c("no_fs","rm_scr_bun")
pred_task<-c("stg1up","stg2up","stg3")

for(pred_in_d in pred_in_d_opt){
  for(fs_type in fs_type_opt){
    perf_tbl_full<-c()
    perf_tbl<-c()
    calib_tbl<-c()
    varimp_tbl<-c()
    for(i in seq_along(pred_task)){
      valid<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_valid_gbm_",fs_type,"_",pred_task[i],".rda"))
      
      #overall summary
      perf_summ<-get_perf_summ(pred=valid$pred,
                               real=valid$y,
                               keep_all_cutoffs=T)
      perf_tbl_full %<>% 
        bind_rows(perf_summ$perf_at %>% 
                    dplyr::mutate(pred_task=pred_task[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      perf_tbl %<>% 
        bind_rows(perf_summ$perf_summ %>% 
                    dplyr::mutate(pred_task=pred_task[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      #calibration
      calib<-get_calib(pred=valid$pred,
                       real=valid$y,
                       n_bins=20)
      
      calib_tbl %<>% 
        bind_rows(calib %>% 
                    dplyr::mutate(pred_task=pred_task[i],pred_in_d=pred_in_d,fs_type=fs_type))
      
      #variable
      varimp<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_varimp_gbm_",fs_type,"_",pred_task[i],".rda")) %>%
        dplyr::mutate(rank=1:n(),
                      Gain_rescale=round(Gain/Gain[1]*100)) %>%
        dplyr::select(rank,Feature,Gain_rescale)
      
      varimp_tbl %<>% 
        bind_rows(varimp %>% 
                    mutate(pred_task=pred_task[i],pred_in_d=pred_in_d,fs_type=fs_type,tot_feature=nrow(varimp)))
    }
    
    perf_out<-list(perf_tbl_full=perf_tbl_full,
                   perf_tbl=perf_tbl,
                   calib_tbl=calib_tbl,
                   varimp_tbl=varimp_tbl)
    
    #save results as r data.frame
    saveRDS(perf_out,file=paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_baseline_model_perf.rda"))
  }
}


##=================tabulate and plot overall summary=================
require_libraries("ggplot2")

#only print out a selective of measures
perf_overall<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_baseline_model_perf.rda"))$perf_tbl %>% 
  filter(overall_meas %in% c("roauc",
                             "roauc_low",
                             "roauc_up",
                             "prauc1",
                             "opt_sens",
                             "opt_spec",
                             "opt_ppv",
                             "opt_npv")) %>%
  dplyr::mutate(overall_meas=recode(overall_meas,
                                    roauc="1.ROAUC",
                                    prauc1="2.PRAUC",
                                    opt_sens="3.Optimal Sensitivity",
                                    opt_spec="4.Optimal Specificity",
                                    opt_ppv="5.Optimal Positive Predictive Value",
                                    opt_npv="6.Optimal Negative Predictive Value"),
                pred_task=recode(pred_task,
                                 `stg1up`="a.At least AKI1",
                                 `stg2up`="b.At least AKI2",
                                 `stg3`="c.AKI3"),
                meas_val=round(meas_val,4)) %>%
  spread(overall_meas,meas_val) %>%
  unite("1.ROAUC_CI",c("roauc_low","roauc_up"),sep=",") %>%
  unite("1.ROAUC",c("1.ROAUC","1.ROAUC_CI"),sep=" (")


#plot out sens, spec, ppv, npv on a scale of cutoff probabilities
brks<-unique(c(0,seq(0.001,0.01,by=0.001),seq(0.02,0.1,by=0.01),seq(0.2,1,by=0.1)))
perf_cutoff<-readRDS("./data/baseline_model_perf.rda")$perf_tbl_full %>%
  dplyr::select(cutoff,size,rec_sens,spec,ppv,npv,pred_task) %>%
  mutate(bin=cut(cutoff,breaks=brks,include.lowest=T,label=F)) %>%
  group_by(bin,pred_task) %>%
  dplyr::summarise(size=sum(size),
                   cutoff=round(max(cutoff),3),
                   sens_l=quantile(rec_sens,0.025,na.rm=T),
                   sens=median(rec_sens,na.rm=T),
                   sens_u=quantile(rec_sens,0.975,na.rm=T),
                   spec_l=quantile(spec,0.025,na.rm=T),
                   spec=median(spec,na.rm=T),
                   spec_u=quantile(spec,0.975,na.rm=T),
                   ppv_l=quantile(ppv,0.025,na.rm=T),
                   ppv=median(ppv,na.rm=T),
                   ppv_u=quantile(ppv,0.975,na.rm=T),
                   npv_l=quantile(npv,0.025,na.rm=T),
                   npv=median(npv,na.rm=T),
                   npv_u=quantile(npv,0.975,na.rm=T)) %>%
  ungroup %>%
  group_by(pred_task) %>%
  dplyr::mutate(size_overall=sum(size),
                size_cum=cumsum(size)) %>%
  ungroup %>%
  mutate(size_abv=size_overall-size_cum) %>%
  dplyr::select(-bin,-size_overall,-size_cum,-size) %>%
  gather(metric_type,metric_val,-cutoff,-size_abv,-pred_task) %>%
  mutate(metric_type2=ifelse(grepl("_l",metric_type),"low",
                             ifelse(grepl("_u",metric_type),"up","mid")),
         metric_type=gsub("_.*","",metric_type)) %>%
  spread(metric_type2,metric_val) %>%
  mutate(pred_task=recode(pred_task,
                          `stg1up`="a.At least AKI1",
                          `stg2up`="b.At least AKI2",
                          `stg3`="c.AKI3"))

ggplot(perf_cutoff %>% dplyr::filter(cutoff <=0.15),
       aes(x=cutoff,y=mid,color=metric_type,fill=metric_type))+
  geom_line()+ geom_ribbon(aes(ymin=low,ymax=up),alpha=0.3)+
  labs(x="cutoff probability",y="performance metrics")+
  facet_wrap(~pred_task,scales="free",ncol=3)


# plot calibration
calib_tbl<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_baseline_model_perf.rda"))$calib_tbl %>%
  mutate(pred_task=recode(pred_task,
                          `stg1up`="a.At least AKI1",
                          `stg2up`="b.At least AKI2",
                          `stg3`="c.AKI3")) %>%
  mutate(color=case_when(y_p > binCI_upper ~ round(y_p/binCI_upper,1)-1,
                         y_p < binCI_lower ~ round(y_p/binCI_lower,1)-1,
                         y_p >= binCI_lower | y_p <= binCI_upper ~ 0))

ggplot(calib_tbl,aes(x=y_p,y=pred_p))+
  geom_point()+geom_abline(intercept=0,slope=1)+
  geom_errorbar(aes(ymin=binCI_lower,ymax=binCI_upper))+
  labs(x="Actual Probability",y="Predicted Probability",
       title="Calibrations")+
  facet_wrap(~pred_task,scales="free")


# plot variable importance
data_dict<-read.csv("./data/feature_dict.csv") %>%
  dplyr::select(FIELD_NAME,VALUESET_ITEM,VALUESET_ITEM_DESCRIPTOR) %>%
  mutate(VALUESET_ITEM_DESCRIPTOR=as.character(VALUESET_ITEM_DESCRIPTOR)) %>%
  mutate(VALUESET_ITEM=gsub("'","",VALUESET_ITEM)) %>%
  mutate(VALUESET_ITEM=ifelse(FIELD_NAME=="RXNORM_CUI",
                              paste0(VALUESET_ITEM,":01"),
                              ifelse(FIELD_NAME=="PX"&!grepl("^(ICD)+",VALUESET_ITEM),
                                     paste0("CH:",VALUESET_ITEM),
                                     VALUESET_ITEM)))

varimp<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_",fs_type,"_baseline_model_perf.rda"))$varimp_tbl %>%
  dplyr::mutate(suffix=gsub("_","",str_extract(Feature,"((\\_min)|(\\_slope)|(\\_change)|(\\_cum))+"))) %>%
  dplyr::mutate(feat=gsub("((\\_min)|(\\_slope)|(\\_change)|(\\_cum))+","",Feature)) %>%
  left_join(data_dict,by=c("feat"="VALUESET_ITEM")) %>%
  dplyr::mutate(feat = ifelse(is.na(VALUESET_ITEM_DESCRIPTOR),Feature,VALUESET_ITEM_DESCRIPTOR)) %>%
  dplyr::mutate(feat = ifelse(!is.na(suffix),paste0(feat,"_",suffix),feat)) %>%
  dplyr::mutate(feat_rank=paste0(ifelse(rank<10,
                                        paste0("0",rank),
                                        as.character(rank)),
                                 ".",feat)) %>%
  unique %>%
  mutate(feat_rank=paste0(substr(feat_rank,1,50),"...")) %>%
  dplyr::mutate(feat_rank=as.factor(feat_rank)) %>%
  dplyr::mutate(feat_rank=factor(feat_rank,levels=rev(levels(feat_rank)))) %>%
  mutate(pred_task=recode(pred_task,
                          `stg1up`="a.At least AKI1",
                          `stg2up`="b.At least AKI2",
                          `stg3`="c.AKI3"))

ggplot(varimp %>% filter(rank <= 20),
       aes(x=feat_rank,y=Gain_rescale))+
  geom_bar(stat="identity")+
  labs(x="Features",y="Normalized Scale")+
  coord_flip()+scale_y_continuous(trans = "reverse")+
  facet_wrap(~pred_task,scales = "free",ncol=1)


##=================subgroup analysis==============================
tbl1<-readRDS("./data/Table1.rda") %>%
  dplyr::select(ENCOUNTERID,SERUM_CREAT_BASE) %>%
  left_join(readRDS("./data/AKI_DEMO.rda") %>%
              filter(key=="AGE") %>%
              dplyr::select(ENCOUNTERID,key,value) %>%
              spread(key,value) %>%
              mutate(AGE=as.numeric(AGE)),
            by="ENCOUNTERID")
  
pred_task<-c(
  "stg1up"
  ,"stg2up"
  ,"stg3"
)

subgrp_out<-c()
for(pred_in_d in c(1,2)){
  for(i in seq_along(pred_task)){
    valid<-readRDS(paste0("./data/model_ref/pred_in_",pred_in_d,"d_valid_gbm_",fs_type,"_",pred_task[i],".rda")) %>%
      mutate(ENCOUNTERID=gsub("_.*","",ROW_ID),
             day_at=as.numeric(gsub(".*_","",ROW_ID))) %>%
      left_join(tbl1,by="ENCOUNTERID")
    
    subgrp_out %<>%
      bind_rows(valid %>%
                  dplyr::select(ENCOUNTERID,day_at,y,pred,SERUM_CREAT_BASE) %>%
                  mutate(admit_scr=cut(SERUM_CREAT_BASE,breaks=c(0,1,2,3,Inf),include.lowest=T,right=F)) %>%
                  group_by(admit_scr) %>%
                  dplyr::summarise(roauc_low=pROC::ci.auc(y,pred)[[1]],
                                   roauc=pROC::ci.auc(y,pred)[[2]],
                                   roauc_up=pROC::ci.auc(y,pred)[[3]]) %>%
                  ungroup %>%
                  gather(subgrp_type,subgrp,-roauc_low,-roauc,-roauc_up) %>%
                  bind_rows(valid %>%
                              dplyr::select(ENCOUNTERID,day_at,y,pred,AGE) %>%
                              mutate(age=cut(AGE,breaks=c(0,45,65,Inf),include.lowest=T,right=F)) %>%
                              group_by(age) %>%
                              dplyr::summarise(roauc_low=pROC::ci.auc(y,pred)[[1]],
                                               roauc=pROC::ci.auc(y,pred)[[2]],
                                               roauc_up=pROC::ci.auc(y,pred)[[3]]) %>%
                              ungroup %>%
                              gather(subgrp_type,subgrp,-roauc_low,-roauc,-roauc_up)) %>%
                  mutate(pred_task=pred_task[i],
                         pred_at=pred_in_d))
  }
}
subgrp_out2<-subgrp_out %>%
  mutate(label=paste0(round(roauc,2),"(",round(roauc_low,2),"-",round(roauc_up,2),")")) %>%
  dplyr::select(-roauc_low,-roauc,-roauc_up) %>%
  unite("pred_task",c("pred_task","pred_at"),sep="_") %>%
  unique %>% spread(pred_task,label)
 
  
  