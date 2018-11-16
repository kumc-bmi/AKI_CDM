############################## evaluation prediction performance ###################################
rm(list=ls())

source("./R/util.R")
require_libraries(c("Matrix",
                    "dplyr",
                    "tidyr",
                    "plyr",
                    "magrittr", 
                    "stringr",                    
                    "ResourceSelection"
))

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
  valid<-readRDS(paste0("./data/model_ref/valid_gbm_no_fs_",pred_task[i],".rda"))
  
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
  
  perf_tbl_full %<>% bind_rows(perf_at %>% mutate(pred_task=pred_task[i]))
  
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
                dplyr::summarize(prec=mean(prec),
                                 sens=mean(rec_sens),
                                 spec=mean(spec),
                                 ppv=mean(ppv),
                                 npv=mean(npv),
                                 acc=mean(acc),
                                 fscore=mean(fscore),
                                 mcc=mean(mcc)) %>%
                gather(overall_meas,meas_val))
  
  perf_tbl %<>% bind_rows(perf_summ %>% mutate(pred_task=pred_task[i]))
  
  
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
  
  calib_tbl %<>% bind_rows(calib %>% mutate(pred_task=pred_task[i]))
  
  #variable
  varimp<-readRDS(paste0("./data/model_ref/varimp_gbm_no_fs_",pred_task[i],".rda")) %>%
    dplyr::mutate(rank=1:n(),
                  Gain_rescale=round(Gain/Gain[1]*100)) %>%
    dplyr::select(rank,Feature,Gain_rescale)
  
  varimp_tbl %<>% 
    bind_rows(varimp %>% mutate(pred_task=pred_task[i],tot_feature=nrow(varimp)))
}

perf_out<-list(perf_tbl_full=perf_tbl_full,
               perf_tbl=perf_tbl,
               calib_tbl=calib_tbl,
               varimp_tbl=varimp_tbl)
saveRDS(perf_out,file="./data/baseline_model_perf.rda")



# tabulate and plot overall summary
require_libraries("ggplot2")

#only print out a selective of measures
perf_overall<-readRDS("./data/baseline_model_perf.rda")$perf_tbl %>% 
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
calib_tbl<-readRDS("./data/baseline_model_perf.rda")$calib_tbl %>%
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

varimp<-calib_tbl<-readRDS("./data/baseline_model_perf.rda")$varimp_tbl %>%
  mutate(feat=gsub("_change","",Feature)) %>%
  left_join(data_dict,by=c("feat"="VALUESET_ITEM")) %>%
  mutate(feat = ifelse(is.na(VALUESET_ITEM_DESCRIPTOR),Feature,VALUESET_ITEM_DESCRIPTOR)) %>%
  mutate(feat = ifelse(grepl("_change",Feature),paste0(feat,"_change"),feat)) %>%
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
