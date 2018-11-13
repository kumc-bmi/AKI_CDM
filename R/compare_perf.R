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
             #,"stg2up"
             #,"stg3"
             )
valid<-c()
for(i in seq_along(pred_task)){
  valid %<>%
    bind_rows(readRDS(paste0("./data/model_ref/valid_gbm_no_fs_",pred_task[i],".rda")) %>%
                mutate(pred_task=pred_task[i]))
}


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
                                 pr$auc.integral,
                                 pr$auc.davis.goadrich,
                                 perf_at$prec[which.min(perf_at$prec_rec_dist)],
                                 perf_at$rec_sens[which.min(perf_at$prec_rec_dist)],
                                 perf_at$fscore[which.min(perf_at$prec_rec_dist)],
                                 length(unique(valid$ROW_ID))),
                      stringsAsFactors = F)


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

perf_summ %<>%
  bind_rows(perf_tbl %>% 
              dplyr::summarize(prec=mean(prec),
                               sens=mean(rec_sens),
                               spec=mean(spec),
                               ppv=mean(ppv),
                               npv=mean(npv),
                               acc=mean(acc),
                               fscore=mean(fscore),
                               mcc=mean(mcc)) %>%
              gather(overall_meas,meas_val))

# plot overall summary
perf_overall<-perf_summ %>% 
  filter(overall_meas %in% c("roauc",
                             "prauc1",
                             "mcc",
                             "acc",
                             "sens",
                             "spec",
                             "ppv",
                             "npv")) %>%
  dplyr::mutate(overall_meas=recode(overall_meas,
                                    roauc="1. ROAUC",
                                    prauc1="2. PRAUC",
                                    acc="3. Accuracy",
                                    mcc="4. Matthews Correlation Coefficient",
                                    sens="5. Average Sensitivity",
                                    spec="6. Average Specificity",
                                    ppv="7. Average Positive Predictive Value",
                                    npv="8. Average Negative Predictive Value"),
                temporal=recode(temporal,
                                `non-temporal`="a.Most Recent Value",
                                `stack-temporal`="b.Stack Temporal",
                                `discrt-surv-temporal`="c.Discrete Survival",
                                `dynamic-temporal`="d.Self Boosting"))
p1<-ggplot(perf_overall,
           aes(x=episode,y=meas_val,linetype=temporal,shape=temporal))+
  geom_point(size=2)+geom_line()+
  facet_wrap(~overall_meas,ncol=4,scales="free")+
  labs(x="Year since DM onset",
       y="Prediction Performance",
       linetype="Temporal Handler",
       shape="Temporal Handler")


# plot calibration
calib %<>%
  mutate(temporal=recode(temporal,
                         `non-temporal`="a.Most Recent Value",
                         `stack-temporal`="b.Stack Temporal",
                         `discrt-surv-temporal`="c.Discrete Survival",
                         `dynamic-temporal`="d.Self Boosting")) %>%
  mutate(color=case_when(y_p > binCI_upper ~ round(y_p/binCI_upper,1)-1,
                         y_p < binCI_lower ~ round(y_p/binCI_lower,1)-1,
                         y_p >= binCI_lower | y_p <= binCI_upper ~ 0))

p2<-ggplot(calib,aes(x=episode,y=pred_bin,fill=color))+
  geom_tile()+
  scale_y_continuous(name="Cumulative proportion of observations (low risk -> high risk)",
                     breaks=seq_len(n_bin),
                     labels=seq(0.05,0.95,length.out = n_bin))+
  scale_fill_gradient2(breaks=c(-0.5,0,0.5),
                       labels=c("Underpredicted","Calibrated","Overpredicted"),
                       low = "darkgreen", mid = "white", high = "darkred")+
  # geom_rug(aes(size=expos),stat="identity",sides="rb")+
  scale_size_continuous(guide=F)+
  labs(x="# of Year since DM onset",fill="Calibration Bias")+
  facet_wrap(~temporal)

