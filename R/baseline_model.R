#### survival-like prediction model ####
rm(list=ls()); gc()

source("./R/util.R")
source("./R/var_etl_surv.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "xgboost",
                    # "CMake",
                    # "LightGBM",
                    # "catboost",
                    "ROCR"))


#################### collect and format variables on daily basis ######################
tbl1<-readRDS("./data/Table1.rda") %>%
  mutate(yr=as.numeric(format(ADMIT_DATE,"%Y")))

onset_dt<-c(tbl1$AKI1_SINCE_ADMIT,tbl1$AKI2_SINCE_ADMIT,tbl1$AKI3_SINCE_ADMIT)
quantile(onset_dt,probs=0:20/20,na.rm=T)
pred_end<-quantile(onset_dt,probs=0.5,na.rm=T)
tw<-seq(0,pred_end) #

#--by chunks: encounter year
enc_yr<-tbl1 %>%
  dplyr::select(yr) %>%
  unique %>% arrange(yr) %>%
  filter(yr>2009) %>%
  unlist

#--by variable type
var_type<-c("demo","vital","lab","dx","px","med")

#--save results as array
var_by_yr<-list()
var_bm<-list()
rsample_idx<-c()
for(i in seq_along(enc_yr)){
  start_i<-Sys.time()
  cat("start variable collection for year",enc_yr[i],".\n")
  
  #--collect end_points
  dat_i<-tbl1 %>% filter(yr==enc_yr[i]) %>%
    dplyr::select(ENCOUNTERID,yr,NONAKI_SINCE_ADMIT,
                  AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT) %>%
    gather(y,dsa_y,-ENCOUNTERID,-yr) %>%
    filter(!is.na(dsa_y)) %>%
    mutate(y=recode(y,
                    "NONAKI_SINCE_ADMIT"=0,
                    "AKI1_SINCE_ADMIT"=1,
                    "AKI2_SINCE_ADMIT"=2,
                    "AKI3_SINCE_ADMIT"=3)) %>%
    mutate(y=as.numeric(y))
  
  #--random sampling
  rsample_idx %<>%
    bind_rows(dat_i %>% 
                dplyr::select(ENCOUNTERID,yr) %>%
                unique %>%
                mutate(cv10_idx=sample(1:10,n(),replace=T)))
  
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
          mutate(value=as.numeric(value),
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
                                tw=tw)
    
    #load
    X_surv %<>% bind_rows(Xy_surv$X_surv)
    y_surv %<>% bind_rows(Xy_surv$y_surv)
    
    lapse_v<-Sys.time()-start_v
    var_etl_bm<-c(var_etl_bm,paste0(lapse_v,units(lapse_v)))
    cat("\n...finished ETL",var_type[v],"for year",enc_yr[i],"in",lapse_v,units(lapse_v),".\n")
  }
  var_by_yr[[i]]<-list(X_surv=X_surv,
                       y_surv=y_surv)
  
  lapse_i<-Sys.time()-start_i
  var_etl_bm<-paste0(lapse_i,units(lapse_i))
  cat("\nfinished variabl collection for year",enc_yr[i],"in",lapse_i,units(lapse_i),".\n")
  
  var_bm[[i]]<-data.frame(bm_nm=c(var_type,"overall"),
                          bm_time=var_etl_bm,
                          stringsAsFactors = F)
}
#--save preprocessed data
saveRDS(rsample_idx,file="./data/rsample_idx.rda")
saveRDS(var_by_yr,file="./data/var_by_yr.rda")
saveRDS(var_bm,file="./data/var_bm.rda")


############################ baseline GBM model ######################################
#--prepare training set
X_tr<-c()
X_ts<-c()
y_tr<-c()
y_ts<-c()
for(i in seq_along(seq(2010,2016))){
  var_by_yr<-readRDS("./data/var_by_yr.rda")[[i]]
  
  X_tr %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
    semi_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
              by="ENCOUNTERID")
  X_ts %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
    semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
              by="ENCOUNTERID")
  
  y_tr %<>% bind_rows(var_by_yr[["y_surv"]]) %>%
    semi_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
              by="ENCOUNTERID")
  y_ts %<>% bind_rows(var_by_yr[["y_surv"]]) %>%
    semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
              by="ENCOUNTERID")
}

X_tr %<>%
  arrange(ENCOUNTERID,dsa_y) %>%
  unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
  long_to_sparse_matrix(df=.,
                        id="ROW_ID",
                        variable="key",
                        val="value")
y_tr %<>%
  arrange(ENCOUNTERID,dsa_y) %>%
  unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
  arrange(ROW_ID) %>%
  unique %>%
  mutate(y=(y>0)*1) # any AKI


X_ts %<>%
  arrange(ENCOUNTERID,dsa_y) %>%
  unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
  long_to_sparse_matrix(df=.,
                        id="ROW_ID",
                        variable="key",
                        val="value")
y_ts %<>%
  arrange(ENCOUNTERID,dsa_y) %>%
  unite("ROW_ID",c("ENCOUNTERID","dsa_y")) %>%
  arrange(ROW_ID) %>%
  unique %>%
  mutate(y=(y>0)*1) # any AKI


#check alignment
all(row.names(X_tr)==y_tr$ROW_ID)
all(row.names(X_ts)==y_ts$ROW_ID)


#covert to xgb data frame
dtrain<-xgb.DMatrix(data=X_tr,label=y_tr$y)
dtest<-xgb.DMatrix(data=X_ts,label=y_ts$y)


#--tune hyperparameter
#hyper-parameter grid for xgboost
eval_metric<-"auc"
objective<-"binary:logistic"
grid_params<-expand.grid(
  max_depth=c(4,6,8,10),
  eta=c(0.05,0.02,0.01,0.005),
  eta=0.02,
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
                nfold = 5,
                early_stopping_rounds = 100,
                print_every_n = 100,
                prediction = T) #keep cv results
  
  bst_grid<-rbind(bst_grid, cbind(grid_params[i,],
                                  metric=max(bst$evaluation_log[[metric_name]]),
                                  steps=which(bst$evaluation_log[[metric_name]]==max(bst$evaluation_log[[metric_name]]))[1]))
  
  bst_grid_cv<-cbind(bst_grid_cv,bst$pred)
  
  if(verb){
    cat('finished train case:',paste0(paste0(c(colnames(grid_params),"scale_pos_weight"),"="),param,collapse="; "),
        'in',Sys.time()-start_i,units(Sys.time()-start_i),"\n")
    start_i<-Sys.time()
  }
}
hyper_param<-bst_grid[which.max(bst_grid$metric),]


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


#--evaluation prediction performance
# various performace table
pred<-ROCR::prediction(valid$pred,
                       valid$y)

prc<-performance(pred,"prec","rec")
roc<-performance(pred,"sens","spec")
nppv<-performance(pred,"ppv","npv")
pcfall<-performance(pred,"pcfall")
acc<-performance(pred,"acc")
fscore<-performance(pred,"f")
mcc<-performance(pred,"phi")

perf_tbl<-data.frame(cutoff=prc@alpha.values[[1]],
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
  dplyr::mutate(episode = j,
                temporal = i) %>%
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
                                 length(unique(valid$PATIENT_NUM))),
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
                binCI_upper = pred_p+1.96*sqrt(y_p*(1-y_p)/expos)) %>%
  dplyr::mutate(episode = j,
                temporal = i)

calib_equal_bin %<>% bind_rows(calib2)


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
calib_equal_bin %<>%
  mutate(temporal=recode(temporal,
                         `non-temporal`="a.Most Recent Value",
                         `stack-temporal`="b.Stack Temporal",
                         `discrt-surv-temporal`="c.Discrete Survival",
                         `dynamic-temporal`="d.Self Boosting")) %>%
  mutate(color=case_when(y_p > binCI_upper ~ round(y_p/binCI_upper,1)-1,
                         y_p < binCI_lower ~ round(y_p/binCI_lower,1)-1,
                         y_p >= binCI_lower | y_p <= binCI_upper ~ 0))

p2<-ggplot(calib_equal_bin,aes(x=episode,y=pred_bin,fill=color))+
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


#--feature importance
feat_imp<-xgb.importance(colnames(X_tr),model=xgb_tune)


