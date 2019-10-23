#source utility functions
source("./R/util.R")

require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom",
                    "Matrix",
                    "xgboost",
                    "ROCR",
                    "PRROC",
                    "ResourceSelection"))
##collect sample data
pred_in_d<-1
pred_task<-"stg2up"
n_chunk<-4

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
                        inner_join(rsample_idx %>% filter(cv10_idx<=6 & yr<2017),
                                  by="ENCOUNTERID"))
  
  X_ts %<>% bind_rows(var_by_yr[["X_surv"]]) %>%
    semi_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
              by="ENCOUNTERID")
  
  y_ts %<>% bind_rows(var_by_yr[["y_surv"]] %>%
                        inner_join(rsample_idx %>% filter(cv10_idx>6 | yr>=2017),
                                  by="ENCOUNTERID"))
}

##-------collect sample sizes (training/calibration/validation)------------
N<-length(unique(rsample_idx$ENCOUNTERID))
rsample_idx %>%
  mutate(sample_type=case_when(yr>=2017 ~ 'T',
                               yr<2017 & cv10_idx>6 ~ 'V',
                               TRUE ~ 'D')) %>%
  group_by(sample_type) %>%
  dplyr::summarize(enc_n=length(unique(ENCOUNTERID))) %>% 
  ungroup %>%
  dplyr::mutate(enc_n2=round(enc_n/N*153821))

# 1 D           89392  75679
# 2 T           32605  27603
# 3 V           59697  50539

##-------collect total number of encounter days-------
y_tr %>% select(ENCOUNTERID,dsa_y) %>% unique %>%
  bind_rows(y_ts %>% select(ENCOUNTERID,dsa_y) %>% unique) %>%
  unique %>% nrow
# [1] 1064619

##-------collect total number of eligible features----------
length(unique(c(X_tr$key,X_ts$key)))
# [1] 38920
length(c(X_tr$key,X_ts$key))
# [1] 142167783

##-------collect clinical facts updating rate-------
X_tr %>% group_by(ENCOUNTERID,dsa) %>%
  dplyr::summarize(fact_n=n()) %>% ungroup %>%
  bind_rows(X_ts %>% group_by(ENCOUNTERID,dsa) %>%
              dplyr::summarize(fact_n=n()) %>% ungroup) %>%
  group_by(ENCOUNTERID) %>%
  dplyr::summarize(m_fact_n=mean(fact_n,na.rm=T),
                   sd_fact_n=sd(fact_n,na.rm=T)) %>%
  ungroup %>%
  dplyr::summarize(m_m_fact_n=mean(m_fact_n,na.rm=T),
                   m_sd_fact_n=mean(sd_fact_n,na.rm=T),
                   sd_m_fact_n=sd(m_fact_n,na.rm=T)) %>%
  ungroup

# m_fact_n sd_fact_n med_fact_n   IQR    q1    q3
#   58.6      77.9         24    78     8    86


X_tr %>% group_by(ENCOUNTERID,dsa) %>%
  dplyr::summarize(fact_n=n()) %>% ungroup %>%
  bind_rows(X_ts %>% group_by(ENCOUNTERID,dsa) %>%
              dplyr::summarize(fact_n=n()) %>% ungroup) %>%
  dplyr::summarize(m_fact_n=mean(fact_n,na.rm=T),
                   sd_fact_n=sd(fact_n,na.rm=T),
                   med_fact_n=median(fact_n,na.rm=T),
                   IQR=IQR(fact_n,na.rm=T),
                   q1=quantile(fact_n,0.25,na.rm=T),
                   q3=quantile(fact_n,0.75,na.rm=T))

# m_m_fact_n m_sd_fact_n sd_m_fact_n
# 66.6        62.9        46.0

