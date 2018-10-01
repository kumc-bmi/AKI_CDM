#### survival-like prediction model ####
rm(list=ls()); gc()

source("./R/util.R")
source("./R/var_etl_surv.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom"))


######collect and format variables on daily basis#######
tbl1<-readRDS("./data/Table1.rda") %>%
  mutate(yr=as.numeric(format(ADMIT_DATE,"%Y")))

onset_dt<-c(tbl1$AKI1_SINCE_ADMIT,tbl1$AKI2_SINCE_ADMIT,tbl1$AKI3_SINCE_ADMIT)
quantile(onset_dt,probs=0:20/20,na.rm=T)
dsa_rg<-seq(0,30)

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
for(i in seq_along(enc_yr)){
  start_i<-Sys.time()
  cat("start variabl collection for year",enc_yr[i],".\n")
  
  dat_i<-tbl1 %>% filter(yr==enc_yr[i])
  var_at<-c()
  var_at_bm<-c()
  for(v in seq_along(var_type)){
    start_v<-Sys.time()
    
    var_v<-readRDS(paste0("./data/AKI_",toupper(var_type[v]),".rda")) %>%
      semi_join(dat_i,by="ENCOUNTERID")

    var_at %<>%
      bind_rows(format_data(var_v,type=var_type[v]))
    
    lapse_v<-Sys.time()-start_v
    var_at_bm<-c(var_at_bm,paste0(lapse_v,units(lapse_v)))
    cat("...collect and transform",var_type[v],"for year",enc_yr[i],"in",lapse_v,units(lapse_v),".\n")
  }
  var_by_yr[[i]]<-var_at
  
  lapse_i<-Sys.time()-start_i
  var_at_bm<-paste0(lapse_i,units(lapse_i))
  cat("finish variabl collection for year",enc_yr[i],"in",lapse_i,units(lapse_i),".\n")
  
  var_bm[[i]]<-data.frame(bm_nm=c(var_type,"overall"),
                          bm_time=var_at_bm,
                          stringsAsFactors = F)
}
saveRDS(var_by_yr,file="./data/var_by_yr.rda")
saveRDS(var_bm,file="./data/var_dm.rda")

##### sampling #####
#leave out encounters after 2017-01-01 as temporal holdout
#within training, apply down-sampling strategy:
#-- balance case and control
for(yr in yr_tr){
  
}

#-- balance case and control based on pathway matching


get_dsurv_temporal<-function(pat_episode,X_long){
  #pivot to sparse matrix
  X_long %<>%
    left_join(pat_episode %>% group_by(PATIENT_NUM, episode) %>% 
                top_n(n=1,wt=DKD_IND_additive) %>% ungroup %>%
                dplyr::select(PATIENT_NUM,episode) %>% unique,
              by = "PATIENT_NUM") %>%
    dplyr::filter(episode_x < episode) %>% 
    unite("VARIABLE_ep",c("VARIABLE","episode_x")) %>%
    arrange(PATIENT_NUM, episode) %>%
    unite("PATIENT_NUM_ep",c("PATIENT_NUM","episode")) %>%
    dplyr::select(PATIENT_NUM_ep, VARIABLE_ep, NVAL_NUM) %>%
    long_to_sparse_matrix(.,
                          id="PATIENT_NUM_ep",
                          variable="VARIABLE_ep",
                          val="NVAL_NUM")
  
  X_idx<-data.frame(PATIENT_NUM_ep = row.names(X_long),
                    stringsAsFactors = F)
  
  #collect target
  y_long<-pat_episode %>%
    dplyr::select(PATIENT_NUM,episode, DKD_IND_additive) %>% unique %>%
    group_by(PATIENT_NUM, episode) %>% 
    top_n(n=1,wt=DKD_IND_additive) %>% ungroup %>%
    unite("PATIENT_NUM_ep",c("PATIENT_NUM","episode")) %>%
    semi_join(X_idx,by="PATIENT_NUM_ep") %>%
    arrange(PATIENT_NUM_ep)
  
  #alignment check
  align_row<-all((row.names(X_long)==y_long$PATIENT_NUM_ep)) # yes
  
  if(!align_row) {
    stop("rows for convariate matrix and target don't align!")
  }
  
  Xy_all<-list(X_ep = X_long,
               y_ep = y_long)
  
  return(Xy_all)
}