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
rsample_idx<-c()
for(i in seq_along(enc_yr)){
  start_i<-Sys.time()
  cat("start variable collection for year",enc_yr[i],".\n")
  
  #--collect end_points
  dat_i<-tbl1 %>% filter(yr==enc_yr[i]) %>%
    dplyr::select(ENCOUNTERID,NONAKI_SINCE_ADMIT,
                  AKI1_SINCE_ADMIT,AKI2_SINCE_ADMIT,AKI3_SINCE_ADMIT) %>%
    gather(y,dsa_y,-ENCOUNTERID) %>%
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
  var_at<-c()
  var_at_bm<-c()
  for(v in seq_along(var_type)){
    start_v<-Sys.time()
    
    #extract
    var_v<-readRDS(paste0("./data/AKI_",toupper(var_type[v]),".rda")) %>%
      semi_join(dat_i,by="ENCOUNTERID")
    
    #transform
    var_v<-format_data(var_v,type=var_type[v])
    var_v<-get_dsurv_temporal(dat_i,var_v)
    
    #load
    var_at %<>% bind_rows(var_v)
    
    lapse_v<-Sys.time()-start_v
    var_at_bm<-c(var_at_bm,paste0(lapse_v,units(lapse_v)))
    cat("\n...collect and transform",var_type[v],"for year",enc_yr[i],"in",lapse_v,units(lapse_v),".\n")
  }
  var_by_yr[[i]]<-var_at
  
  lapse_i<-Sys.time()-start_i
  var_at_bm<-paste0(lapse_i,units(lapse_i))
  cat("\nfinish variabl collection for year",enc_yr[i],"in",lapse_i,units(lapse_i),".\n")
  
  var_bm[[i]]<-data.frame(bm_nm=c(var_type,"overall"),
                          bm_time=var_at_bm,
                          stringsAsFactors = F)
}
saveRDS(var_by_yr,file="./data/var_by_yr.rda")
saveRDS(var_bm,file="./data/var_dm.rda")

##### sampling #####
#leave out encounters after 2017-01-01 as temporal holdout
#-- random sampling



#-- balance case and control



#-- balance case and control based on pathway matching


