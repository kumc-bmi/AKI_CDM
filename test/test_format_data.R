#### test format_data() ####
rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "broom"))

dat<-readRDS("./data/AKI_DEMO.rda")
dat_out<-dat%>% dplyr::select(-PATID) %>%
  filter(key %in% c("AGE","SEX","RACE","HISPANIC")) %>%
  group_by(ENCOUNTERID,key) %>%
  top_n(n=1L,wt=value) %>% #randomly pick one if multiple entries exist
  ungroup %>% 
  mutate(cat=value,dsa=-1,key_cp=key,
         value2=ifelse(key=="AGE",value,"1")) %>%
  unite("key2",c("key_cp","cat"),sep="_") %>%
  mutate(key=ifelse(key=="AGE",key,key2),
         value=as.numeric(value2)) %>%
  dplyr::select(ENCOUNTERID,key,value,dsa)

############med##########
dat<-readRDS("./data/AKI_MED.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  group_by(ENCOUNTERID,key) %>%
  arrange(dsa) %>%
  dplyr::mutate(value=cumsum(value)) %>%
  ungroup %>%
  mutate(key=paste0(key,"_cum")) %>%
  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
  bind_rows(dat %>% dplyr::select(-PATID) %>%
              dplyr::select(ENCOUNTERID,key,value,dsa) %>%
              unique)
#passed

############dx############
dat<-readRDS("./data/AKI_DX.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  group_by(ENCOUNTERID,key,dsa) %>%
  dplyr::summarize(value=(n() >= 1)*1) %>%
  ungroup %>%
  group_by(ENCOUNTERID,key) %>%
  top_n(n=1L,wt=dsa) %>%
  ungroup


############px############
dat<-readRDS("./data/AKI_PX.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  group_by(ENCOUNTERID,key,dsa) %>%
  dplyr::summarize(value=(n() >= 1)*1) %>%
  ungroup


############lab###########
dat<-readRDS("./data/AKI_LAB.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  unite("key_unit",c("key","unit"),sep="@") %>%
  group_by(ENCOUNTERID,key_unit,dsa) %>%
  dplyr::summarize(value=mean(value,na.rm=T)) %>%
  ungroup %>%
  dplyr::rename(key=key_unit) %>%
  dplye::select(ENCOUTNERID,key,value,dsa)

#calculated new features: BUN/SCr ratio (same-day)
bun_scr_ratio<-dat_out %>% 
  mutate(key_agg=case_when(key %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
                                      '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
                                      '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9') ~ "SCR",
                           key %in% c('12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
                                      '11065-0','3094-0','35234-4','14937-7') ~ "BUN",
                           key %in% c('3097-3','44734-2') ~ "BUN_SCR")) %>%
  filter(toupper(unit) %in% c("MG/DL","MG/MG") & key_agg %in% c("SCR","BUN","BUN_SCR")) %>%
  group_by(ENCOUNTERID,key_agg,dsa) %>%
  dplyr::summarize(value=mean(value,na.rm=T)) %>%
  ungroup %>%
  spread(key_agg,value) %>%
  filter((!is.na(SCR)&!is.na(BUN))|!is.na(BUN_SCR)) %>%
  mutate(BUN_SCR = ifelse(is.na(BUN_SCR),round(BUN/SCR,2),BUN_SCR)) %>%
  mutate(key="BUN_SCR") %>%
  dplyr::rename(value=BUN_SCR) %>%
  dplyr::select(ENCOUNTERID,key,value,dsa)

#engineer new features: change of lab from last collection
lab_delta_eligb<-dat_out %>%
  group_by(ENCOUNTERID,key) %>%
  dplyr::mutate(lab_cnt=length(unique(dsa))) %>%
  ungroup %>%
  group_by(key) %>%
  dplyr::summarize(p5=quantile(lab_cnt,probs=0.05,na.rm=T),
                   p25=quantile(lab_cnt,probs=0.25,na.rm=T),
                   med=median(lab_cnt,na.rm=T),
                   p75=quantile(lab_cnt,probs=0.75,na.rm=T),
                   p95=quantile(lab_cnt,probs=0.95,na.rm=T))

#--collect changes of lab only for those are regularly repeated
lab_delta<-dat_out %>%
  semi_join(lab_delta_eligb %>% filter(med>=2),
            by="key")
dsa_rg<-seq(min(lab_delta$dsa),max(lab_delta$dsa))
lab_delta %<>%
  bind_rows(data.frame(ENCOUNTERID = rep(0,length(dsa_rg)),
                       key=rep("0",length(dsa_rg)),
                       value=NA,
                       dsa=dsa_rg,
                       stringsAsFactor = F)) %>%
  spread(dsa,value) %>%
  gather(dsa,value,-ENCOUNTERID,-key) %>%
  group_by(ENCOUNTERID,key) %>%
  arrange(dsa) %>%
  dplyr::mutate(value=fill(value,.direction="down")) %>%
  dplyr::mutate(value=fill(value,.direction="up")) %>%
  dplyr::mutate(value_lag=lag(value,n=1L,default=value[1])) %>%
  ungroup %>%
  mutate(value=value-value_lag,
         key=paste0(key,"_change")) %>%
  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
  unique



############vital###############
#vital-smoking, tabacco, tobacco_type
dat<-readRDS("./data/AKI_VITAL.rda")
dat_out<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
  group_by(ENCOUNTERID,key) %>%
  arrange(value) %>% slice(1:1) %>%
  ungroup

dat_out %>%
  group_by(ENCOUNTERID) %>%
  dplyr::summarize(cnt=length(unique(key)),
                   cnt_dup=length(key)) %>%
  ungroup %>%
  group_by(cnt,cnt_dup) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTERID))) %>%
  ungroup %>%
  View

#vital-ht,wt,bmi
dat_out<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("HT","WT","BMI")) %>%
  group_by(ENCOUNTERID,key) %>%
  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
  ungroup

dat_out %>%
  group_by(ENCOUNTERID) %>%
  dplyr::summarize(cnt=length(unique(key)),
                   cnt_dup=length(key)) %>%
  ungroup %>%
  group_by(cnt,cnt_dup) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTERID))) %>%
  ungroup %>%
  View

#vital-bp
bp<-dat %>% dplyr::select(-PATID) %>%
  filter(key %in% c("BP_DIASTOLIC","BP_SYSTOLIC")) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(value=ifelse((key=="BP_DIASTOLIC" & (value>120 | value<40))|
                        (key=="BP_SYSTOLIC" & (value>210 | value<40)),NA,value)) %>%
  group_by(ENCOUNTERID,key,dsa) %>%
  dplyr::mutate(value_imp=median(value,na.rm=T)) %>%
  ungroup

bp %<>%
  filter(!is.na(value_imp)) %>%
  mutate(imp_ind=ifelse(is.na(value),1,0)) %>%
  mutate(value=ifelse(is.na(value),value_imp,value)) %>%
  dplyr::select(-value_imp) 

bp %>% 
  group_by(key) %>%
  dplyr::summarize(enc_cnt=length(unique(ENCOUNTERID)),
                   cnt=n(),
                   imp_cnt=sum(imp_ind)) %>%
  ungroup %>%
  mutate(imp_rt=round(imp_cnt/cnt,2)) %>%
  View

bp %<>% select(-imp_ind)
bp_min<-bp %>%
  group_by(ENCOUNTERID,key,dsa) %>%
  dplyr::summarize(value_lowest=min(value,na.rm=T)) %>%
  ungroup %>%
  mutate(key=paste0(key,"_min"))

i<-1
bp_slp_obj<-bp %>%
  filter(as.numeric(format(timestamp,"%Y"))==bp_chk$yr[i]) %>%
  mutate(add_hour=difftime(timestamp,format(timestamp,"%Y-%m-%d"),units="hours")) %>%
  mutate(timestamp=dsa+sign(dsa)*round(as.numeric(add_hour)/24,2)) %>%
  dplyr::select(-add_hour) %>%
  group_by(ENCOUNTERID,key,dsa) %>%
  do(fit_val=lm(value ~ timestamp,data=.))

bp_slp<-tidy(bp_slp_obj,fit_val) %>%
  filter(term=="timestamp") %>%
  dplyr::rename(value=estimate) %>%
  ungroup %>%
  mutate(key=paste0(key,"_slope"))

bp_slp %<>%
  mutate(value=ifelse(p.value>0.5 | is.nan(p.value),0,value)) %>%
  dplyr::select(ENCOUNTERID,key,dsa,value)
#passed!




