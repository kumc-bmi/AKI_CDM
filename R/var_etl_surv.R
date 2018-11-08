#### survival-like data format transformation ####
format_data<-function(dat,type=c("demo","vital","lab","dx","px","med"),pred_end){
  if(type=="demo"){
    #demo has to be unqiue for each encounter
    dat_out<-dat %>%
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
    
  }else if(type=="vital"){
    dat_out<-c()
    
    #multiple smoking status is resolved by using the most recent record
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  arrange(value) %>% dplyr::slice(1:1) %>%
                  ungroup %>%
                  mutate(cat=value,dsa=-1,key_cp=key,value=1) %>%
                  unite("key",c("key_cp","cat"),sep="_") %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa))
                  
    
    #multiple ht,wt,bmi resolved by taking median
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("HT","WT","BMI")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
                  ungroup %>% mutate(dsa=-1))

    #multiple bp are aggregated by taking: lowest & slope
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
    
    bp %<>% dplyr::select(-imp_ind)
    #--minimal bp
    bp_min<-bp %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value_lowest=min(value,na.rm=T)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_min")) %>%
      dplyr::rename(value=value_lowest)
    
    #--trend of bp
    bp_slp_eligb<-bp %>%
      mutate(add_time=difftime(timestamp,format(timestamp,"%Y-%m-%d"),units="mins")) %>%
      mutate(timestamp=round(as.numeric(add_time)/(24*60),2)) %>%
      dplyr::select(-add_time) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::mutate(df=length(unique(timestamp))-1) %>%
      dplyr::mutate(sd=ifelse(df>0,sd(value),0))
    
    bp_slp_obj<-bp_slp_eligb %>%
      filter(df > 1 & sd >= 1e-2) %>%
      do(fit_val=glm(value ~ timestamp,data=.))
    
    bp_slp<-tidy(bp_slp_obj,fit_val) %>%
      filter(term=="timestamp") %>%
      dplyr::rename(value=estimate) %>%
      ungroup %>%
      mutate(value=ifelse(p.value>0.5 | is.nan(p.value),0,value)) %>%
      dplyr::select(ENCOUNTERID,key,dsa,value) %>%
      bind_rows(bp_slp_eligb %>% 
                  filter(df<=1 | sd < 1e-2) %>% mutate(value=0) %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                  ungroup %>% unique) %>%
      bind_rows(bind_rows(bp_slp_eligb %>% 
                            filter(df==1 & sd >= 1e-2) %>% 
                            mutate(value=round((max(value)-min(value))/(max(timestamp)-min(timestamp)),2)) %>%
                            dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                            ungroup %>% unique)) %>%
      mutate(key=paste0(key,"_slope"))
    
    #--stack bp
    bp<-bp_min %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(bp_slp %>% 
                  dplyr::select(ENCOUNTERID,key,value,dsa))

    #all vitals
    dat_out %<>%
      mutate(dsa=-1) %>% bind_rows(bp)
    
    #clean out some memories
    rm(bp,bp_min,bp_slp_eligb,bp_slp_obj,bp_slp)
    gc()
    
  }else if(type=="lab"){
    #multiple same lab on the same day will be resolved by taking the average
    dat_out<-dat %>%
      filter(key != "NI") %>%
      mutate(key_cp=key,unit_cp=unit) %>%
      unite("key_unit",c("key_cp","unit_cp"),sep="@") %>%
      group_by(ENCOUNTERID,key,unit,key_unit,dsa) %>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      ungroup
    
    #calculated new features: BUN/SCr ratio (same-day)
    bun_scr_ratio<-dat_out %>% 
      mutate(key_agg=case_when(key %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
                                          '16188-5','16189-3','59826-8','35591-7','50380-5','50381-3','35592-5',
                                          '44784-7','11041-1','51620-3','72271-0','11042-9','51619-5','35203-9','14682-9') ~ "SCR",
                               key %in% c('12966-8','12965-0','6299-2','59570-2','12964-3','49071-4','72270-2',
                                          '11065-0','3094-0','35234-4','14937-7') ~ "BUN",
                               key %in% c('3097-3','44734-2') ~ "BUN_SCR")) %>% #not populated
      filter((toupper(unit) %in% c("MG/DL","MG/MG")) & 
               (key_agg %in% c("SCR","BUN","BUN_SCR"))) %>%
      group_by(ENCOUNTERID,key_agg,dsa) %>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      ungroup %>%
      spread(key_agg,value) %>%
      filter(!is.na(SCR)&!is.na(BUN)) %>%
      mutate(BUN_SCR = round(BUN/SCR,2)) %>%
      mutate(key="BUN_SCR") %>%
      dplyr::rename(value=BUN_SCR) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
    #engineer new features: change of lab from last collection
    lab_delta_eligb<-dat_out %>%
      group_by(ENCOUNTERID,key) %>%
      dplyr::mutate(lab_cnt=sum(dsa<=pred_end)) %>%
      ungroup %>%
      group_by(key) %>%
      dplyr::summarize(p5=quantile(lab_cnt,probs=0.05,na.rm=T),
                       p25=quantile(lab_cnt,probs=0.25,na.rm=T),
                       med=median(lab_cnt,na.rm=T),
                       p75=quantile(lab_cnt,probs=0.75,na.rm=T),
                       p95=quantile(lab_cnt,probs=0.95,na.rm=T))
    
    #--collect changes of lab only for those are regularly repeated
    lab_delta<-dat_out %>%
      semi_join(lab_delta_eligb %>% filter(med>=(pred_end-1)),
                by="key")
    
    dsa_rg<-seq(0,pred_end)

    lab_delta %<>%
      group_by(ENCOUNTERID,key) %>%
      dplyr::mutate(dsa_max=max(dsa)) %>%
      filter(dsa<=dsa_max) %>%
      arrange(dsa) %>%
      dplyr::mutate(value_lag=lag(value,n=1L,default=NA)) %>%
      ungroup %>%
      filter(!is.na(value_lag)) %>%
      mutate(value=value-value_lag,
             key=paste0(key,"_change")) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      unique
    
    dat_out %<>%
      bind_rows(bun_scr_ratio) %>%
      bind_rows(lab_delta)
    
  }else if(type == "dx"){
    #multiple records resolved as "present (1) or absent (0)"
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value=(n() >= 1)*1) %>%
      ungroup %>%
      group_by(ENCOUNTERID,key) %>%
      top_n(n=1L,wt=dsa) %>%
      ungroup %>% 
      mutate(key=as.character(key)) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
  }else if(type == "px"){
    #multiple records resolved as "present (1) or absent (0)"
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value=(n() >= 1)*1) %>%
      ungroup %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa)
      
  }else if(type=="med"){
    #multiple records accumulated
    dat_out<-dat %>%
      group_by(ENCOUNTERID,key) %>%
      arrange(dsa) %>%
      dplyr::mutate(value=cumsum(value)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_cum")) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(dat %>%
                  dplyr::select(ENCOUNTERID,key,value,dsa) %>%
                  unique)
  }

  return(dat_out)
}

#tw should be the same time unit as dsa
get_dsurv_temporal<-function(dat,censor,tw){
  y_surv<-c()
  X_surv<-c()
  for(t in tw){
    #stack y
    censor_t<-censor %>%
      mutate(pred_pt=case_when(dsa_y >= t ~ t,
                               dsa_y <  t ~ NA_real_),
             y_ep=case_when(dsa_y == t ~ y,
                            dsa_y >  t ~ pmax(0,y-1),
                            dsa_y <  t ~ NA_real_)) %>%
      filter(!is.na(pred_pt)) %>%
      group_by(ENCOUNTERID) %>%
      arrange(desc(pred_pt),desc(y_ep)) %>%
      dplyr::slice(1:1) %>%
      ungroup %>%
      mutate(dsa_y=pred_pt,y=y_ep) %>%
      dplyr::select(-pred_pt,-y_ep)
    
    y_surv %<>% 
      bind_rows(censor_t %>%
                  dplyr::select(ENCOUNTERID,dsa_y,y))
    
    #stack x
    X_surv %<>% 
      bind_rows(dat %>% left_join(censor,by="ENCOUNTERID") %>%
                  filter(dsa < dsa_y) %>%
                  group_by(ENCOUNTERID,key) %>%
                  top_n(n=1,wt=-dsa) %>%
                  ungroup %>%
                  dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value) %>%
                  bind_rows(censor_t %>% 
                              mutate(dsa=dsa_y-1,
                                     key=paste0("day",(dsa_y-1)),
                                     value=1) %>%
                              dplyr::select(ENCOUNTERID,dsa_y,dsa,key,value)))
  }

  Xy_surv<-list(X_surv = X_surv,
                y_surv = y_surv)
  
  return(Xy_surv)
}
