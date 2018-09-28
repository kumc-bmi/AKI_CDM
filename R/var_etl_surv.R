#### survival-like data format transformation ####
format_data<-function(dat,type=c("demo","vital","lab","dx","px","med")){
  if(type=="demo"){
    #demo has to be unqiue for each encounter
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
    
  }else if(type=="vital"){
    dat_out<-c()
    
    #multiple smoking status is resolved by using the most recent record
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  arrange(value) %>% slice(1:1) %>%
                  ungroup %>% mutate(value=as.character(value),dsa=-1))
    
    #multiple ht,wt,bmi resolved by taking median
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("HT","WT","BMI")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
                  ungroup %>% mutate(value=as.character(value),dsa=-1))

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
      mutate(key=paste0(key,"_min"))
    
    #--trend of bp
    bp_slp_obj<-bp %>%
      mutate(add_hour=difftime(timestamp,format(timestamp,"%Y-%m-%d"),units="hours")) %>%
      mutate(timestamp=dsa+sign(dsa)*round(as.numeric(add_hour)/24,2)) %>%
      dplyr::select(-add_hour) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      do(fit_val=lm(value ~ timestamp,data=.))
    
    bp_slp<-tidy(bp_slp_obj,fit_val) %>%
      filter(term=="timestamp") %>%
      dplyr::rename(value=estimate) %>%
      ungroup %>%
      mutate(key=paste0(key,"_slope")) %>%
      mutate(value=ifelse(p.value>0.5 | is.nan(p.value),0,value)) %>%
      dplyr::select(ENCOUNTERID,key,value,dsa)
    
    #--stack bp
    bp<-bp_min %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(bp_slp %>% 
                  dplyr::select(ENCOUNTERID,key,value,dsa))

    #all vitals
    dat_out %<>%
      mutate(dsa=-1) %>% bind_rows(bp)
    
    #clean out some memories
    rm(bp,bp_min,bp_slp_obj,bp_slp)
    gc()
    
  }else if(type=="lab"){
    #multiple same lab on the same day will be resolved by taking the average
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
    
    dat_out %>%
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
  }
  return(dat_out)
}
