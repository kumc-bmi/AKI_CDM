#### survival-like prediction model ####
format_data<-function(dat,type=c("demo","vital","lab","dx","px","med")){
  if(type=="demo"){
    #demo has to be unqiue for each encounter
    dat_out<-dat%>% dplyr::select(-PATID) %>%
      filter(key %in% c("AGE","SEX","RACE","HISPANIC")) %>%
      group_by(ENCOUNTERID,key) %>%
      top_n(n=1L,wt=value) %>% #randomly pick one if multiple entries exist
      ungroup
  }else if(type=="vital"){
    dat_out<-c()
    
    #multiple smoking status is resolved by using the most recent record
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("SMOKING","TOBACCO","TOBACCO_TYPE")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  arrange(value) %>% slice(1:1) %>%
                  ungroup)
    
    #multiple ht,wt,bmi resolved by taking median
    dat_out %<>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  filter(key %in% c("HT","WT","BMI")) %>%
                  group_by(ENCOUNTERID,key) %>%
                  dplyr::summarize(value=median(as.numeric(value),na.rm=T)) %>%
                  ungroup)

    #multiple bp are aggregated by taking: lowest & slope
    bp<-dat %>% dplyr::select(-PATID) %>%
      filter(key %in% c("BP_DIASTOLIC","BP_SYSTOLIC")) %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=ifelse((key=="BP_DIASTOLIC" & (value>120 | value<40))|
                          (key=="BP_SYSTOLIC" & (value>210 | value<40)),NA,value)) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::mutate(value_imp=median(value,na.rm=T)) %>%
      ungroup %>%
      filter(!is.na(value_imp)) %>%
      mutate(value=ifelse(is.na(value),value_imp,value)) %>%
      dplyr::select(-value_imp) 
    
    #--minimal bp
    bp_min<-bp %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      dplyr::summarize(value_lowest=min(value,na.rm=T)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_min"))
    
    #--trend of bp
    bp_slp_obj<-bp %>%
      mutate(add_hour=difftime(timestamp,format(timestamp,"%Y-%m-%d"),units="hours")) %>%
      mutate(timestamp=dsa+round(as.numeric(add_hour)/24,2)) %>%
      dplyr::select(-add_hour) %>%
      group_by(ENCOUNTERID,key,dsa) %>%
      do(fit_val=lm(value ~ timestamp,data=.))
    
    bp_slp<-tidy(bp_slp_obj,fit_val) %>%
      filter(term=="timestamp") %>%
      dplyr::rename(value=estimate) %>%
      mutate(key=paste0(key,"_slope"))
    
    bp<-bp_min %>% 
      dplyr::select(ENCOUNTERID,key,value,dsa) %>%
      bind_rows(bp_slp %>% 
                  dplyr::select(ENCOUNTERID,key,value,dsa))
    
    dat_out %<>%
      mutate(dsa_int=-1) %>% bind_rows(bp)
    
    rm(bp,bp_min,bp_slp_obj,bp_slp)
    gc()
  }else if(type=="lab"){
    #multiple same lab on the same will be resolved by taking the average
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      unite("key_unit",c("key","unit"),sep="@") %>%
      group_by(ENCOUNTERID,key_unit,dsa) %>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      ungroup %>%
      dplyr::rename(key=key_unit) %>%
      dplye::select(ENCOUTNERID,key,value,dsa)
    
    #calculated new features: BUN/SCr ratio (same-day)
    bun_scr_ratio<-dat_out %>% 
      filter(key %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8',
                        )) %>%
      mutate(key_agg=case_when(key %in% c('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8') ~ "SCR",
                               key %in% c('3094-0','6299-2',) ~ "BUN",
                               key %in% c('3097-3' ~ "BUN_SCR")))
    
    #engineer new features: change of lab from last collection
    
    
    
  }else if(type %in% c("dx","px")){
    #multiple records resolved as "present (1) or absent (0)"
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      mutate(dsa_int=round(dsa)) %>%
      group_by(ENCOUNTERID,key,dsa_int) %>%
      dplyr::summarize(value=(n() >= 1)*1) %>%
      ungroup
      
  }else if(type=="med"){
    #multiple records accumulated
    dat_out<-dat %>% dplyr::select(-PATID) %>%
      mutate(dsa_int=round(dsa)) %>%
      group_by(ENCOUNTERID,key) %>%
      arrange(dsa_int) %>%
      dplyr::mutate(value=cumsum(value)) %>%
      ungroup %>%
      mutate(key=paste0(key,"_cum")) %>%
      dplyr::select(ENCOUNTERID,key,value, dsa_int) %>%
      bind_rows(dat %>% dplyr::select(-PATID) %>%
                  mutate(dsa_int=round(dsa)) %>%
                  dplyr::select(ENCOUNTERID,key,value, dsa_int) %>%
                  unique)
  }
  
  return(dat_out)
}


setwd("~/dkd/DKD_PM")

rm(list=ls()); gc()
source("../helper_functions.R")
require_libraries(c( "Matrix"
                     ,"pROC"
                     ,"xgboost"
                     ,"dplyr"
                     ,"tidyr"
                     ,"magrittr"
))

# Load data
load("./data/DKD_heron_facts_prep.Rdata")
load("./data/pat_episode.Rdata")

# time_iterv<-"3mth"
# time_iterv<-"6mth"
time_iterv<-"1yr"

# ep_unit<-90
# ep_unit<-182.5
ep_unit<-365.25
pat_episode<-pat_tbl %>%
  dplyr::mutate(episode = floor(as.numeric(DAY_SINCE)/ep_unit))

#### stack temporal info
x_add<-pat_episode %>%
  dplyr::select(PATIENT_NUM,episode) %>% #each patient has multiple episodes
  dplyr::mutate(VARIABLE=paste0("ep_",episode),
                NVAL_NUM=1) %>% #use episode indicator as additional feature
  unique


#collect all historical values
X_long<-fact_stack %>% 
  #take out all clinical facts for the training points
  inner_join(x_add %>% dplyr::select(PATIENT_NUM, episode) %>%
               group_by(PATIENT_NUM) %>% top_n(n=1,wt=episode) %>% ungroup %>% unique,
             by="PATIENT_NUM") %>%
  #collect all historical values 1-episode prior to eventual target
  filter(day_from_dm < episode*ep_unit) %>%
  #merge pre-DM facts together (day_from_dm = -1)
  dplyr::mutate(day_from_dm = ifelse(day_from_dm < 0,-1,day_from_dm)) %>%
  #associate concepts with timing
  dplyr::mutate(episode_x = floor(day_from_dm/ep_unit))

rm(fact_stack); gc()

# unique(X_long$VARIABLE_CATEG)
# [1] "alerts"         "allergy"       "demographics"  "diagnoses"     "history"       "labs"          "medications"   "orders"        "visit_details" "procedures"   
# [11] "uhc"           "engineered"   

#collect time-variant features
#if exists at least 1 entry within time window t, then 1
#for data types such as:
# -alerts
# -allergy
# -diagnosis
# -history (except for KUMC|PACK_PER_DAY,KUMC|TOBACCO_USED_YEARS)
# -orders
# -procedues
# -uhc
# -visit_details (except for KUH|PAT_ENC:)

X_t1<-X_long %>%
  dplyr::filter(VARIABLE_CATEG %in% c("alerts",
                                      "allergy",
                                      "diagnosis",
                                      "history",
                                      "orders",
                                      "visit_details")) %>%
  dplyr::filter(!(VARIABLE_CATEG == "history" & 
                    grepl("(PACK_PER_DAY)|(TOBACCO_USED_YEARS)",CONCEPT_CD))) %>%
  dplyr::filter(!(VARIABLE_CATEG == "visit_details" & grepl("(PAT_ENC)+",CONCEPT_CD))) %>%
  group_by(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x) %>%
  top_n(n=-1,wt=day_to_end) %>% ungroup %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x,day_from_dm,NVAL_NUM) %>%
  unique

#eyeball an example
X_t1 %>% filter(PATIENT_NUM == 70) %>% arrange(day_from_dm) %>% View

#if exists multiple entries within time window t, then use counts of distinct observations
#for data type such as:
# -medications (roll-up concept)
X_t2<-X_long %>%
  dplyr::filter(VARIABLE_CATEG == "medications") %>%
  group_by(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x) %>%
  dplyr::summarize(NVAL_NUM = length(unique(START_DATE))) %>%
  ungroup %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x,NVAL_NUM) %>%
  unique

#eyeball an example
X_t2 %>% filter(PATIENT_NUM == 70) %>% View


#if exists multiple entries of cumulative values within time window t, then only use the max (worst-case senario)
#for data type such as:
# -engineered (fact_cnt)
# -history (KUMC|PACK_PER_DAY,KUMC|TOBACCO_USED_YEARS)
X_t3<-X_long %>%
  dplyr::filter((VARIABLE_CATEG == "engineered" & CONCEPT_CD %in% c("fact_cnt"))|
                  (VARIABLE_CATEG == "history" & grepl("(PACK_PER_DAY)|(TOBACCO_USED_YEARS)",CONCEPT_CD))) %>%
  group_by(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x) %>%
  dplyr::summarize(NVAL_NUM = max(NVAL_NUM,na.rm=T)) %>%
  ungroup %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,CONCEPT_CD,episode_x,NVAL_NUM) %>%
  unique

#eyeball an example
X_t3 %>% filter(PATIENT_NUM == 70) %>%  View


#if exists multiple entries of distinct values within time window t, then use aggregated value
#for data type such as:
# -labs
# -visit_details (vitals)
# -engineered (newfact_cnt_slast)
X_t4<-X_long %>% 
  dplyr::filter((VARIABLE_CATEG %in% c("labs")) |
                  (VARIABLE_CATEG == "visit_details" & grepl("(PAT_ENC)+",CONCEPT_CD)) |
                  (VARIABLE_CATEG == "engineered" & CONCEPT_CD %in% c("newfact_cnt_slast")))

# set aside time-invariant variables (only single entry is available within the time frame)
id_invar<-X_t4 %>%
  group_by(PATIENT_NUM,CONCEPT_CD,episode_x) %>%
  #recording frequency at individual level (per patient-yr since DMonset)
  dplyr::mutate(pt_freq=length(unique(START_DATE))) %>%
  ungroup %>% group_by(CONCEPT_CD,episode_x) %>%
  dplyr::summarize(pt_expos = length(unique(PATIENT_NUM)),
                   avg_pt_freq = mean(pt_freq,na.rm=T),
                   sd_pt_freq = sd(pt_freq,na.rm=T)) %>%
  ungroup %>%
  mutate(invar_ind = ifelse((pt_expos <= 30 | 
                               avg_pt_freq + 3*sd_pt_freq < 3),1,0)) #TODO: better threshold

#quick check of temporal-differentiable variables
id_invar %>% group_by(invar_ind) %>%
  dplyr::summarize(var_cnt = n()) %>% View


# save the intermetiate lab frequency table for some further analysis
heron_terms<-read.csv("./data/heron_terms.csv",header=T,stringsAsFactors=F)
temporal_profile<-id_invar %>%
  dplyr::mutate(link_concept=gsub("[#@].*$","",CONCEPT_CD)) %>%
  left_join(heron_terms %>% 
              dplyr::select(C_BASECODE, C_NAME, C_VISUAL_PATH) %>% unique,
            by=c("link_concept"="C_BASECODE")) %>%
  dplyr::select(-link_concept)
save(temporal_profile,file="./data/temporal_profile.Rdata")


# only aggregate values for variables with multiple entries
X_var_long<-X_t4 %>%
  semi_join(id_invar %>% filter(invar_ind == 0),by=c("CONCEPT_CD","episode_x")) %>%
  dplyr::select(PATIENT_NUM, VARIABLE_CATEG, CONCEPT_CD, episode_x, NVAL_NUM) %>%
  unique %>% group_by(PATIENT_NUM, VARIABLE_CATEG, CONCEPT_CD, episode_x) %>%
  do(mutate(.,
            agg_min = min(NVAL_NUM,na.rm=T),
            agg_max = max(NVAL_NUM,na.rm=T),
            agg_mean = mean(NVAL_NUM,na.rm=T),
            agg_sd = sd(NVAL_NUM,na.rm=T))) %>%
  ungroup


X_var_long %<>%
  dplyr::select(-NVAL_NUM) %>%
  gather(agg,NVAL_NUM,-PATIENT_NUM,-VARIABLE_CATEG,-CONCEPT_CD,-episode_x) %>%
  dplyr::mutate(episode = episode_x) %>%
  unite("VARIABLE",c("CONCEPT_CD","agg")) %>%
  filter(!is.na(NVAL_NUM)) %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM, episode_x) %>%
  unique


# collect time-invariant variables
X_fix_long<-X_t4 %>%
  semi_join(id_invar %>% filter(invar_ind == 1),by=c("CONCEPT_CD","episode_x")) %>%
  group_by(PATIENT_NUM, VARIABLE_CATEG, CONCEPT_CD, episode_x, NVAL_NUM) %>%
  top_n(n=-1,wt=day_to_end) %>% ungroup %>%
  dplyr::mutate(episode = episode_x,VARIABLE = CONCEPT_CD) %>%
  # unite("VARIABLE_ep",c("CONCEPT_CD","episode")) %>%
  filter(!is.na(NVAL_NUM)) %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM,episode_x) %>%
  unique

X_t4<-X_var_long %>% bind_rows(X_fix_long)


#eyeball an example
X_t4 %>% filter(PATIENT_NUM == 70) %>%  View


#multiple entries could exist, but should only use one value for all time windows
#for data type such as:
# -demographics (age(at DM onset), gender,race,ethnicity)
X_t5<-X_long %>%
  filter(VARIABLE_CATEG == "demographics") %>%
  dplyr::rename(VARIABLE = CONCEPT_CD) %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM,episode_x) %>%
  unique

#eyeball an example
X_t5 %>% filter(PATIENT_NUM == 70) %>%  View


#re-construct X matrix
X_long<-X_t1 %>% dplyr::rename(VARIABLE = CONCEPT_CD) %>%
  dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM,episode_x) %>%
  bind_rows(X_t2 %>% dplyr::rename(VARIABLE = CONCEPT_CD) %>%
              dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM,episode_x)) %>%
  bind_rows(X_t3 %>% dplyr::rename(VARIABLE = CONCEPT_CD) %>%
              dplyr::select(PATIENT_NUM,VARIABLE_CATEG,VARIABLE,NVAL_NUM,episode_x)) %>%
  bind_rows(X_t4) %>%
  bind_rows(X_t5)

#eyeball an example
X_long %>% filter(PATIENT_NUM == 70) %>%  View

# View(X_long %>% group_by(VARIABLE_CATEG,episode_x) %>%
#        dplyr::summarize(fact_cnt=n(),
#                         pat_cnt=length(unique(PATIENT_NUM)),
#                         cd_cnt=length(unique(VARIABLE))))

#save results
save(X_long,file="./data/X_long.Rdata")

rm(list=ls())
gc()

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