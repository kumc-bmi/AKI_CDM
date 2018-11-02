#source utility functions
source("./R/util.R")

#load libraries
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr",
                    "openxlsx"))


wb_lst<-c("KUMC_AKI_CDM_EXT_VALID_p1_QA_TBL",
          "UTHSCA_AKI_CDM_EXT_VALID_p1_QA_TBL",
          "UIOWA_AKI_CDM_EXT_VALID_p1_QA_TBL")

# stack up consort diagram
consort<-c()
for (wb in seq_len(length(wb_lst))) {
  consort %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=1) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}


# stack up AKI times
aki_time<-c()
for (wb in seq_len(length(wb_lst))) {
  aki_time %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=2) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}

# stack up demo
demo<-c()
for (wb in seq_len(length(wb_lst))) {
  demo %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=3) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}



# stack up historical diagnosis
ccs<-c()
for (wb in seq_len(length(wb_lst))) {
  ccs %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=7) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}



####comparison tables
##==AKI stages & demographics
demo %<>%
  mutate(value=trimws(value,"both")) %>%
  gather(aki_stg,summ,-key,-value,-site) %>%
  separate(summ,c("cnt","prop"),sep=",") %>%
  mutate(cnt=ifelse(cnt=="<11","10",cnt)) %>%
  mutate(cnt=as.numeric(cnt),
         prop=as.numeric(gsub("\\%","",prop))/100) %>%
  mutate(stg_bin=ifelse(aki_stg=="ADMIT","ALL",
                        ifelse(aki_stg!="NONAKI","AKI",aki_stg)),
         value=case_when(key=="AGE_GRP"&value %in% c("46-55","56-65",">=65") ~ ">45",
                         key=="AGE_GRP"&!(value %in% c("46-55","56-65",">=65")) ~ "<=45",
                         key=="SEX"&value=="F" ~ "Female",
                         key=="SEX"&value=="M" ~ "Male",
                         key=="SEX"&!(value %in% c("F","M")) ~ "Unknown",
                         key=="RACE"&value=="05" ~ "White",
                         key=="RACE"&value=="03" ~ "African American",
                         key=="RACE"&value=="02" ~ "Asian",
                         key=="RACE"&value %in% c("01","04","06","OT") ~ "Other",
                         key=="RACE"&value %in% c("07","NI","UN") ~ "Unknown",
                         key=="HISPANIC"&value=="Y" ~ "Hispanic",
                         key=="HISPANIC"&value=="N" ~ "Non-Hispanic",
                         key=="HISPANIC"&!(value %in% c("Y","N")) ~ "Unknown")) %>%
  group_by(site,stg_bin,key,value) %>%
  dplyr::summarize(cnt=sum(cnt)) %>%
  ungroup

#--collect denominator
demo_denom<-demo %>%
  filter(stg_bin=="ALL") %>%
  dplyr::select(site,key,value,cnt) %>%
  unique

aki_denom<-consort %>% 
  filter(CNT_TYPE == "Total") %>%
  dplyr::select(site,ENC_CNT)


#--get conditional AKI distributions
aki<-aki_time %>% 
  dplyr::select(site,stage,enc_cnt) %>%
  mutate(enc_cnt=as.numeric(enc_cnt)) %>%
  left_join(aki_denom,by="site") %>%
  mutate(prop=round(enc_cnt/ENC_CNT,3)) 

#-------------------test---------------------------#
test_out<-c()

tbl1<-aki %>%
  dplyr::select(stage,site,enc_cnt) %>%
  spread(site,enc_cnt) %>%
  dplyr::select(-stage)
Xsq<-chisq.test(tbl1,simulate.p.value = TRUE)
test_out %<>%
  bind_rows(data.frame(test="overall",
                       Chisq=Xsq$statistic,
                       pval=Xsq$p.value,
                       stringsAsFactors=F))

test_subj<-c("AKI1","AKI2","AKI3")
for(s in test_subj){
  tbl_s<-aki %>%
    filter(stage %in% c(s,"NONAKI")) %>%
    dplyr::select(stage,site,enc_cnt) %>%
    spread(site,enc_cnt) %>%
    dplyr::select(-stage)
  
  Xsq<-chisq.test(tbl_s,simulate.p.value = TRUE)
  test_out %<>%
    bind_rows(data.frame(test=s,
                         Chisq=Xsq$statistic,
                         pval=Xsq$p.value,
                         stringsAsFactors=F))
}
#----------------------------------------------------
aki %<>%
  mutate(label=paste0(enc_cnt,",",prop*100,"%")) %>%
  dplyr::select(stage,site,label) %>%
  spread(site,label) %>%
  inner_join(test_out %>% dplyr::select(-Chisq),
             by=c("stage"="test"))


demo %<>% 
  filter(key!="TOTAL" & stg_bin != "ALL") %>%
  left_join(demo_denom %>% rename(cnt_denom=cnt),
            by=c("site","key","value")) %>%
  spread(stg_bin,cnt) %>%
  mutate(AKI=cnt_denom-NONAKI) %>%
  gather(stg_bin,cnt,-site,-key,-value,-cnt_denom) %>%
  mutate(prop=round(cnt/cnt_denom,3)) %>%
  mutate(label=paste0(cnt,",",prop*100,"%")) %>%
  unite("site_aki_bin",c("site","stg_bin"),sep="_") %>%
  unite("key_val",c("key","value"),sep="_") %>%
  dplyr::select(key_val,site_aki_bin,label) %>%
  spread(site_aki_bin,label,fill="0,0%")



####comparison figures
##==ccs distribution
ccs %<>%
  dplyr::select(key,record_cnt,site) %>%
  mutate(record_cnt=ifelse(record_cnt=="<11",10,record_cnt)) %>%
  mutate(record_cnt=as.numeric(record_cnt)) 

overall<-ccs %>% 
  dplyr::select(key,record_cnt,site) %>%
  group_by(site) %>%
  dplyr::summarize(RECORD_CNT=sum(record_cnt)) %>%
  ungroup

ccs %<>%
  left_join(overall,by="site") %>%
  mutate(record_prop=round(record_cnt/RECORD_CNT,3)) %>%
  dplyr::select(-RECORD_CNT)


ccs %<>%
  mutate(ccs_grp=case_when(key<=10 ~ 1,
                           key>10&key<=30 ~ 2.1,
                           key>30&key<=47 ~ 2.2,
                           key>47&key<=58 ~ 3,
                           key>58&key<=64 ~ 4,
                           (key>=650&key<=663)|key==670 ~ 5,
                           key>=76&key<=95 ~ 6,
                           key>95&key<=108 ~ 7.1,
                           key>108&key<=121 ~ 7.2,
                           key>121&key<=134 ~ 8,
                           key>134&key<=155 ~ 9,
                           key>155&key<=175 ~ 10,
                           key>175&key<=196 ~ 11,
                           key>196&key<=200 ~ 12,
                           key>200&key<=212 ~ 13,
                           key>212&key<=217 ~ 14,
                           key>217&key<=224 ~ 15,
                           key>224&key<=244 ~ 16,
                           key>244&key<=258 ~ 17,
                           key>=259 ~ 18))

ccs %<>%
  mutate(ccs_grp_label=recode(ccs_grp,
                              `1` ="Infectious and parasitic diseases",
                              `2.1` ="Neoplasms (11-30)",
                              `2.2` ="Neoplasms (31-47)",
                              `3` ="Endocrine; nutritional; \nand metabolic ndiseases \nand immunity disorders",
                              `4` ="Diseases of the blood and \nblood-forming organs",
                              `5` ="Mental illness",
                              `6` ="Diseases of the nervous system \nand sense organs",
                              `7.1` ="Diseases of the circulatory system \n(except Hypertension and \nDZs of the heart)",
                              `7.2` ="Diseases of the circulatory system \n(Hypertension and \nDZs of the heart)",
                              `8` ="Diseases of the respiratory system",
                              `9` ="Diseases of the digestive system",
                              `10`="Diseases of the genitourinary system",
                              `11`="Complications of pregnancy; \nchildbirth; and the puerperium",
                              `12`="Diseases of the skin and \nsubcutaneous tissue",
                              `13`="Diseases of the musculoskeletal \nsystem and connective tissue",
                              `14`="Congenital anomalies",
                              `15`="Certain conditions originating \nin the perinatal period",
                              `16`="Injury and poisoning",
                              `17`="symptoms; signs; and \nill-defined conditions and \nfactors influencing health status",
                              `18`="Residual codes; unclassified"))

ccs %<>%
  mutate(ccs_grp_label=reorder(ccs_grp_label,record_cnt,function(x) 1/max(x)))


#complete plot
ggplot(ccs %>% filter(record_prop>0),
       aes(x=as.factor(key),y=record_prop,color=site,fill=site))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7,face="bold"))+
  labs(x="CCS Group",y="Record Proportion",fill="GPC sites",color="GPC sites")+
  facet_wrap(~ccs_grp_label,scales="free")

#selective plot
load("./data/ccs_ref.Rdata")
ccs_sel<-ccs %>% 
  filter(ccs_grp %in% c(2.2,3,7.1)) %>%
  mutate(ccs_grp = recode(ccs_grp,
                          `2.2`="a",
                          `3`="b",
                          `7.1`="c")) %>%
  unite(ccs_grp_label_idx,c("ccs_grp","ccs_grp_label"),sep=".") %>%
  left_join(ccs_ref %>% filter(type=="dx") %>% 
              dplyr::select(-type),
            by=c("key"="ccs_code")) %>%
  mutate(key2=ifelse(key<100,paste0("0",key),as.character(key))) %>%
  unite("ccs_label",c("key2","ccs_name"),sep=".") %>%
  mutate(ccs_label=paste0(substr(ccs_label,1,41),"\n",
                          substr(ccs_label,42,80),"\n",
                          substr(ccs_label,81,nchar(ccs_label))))

ggplot(ccs_sel,aes(x=as.factor(key),y=record_prop,color=site,fill=site))+
  geom_bar(stat="identity",position="dodge")+ ylim(0,0.035)+
  theme(axis.text.x = element_text(angle = 70, hjust = 1,size=8.5,face="bold"))+
  labs(x="CCS Group",y="Record Proportion",fill="GPC sites",color="GPC sites")+
  facet_wrap(~ccs_grp_label_idx,scales="free")

