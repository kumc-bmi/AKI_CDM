#### survival-like prediction model ####

##-------setups-----------
setwd("~/#Projects/AKI/AKI_GPC")

rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "scales",
                    "broom",
                    "purrr",
                    "Matrix",
                    "ggplot2",
                    "ggrepel",
                    "gridExtra",
                    "ggpubr",
                    "VennDiagram"))

params_site<-list(
  KUMC=list(site="KUMC"),
  UTSW=list(site="UTSW"),
  UNMC=list(site="UNMC"),
  MU=list(site="MU"),
  MCW=list(site="MCW"), 
  MCRI=list(site="MCRI")
) 

##task parameters
#-----prediction point
pred_in_d_opt<-c(1,2)

#-----prediction tasks
pred_task_lst<-c("stg1up","stg2up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")

##-----------------------------Internal Validations-----------------------------------------------------------------------
##-------internal validations (KUMC only) --------
#full ROC and PRC curves
int_valid<-readRDS("./data/kumc_perffull_2d.rda") %>% ungroup %>%
  bind_rows(readRDS("./data/kumc_perffull_1d.rda") %>% ungroup) %>%
  dplyr::select(-size)

end_pt<-int_valid %>% 
  select(-cutoff,-meas,-meas_med,-meas_lb,-meas_ub) %>%
  unique %>%
  mutate(cutoff=-Inf,
         rec_sens=1,
         spec=0,
         prec=0) %>%
  gather(meas,meas_med,-pred_in_d,-pred_task,-fs_type,-grp,-cutoff) %>%
  mutate(meas_lb=meas_med,
         meas_ub=meas_med)


int_valid %<>%
  bind_rows(end_pt) %>%
  mutate(grp_type=gsub(":.*","",grp),
         grp_cat=gsub(".*:","",grp)) %>%
  filter(!grp_cat %in% c("[3,Inf]") & !grp_cat %in% c(4,6,7) & 
         !(fs_type=="rm_scr_bun"&grp_type!="Subgrp_VTYPE")) %>%
  mutate(grp_type=recode(grp_type,
                         "Subgrp_AGE"="Age",
                         "Subgrp_DAY"="Daily",
                         "Subgrp_Scr_Base"="Baseline Serum Creatinine",
                         "Subgrp_VTYPE"="Validation Type"),
         grp_cat=recode(grp_cat,
                        `1`="1d",
                        `2`="2d",
                        `3`="3d",
                        `5`="4d or more")) %>%
  mutate(grp_cat=case_when(grp_cat=="int"&fs_type=="no_fs" ~ "Int. w/ all features",
                           grp_cat=="int"&fs_type=="rm_scr_bun" ~ "Int. w/o Scr,BUN",
                           grp_cat=="ext"&fs_type=="no_fs" ~ "Tmp.Ext. w/ all features",
                           grp_cat=="ext"&fs_type=="rm_scr_bun" ~ "Tmp.Ext. w/o Scr,BUN",
                           TRUE ~ grp_cat))


#ROC and PRC summaries
int_valid_summ<-readRDS("./data/kumc_perfsumm_2d.rda") %>% ungroup %>%
  bind_rows(readRDS("./data/kumc_perfsumm_1d.rda") %>% ungroup) %>%
  dplyr::select(-size) %>%
  dplyr::mutate(label=paste0(round(meas_med+0.005,2),
                             " (",round(meas_lb+0.005,2),",",round(meas_ub+0.005,2),")"))

int_valid_summ %<>%
  mutate(grp_type=gsub(":.*","",grp),
         grp_cat=gsub(".*:","",grp)) %>%
  filter(!grp_cat %in% c("[3,Inf]") & !grp_cat %in% c(4,6,7) & 
           !(fs_type=="rm_scr_bun"&grp_type!="Subgrp_VTYPE")) %>%
  mutate(grp_type=recode(grp_type,
                         "Subgrp_AGE"="Age",
                         "Subgrp_DAY"="Daily",
                         "Subgrp_Scr_Base"="Baseline Serum Creatinine",
                         "Subgrp_VTYPE"="Validation Type"),
         grp_cat=recode(grp_cat,
                        `1`="1d",
                        `2`="2d",
                        `3`="3d",
                        `5`="4d or more")) %>%
  mutate(grp_cat=case_when(grp_cat=="int"&fs_type=="no_fs" ~ "Int. w/ all features",
                           grp_cat=="int"&fs_type=="rm_scr_bun" ~ "Int. w/o Scr,BUN",
                           grp_cat=="ext"&fs_type=="no_fs" ~ "Tmp.Ext. w/ all features",
                           grp_cat=="ext"&fs_type=="rm_scr_bun" ~ "Tmp.Ext. w/o Scr,BUN",
                           TRUE ~ grp_cat))


##---overall ROC&PRC curves
grp_type_sel<-c("Validation Type",
                "Daily",
                "Age",
                "Baseline Serum Creatinine")

int_roc<-list()
int_prc<-list()
for(i in seq_along(pred_task_lst)){
  roc_task<-int_valid %>%
    filter(pred_task==pred_task_lst[i] & 
           meas %in% c("spec") & 
           grp_type != "Overall") %>%
    select(-meas_lb,-meas_ub,-meas) %>%
    dplyr::rename("spec"="meas_med") %>%
    left_join(int_valid %>%
                filter(meas %in% c("rec_sens") & pred_task==pred_task_lst[i] & grp_type != "Overall") %>%
                dplyr::rename("sens"="meas_med") %>% select(-meas),
              by=c("pred_in_d","pred_task","fs_type", "grp","cutoff","grp_type","grp_cat")) %>%
    group_by(pred_in_d,pred_task,fs_type,grp,grp_type,grp_cat) %>%
    arrange(desc(spec),desc(cutoff))  %>%
    dplyr::mutate(sens2=cummax(sens),
                  meas_lb2=cummax(meas_lb),
                  meas_ub2=cummax(meas_ub)) %>%
    ungroup
  
  prc_task<-int_valid %>%
    filter(pred_task==pred_task_lst[i] & 
           meas %in% c("rec_sens") & 
           grp_type != "Overall") %>%
    select(-meas_lb,-meas_ub,-meas) %>%
    dplyr::rename("rec"="meas_med") %>%
    left_join(int_valid %>%
                filter(meas %in% c("prec") & pred_task==pred_task_lst[i] & grp_type != "Overall") %>%
                dplyr::rename("prec"="meas_med") %>% select(-meas),
              by=c("pred_in_d","pred_task","fs_type", "grp","cutoff","grp_type","grp_cat")) %>%
    group_by(pred_in_d,pred_task,fs_type,grp,grp_type,grp_cat) %>%
    arrange(desc(rec),desc(cutoff))  %>%
    dplyr::mutate(prec2=cummax(prec),
                  meas_lb2=cummax(meas_lb),
                  meas_ub2=cummax(meas_ub)) %>%
    ungroup
  
  for(d in c(1,2)){
    
    for(g in seq_along(grp_type_sel)){
      
      roc_sub<-roc_task %>% 
        filter(pred_in_d==d&grp_type==grp_type_sel[g])
      
      prc_sub<-prc_task %>% 
        filter(pred_in_d==d&grp_type==grp_type_sel[g])
      
      annot_sub<-int_valid_summ %>%
        filter(pred_task==pred_task_lst[i]&pred_in_d==d&grp_type==grp_type_sel[g])
      
      int_roc[[paste0(i,d,g)]]<-ggplot(roc_sub,aes(x=(1-spec),y=sens2,color=grp_cat))+
        # geom_smooth(method="gam",formula=y~s(x,bs="cs"))+
        geom_rect(aes(xmin=0,xmax=0.1,ymin=0,ymax=Inf),alpha=0.1,fill="grey80",color="grey80")+
        geom_line()+geom_point()+
        geom_text(data=annot_sub %>% filter(overall_meas=="roauc") %>%
                    arrange(grp_type,desc(grp_cat)) %>%
                    mutate(x=0.6,y=row_number()/10) ,
                         aes(x=x,y=y,color=grp_cat,label=label),
                  fontface="bold",hjust = 0)+
        geom_ribbon(aes(ymin=meas_lb2,ymax=meas_ub2,fill=grp_cat),alpha=0.1)+
        geom_abline(slope=1,intercept=0,linetype=3)+
        scale_x_continuous(limits = c(0,1))+
        guides(color=guide_legend(keyheight=ifelse(g==1|(d==1&g==2),1.2,1.5),
                                  direction="vertical"),
               fill=guide_legend(keyheight=ifelse(g==1|(d==1&g==2),1.2,1.5),
                                 direction="vertical"))+
        theme(text=element_text(size=20,face="bold"))+
        labs(x="",y="", color="",fill="") +
        facet_wrap(~grp_type,scales="free",ncol=4)
      
      int_prc[[paste0(i,d,g)]]<-ggplot(prc_sub,aes(x=rec,y=prec2,color=grp_cat))+
        # geom_smooth(method="gam",formula=y~s(x,bs="cs"))+
        geom_line()+geom_point()+
        geom_text(data=annot_sub %>% filter(overall_meas=="prauc1") %>%
                    arrange(grp_type,grp_cat) %>%
                    mutate(x=0.6,y=1-row_number()/10),
                  aes(x=x,y=y,color=grp_cat,label=label),
                  fontface="bold",hjust = 0)+
        geom_ribbon(aes(ymin=meas_lb2,ymax=meas_ub2,fill=grp_cat),alpha=0.1)+
        scale_x_continuous(limits = c(0,1))+
        guides(color=guide_legend(keyheight=ifelse(g==1|(d==1&g==2),1.2,1.5),
                                  direction="vertical"),
               fill=guide_legend(keyheight=ifelse(g==1|(d==1&g==2),1.2,1.5),
                                  direction="vertical"))+
        theme(text=element_text(size=20,face="bold"))+
        labs(x="",y="",color="",fill="") +
        facet_wrap(~grp_type,scales="free",ncol=4)
    }
  }
}

#figure 1 - internal ROC (48 hours, subgroups)
fig1<-ggarrange(ggarrange(int_roc[["121"]],int_roc[["221"]],int_roc[["321"]],ncol=1,nrow=3,legend = "bottom",common.legend =T,
                          labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.2,font.label=list(color="red")),
                ggarrange(int_roc[["122"]],int_roc[["222"]],int_roc[["322"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ggarrange(int_roc[["123"]],int_roc[["223"]],int_roc[["323"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ggarrange(int_roc[["124"]],int_roc[["224"]],int_roc[["324"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ncol=4)

annotate_figure(fig1,
                left=text_grob("Sensitivity",rot=90,face="bold",size=20),
                bottom=text_grob("1-Specificity",face="bold",size=20,vjust=-6))


#ext-figure 1 - internal PRC (48 hours, subgroups)
extfig1<-ggarrange(ggarrange(int_prc[["121"]],int_prc[["221"]],int_prc[["321"]],ncol=1,nrow=3,legend = "bottom",common.legend =T,
                             labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.2,font.label=list(color="red")),
                   ggarrange(int_prc[["122"]],int_prc[["222"]],int_prc[["322"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                   ggarrange(int_prc[["123"]],int_prc[["223"]],int_prc[["323"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                   ggarrange(int_prc[["124"]],int_prc[["224"]],int_prc[["324"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                   ncol=4)

annotate_figure(extfig1,
                left=text_grob("Precision",rot=90,face="bold",size=20),
                bottom=text_grob("Recall (Sensitivity)",face="bold",size=20,vjust=-6))


#supplemental - internal ROC and PRC (24 hours, subgroups)
sfig1<-ggarrange(ggarrange(int_roc[["111"]],int_roc[["211"]],int_roc[["321"]],ncol=1,nrow=3,legend = "bottom",common.legend =T,
                          labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.2,font.label=list(color="red")),
                ggarrange(int_roc[["112"]],int_roc[["212"]],int_roc[["312"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ggarrange(int_roc[["113"]],int_roc[["213"]],int_roc[["313"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ggarrange(int_roc[["114"]],int_roc[["214"]],int_roc[["314"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                ncol=4)

annotate_figure(sfig1,
                left=text_grob("Sensitivity",rot=90,face="bold",size=20),
                bottom=text_grob("1-Specificity",face="bold",size=20,vjust=-6))

sfig2<-ggarrange(ggarrange(int_prc[["111"]],int_prc[["211"]],int_prc[["311"]],ncol=1,nrow=3,legend = "bottom",common.legend =T,
                           labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.2,font.label=list(color="red")),
                 ggarrange(int_prc[["112"]],int_prc[["212"]],int_prc[["312"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                 ggarrange(int_prc[["113"]],int_prc[["213"]],int_prc[["313"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                 ggarrange(int_prc[["114"]],int_prc[["214"]],int_prc[["314"]],ncol=1,nrow=3,legend = "bottom",common.legend =T),
                 ncol=4)

annotate_figure(sfig2,
                left=text_grob("Precision",rot=90,face="bold",size=20),
                bottom=text_grob("Recall (Sensitivity)",face="bold",size=20,vjust=-6))


##-----------------------------External Validations-----------------------------------------------------------------------
##-------AKI proportions and Timing-------
descrp<-readRDS("./data/multi_site.rda")

aki_summ<-descrp$aki_t %>%
  dplyr::select(stage,n,site,
                los_q1,los_median,los_q3,
                time_q1,time_median,time_q3,
                rel_time_q1,rel_time_median,rel_time_q3) %>%
  dplyr::mutate(stage=gsub("_.*","",stage)) %>%
  group_by(site) %>%
  dplyr::mutate(tot=sum(n)) %>%
  ungroup %>%
  dplyr::mutate(prop=round(n/tot,4)) %>%
  dplyr::mutate(label_prop=paste0(prop*100,"%")) %>%
  dplyr::mutate(stage_fac=factor(stage,levels=c("NONAKI","AKI1","AKI2","AKI3"))) %>%
  dplyr::mutate(site_fac=factor(site,levels=c("KUMC","UTSW","UNMC","MU","MCW","MCRI"),labels=paste0("Site_",1:6))) %>%
  mutate(stage_fac=recode(stage_fac,
                          "NONAKI"="Non-AKI",
                          "AKI1"="Any AKI",
                          "AKI2"="At least AKI2"))

# #AKI timings
# ggplot(aki_summ %>% 
#          dplyr::select(site_fac,stage_fac,
#                        los_q1,los_median,los_q3,
#                        time_q1,time_median,time_q3,
#                        rel_time_q1,rel_time_median,rel_time_q3) %>%
#          gather(time_var,time_val,-stage_fac,-site_fac) %>%
#          dplyr::mutate(y_scale=as.numeric(site_fac)+as.numeric(stage_fac)/10) %>%
#          dplyr::mutate(los_type=gsub("_.*","",time_var)) %>%
#          mutate(los_type=recode(los_type,
#                                 "los"="a.Overall LOS",
#                                 "time"="b.Onset Time of Clinical Event\n(AKI or last normal lab)",
#                                 "rel"="c.Relative Time of Clinical Event\n(AKI or last normal lab) to Overall LOS")),
#        aes(x=time_val,y=y_scale,color=stage_fac,shape=site_fac)) +
#   geom_line()+geom_point()+
#   scale_y_continuous(breaks=1:5,labels=paste0("Site_",1:5))+
#   # scale_x_continuous(breaks=1:20,labels=1:20)+
#   scale_shape_discrete(guide = FALSE)+
#   labs(x="# of days since admission",y="GPC sites",color="AKI stages")+
#   facet_wrap(~los_type,scales="free",ncol=3)


##-------Demographic Comparisons-----
demo<-descrp$demo %>%
  dplyr::mutate(value=recode(value,
                             "NI"="UN",
                             "R"="UN",
                             "04"="OT",
                             "06"="OT",
                             "07"="OT"),
                value=case_when(key=="HISPANIC"&value=="OT" ~ "UN",
                                TRUE ~ value)) %>%
  dplyr::filter(stage=="ALL"&!(key=="SEX"&value=="UN")) %>%
  group_by(site,stage,key,value) %>%
  dplyr::summarize(n=sum(n)) %>%
  ungroup %>%
  group_by(site,key) %>%
  dplyr::mutate(tot=sum(n)) %>%
  ungroup %>%
  #add AKI proportions to table 1
  bind_rows(aki_summ %>% 
              dplyr::select(site,stage,n,tot) %>%
              dplyr::mutate(key="AKI_stg",
                            value=stage,
                            stage="ALL")) %>%
  dplyr::mutate(prop=round(n/tot,3)) %>%
  dplyr::mutate(printout=paste0(n," (",100*prop,")")) %>%
  dplyr::select(site,key,value,printout) %>%
  spread(site,printout,fill="0 (0)") %>%
  group_by(key) %>%
  dplyr::mutate(k=length(unique(value))) %>%
  ungroup %>%
  gather(site,site_val,-key,-value,-KUMC,-k) %>%
  dplyr::mutate(n_ref=as.numeric(gsub("\\(.*","",KUMC)),
                p_ref=as.numeric(gsub("\\)","",gsub(".*\\(","",KUMC)))/100,
                n_site=as.numeric(gsub("\\(.*","",site_val)),
                p_site=as.numeric(gsub("\\)","",gsub(".*\\(","",site_val)))/100) %>%
  dplyr::mutate(value_diff=abs(p_ref-p_site),
                test_stats05=sqrt(qchisq(0.95,k-1))*sqrt(p_ref*(1-p_ref)/n_ref+p_site*(1-p_site)/n_site),
                test_stats01=sqrt(qchisq(0.99,k-1))*sqrt(p_ref*(1-p_ref)/n_ref+p_site*(1-p_site)/n_site),
                test_stats001=sqrt(qchisq(0.999,k-1))*sqrt(p_ref*(1-p_ref)/n_ref+p_site*(1-p_site)/n_site)) %>%
  dplyr::mutate(signif=case_when(value_diff>=test_stats001 ~ '***',
                                 value_diff>=test_stats01&value_diff<test_stats001 ~ '**',
                                 value_diff>=test_stats05&value_diff<test_stats01 ~ '*',
                                 TRUE ~ ''))

#tbl1
demo %>%
  unite("printout",c("site_val","signif"),sep="") %>%
  dplyr::select(key,value,site,printout) %>%
  spread(site,printout) %>%
  View


##-------external validation overview-------
ext_valid<-readRDS("./data/extvalid/ext_valid_full.rda")

ext_valid %<>%
  filter(!is.na(meas_med)) %>%
  mutate(valid_retrain=case_when(site==ref_site ~ "Refitted Model",
                                 TRUE ~ "Transported Model")) %>%
  mutate(grp_type=gsub(":.*","",grp),
         grp_cat=gsub(".*:","",grp)) %>%
  filter((ref_site=="KUMC"|valid_retrain=="Refitted Model")&
         !(ref_site=="KUMC"&valid_retrain=="Refitted Model")) %>%
  filter(!grp_cat %in% c("[3,Inf]") & !grp_cat %in% c(4,6,7)) %>%
  dplyr::select(-size) %>%
  mutate(grp_type=recode(grp_type,
                         "Subgrp_AGE"="Age",
                         "Subgrp_DAY"="Daily",
                         "Subgrp_Scr_Base"="Baseline Serum Creatinine"),
         grp_cat=recode(grp_cat,
                        `1`="1d",
                        `2`="2d",
                        `3`="3d",
                        `5`="4d or more"),
         fs_type=recode(fs_type,
                        "no_fs"="w/ all features",
                        "rm_scr_bun"="w/o Scr,Bun")) %>%
  mutate(grp_cat=case_when(grp_type=="Overall" ~ fs_type,
                           TRUE ~ grp_cat))

end_pt<-ext_valid %>%
  select(-cutoff,-meas,-meas_med,-meas_lb,-meas_ub) %>%
  unique %>%
  mutate(cutoff=-Inf,
         rec_sens=1,
         spec=0,
         prec=0) %>%
  gather(meas,meas_med,-pred_in_d,-pred_task,-fs_type,-grp,-cutoff,
         -site,-ref_site,-valid_retrain,-grp_type,-grp_cat) %>%
  mutate(meas_lb=meas_med,
         meas_ub=meas_med)

ext_valid %<>% 
  filter(!cutoff %in% c(Inf,NA)) %>%
  bind_rows(end_pt)


#ROC and PRC summaries
ext_valid_summ<-readRDS("./data/extvalid/ext_valid_summ.rda") %>% ungroup %>%
  dplyr::select(-size) %>%
  dplyr::mutate(label=paste0(round(meas_med+0.005,2),
                             " (",round(meas_lb+0.005,2),",",round(meas_ub+0.005,2),")"))

ext_valid_summ %<>%
  filter(!is.na(meas_med)) %>%
  mutate(valid_retrain=case_when(site==ref_site ~ "Refitted Model",
                                 TRUE ~ "Transported Model")) %>%
  mutate(grp_type=gsub(":.*","",grp),
         grp_cat=gsub(".*:","",grp)) %>%
  filter((ref_site=="KUMC"|valid_retrain=="Refitted Model")&
           !(ref_site=="KUMC"&valid_retrain=="Refitted Model")) %>%
  filter(!grp_cat %in% c("[3,Inf]") & !grp_cat %in% c(4,6,7)) %>%
  mutate(grp_type=recode(grp_type,
                         "Subgrp_AGE"="Age",
                         "Subgrp_DAY"="Daily",
                         "Subgrp_Scr_Base"="Baseline Serum Creatinine"),
         grp_cat=recode(grp_cat,
                        `1`="1d",
                        `2`="2d",
                        `3`="3d",
                        `5`="4d or more"),
         fs_type=recode(fs_type,
                        "no_fs"="w/ all features",
                        "rm_scr_bun"="w/o Scr,Bun")) %>%
  mutate(grp_cat=case_when(grp_type=="Overall" ~ fs_type,
                           TRUE ~ grp_cat))

grp_type_sel<-c("Overall",
                "Daily",
                "Age",
                "Baseline Serum Creatinine")

ext_roc<-list()
ext_prc<-list()
for(i in seq_along(pred_task_lst)){
  roc_task<-ext_valid %>%
    filter(pred_task==pred_task_lst[i] & 
             meas %in% c("spec")) %>%
    select(-meas_lb,-meas_ub,-meas) %>%
    dplyr::rename("spec"="meas_med") %>%
    left_join(ext_valid %>%
                filter(meas %in% c("rec_sens") & pred_task==pred_task_lst[i]) %>%
                dplyr::rename("sens"="meas_med") %>% select(-meas),
              by=c("pred_in_d","pred_task","fs_type", "grp","cutoff","grp_type","grp_cat",
                   "site","ref_site","valid_retrain")) %>%
    group_by(pred_in_d,pred_task,fs_type,grp,grp_type,grp_cat,site,ref_site,valid_retrain) %>%
    arrange(desc(spec),desc(cutoff))  %>%
    dplyr::mutate(sens2=cummax(sens),
                  meas_lb2=cummax(meas_lb),
                  meas_ub2=cummax(meas_ub)) %>%
    ungroup
  
  prc_task<-ext_valid %>%
    filter(pred_task==pred_task_lst[i] & 
             meas %in% c("rec_sens")) %>%
    select(-meas_lb,-meas_ub,-meas) %>%
    dplyr::rename("rec"="meas_med") %>%
    left_join(ext_valid %>%
                filter(meas %in% c("prec") & pred_task==pred_task_lst[i]) %>%
                dplyr::rename("prec"="meas_med") %>% select(-meas),
              by=c("pred_in_d","pred_task","fs_type", "grp","cutoff","grp_type","grp_cat",
                   "site","ref_site","valid_retrain")) %>%
    group_by(pred_in_d,pred_task,fs_type,grp,grp_type,grp_cat) %>%
    arrange(desc(rec),desc(cutoff))  %>%
    dplyr::mutate(prec2=cummax(prec),
                  meas_lb2=cummax(meas_lb),
                  meas_ub2=cummax(meas_ub)) %>%
    ungroup
  
  for(d in c(1,2)){
    
    for(s in seq_along(params_site)[-1]){
      
      for(g in seq_along(grp_type_sel)){
        if(g==1){
          roc_sub<-roc_task %>% 
            filter(pred_in_d==d&site==params_site[[s]]$site&grp_type==grp_type_sel[g])
          
          prc_sub<-prc_task %>% 
            filter(pred_in_d==d&site==params_site[[s]]$site&grp_type==grp_type_sel[g])
        }else{
          roc_sub<-roc_task %>% 
            filter(pred_in_d==d&site==params_site[[s]]$site&grp_type==grp_type_sel[g]&fs_type=="w/ all features")
          
          prc_sub<-prc_task %>% 
            filter(pred_in_d==d&site==params_site[[s]]$site&grp_type==grp_type_sel[g]&fs_type=="w/ all features")
        }
        
        annot_sub<-ext_valid_summ %>%
          filter(pred_task==pred_task_lst[i]&pred_in_d==d&site==params_site[[s]]$site&grp_type==grp_type_sel[g])
        
        ext_roc[[paste0(i,d,s,g)]]<-ggplot(roc_sub,aes(x=(1-spec),y=sens2,
                                                       color=valid_retrain))+
          # geom_smooth(method="gam",formula=y~s(x,bs="cs"))+
          geom_rect(aes(xmin=0,xmax=0.1,ymin=0,ymax=Inf),alpha=0.1,fill="grey80",color="grey80")+
          geom_line(aes(linetype=grp_cat),size=1)+
          geom_point(aes(shape=grp_cat),size=2.5)+
          geom_ribbon(aes(ymin=meas_lb2,ymax=meas_ub2,fill=valid_retrain,linetype=grp_cat),
                      alpha=0.1,size=1)+
          geom_abline(slope=1,intercept=0,linetype=3)+
          geom_text(data=annot_sub %>% filter(overall_meas=="roauc") %>%
                      arrange(valid_retrain,desc(grp_cat)) %>%
                      mutate(x=0.5,y=row_number()/10),
                    aes(x=x,y=y,color=valid_retrain,label=label),
                    fontface="bold",hjust = 0)+
          geom_point(data=annot_sub %>% filter(overall_meas=="roauc") %>%
                       arrange(valid_retrain,desc(grp_cat)) %>%
                       mutate(x=0.4,y=row_number()/10),
                     aes(x=x,y=y,color=valid_retrain,shape=grp_cat),size=3)+
          scale_x_continuous(limits = c(0,1))+
          guides(color=guide_legend(direction="horizontal"),
                 fill=guide_legend(direction="horizontal"),
                 shape=guide_legend(direction="horizontal"),
                 linetype=guide_legend(direction="horizontal"))+
          theme(text=element_text(size=15,face="bold"),
                plot.subtitle = element_text(hjust = 0.5))+
          labs(x="",y="", color="",fill="",shape="",linetype="",
               subtitle=ifelse(i==1,paste0("Site ",s),""))
        
        
        ext_prc[[paste0(i,d,s,g)]]<-ggplot(prc_sub,aes(x=rec,y=prec2,
                                                       color=valid_retrain,
                                                       linetype=grp_cat,
                                                       shape=grp_cat))+
          # geom_smooth(method="gam",formula=y~s(x,bs="cs"))+
          geom_line(aes(linetype=grp_cat),size=1)+
          geom_point(aes(shape=grp_cat),size=2.5)+
          geom_ribbon(aes(ymin=meas_lb2,ymax=meas_ub2,fill=grp_cat),
                      alpha=0.1,size=1)+
          geom_text(data=annot_sub %>% filter(overall_meas=="prauc1") %>%
                      arrange(valid_retrain,grp_cat) %>%
                      mutate(x=0.5,y=1-row_number()/10),
                    aes(x=x,y=y,color=valid_retrain,label=label),
                    fontface="bold",hjust = 0)+
          geom_point(data=annot_sub %>% filter(overall_meas=="prauc1") %>%
                       arrange(valid_retrain,grp_cat) %>%
                       mutate(x=0.4,y=1-row_number()/10),
                     aes(x=x,y=y,color=valid_retrain,shape=grp_cat),size=3)+
          scale_x_continuous(limits = c(0,1))+
          guides(color=guide_legend(direction="horizontal"),
                 fill=guide_legend(direction="horizontal"),
                 shape=guide_legend(direction="horizontal"),
                 linetype=guide_legend(direction="horizontal"))+
          theme(text=element_text(size=15,face="bold"),
                plot.subtitle = element_text(hjust = 0.5))+
          labs(x="",y="", color="",fill="",shape="",linetype="",
               subtitle=ifelse(i==1,paste0("Site ",s),""))
      }
      
    }
  }
}

#figure 2 - external ROC (48 hours,overall)
ext_roc[["1261"]]#generate legend samples
ext_roc[["1261"]] +
  guides(color=guide_legend(direction="vertical"),
         fill=guide_legend(direction="vertical"),
         shape=guide_legend(direction="vertical"),
         linetype=guide_legend(direction="vertical"))


fig2<-ggarrange(ggarrange(ext_roc[["1221"]],ext_roc[["2221"]],ext_roc[["3221"]],ncol=1,nrow=3,legend = "none",common.legend =T,
                          labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                ggarrange(ext_roc[["1231"]],ext_roc[["2231"]],ext_roc[["3231"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1241"]],ext_roc[["2241"]],ext_roc[["3241"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1251"]],ext_roc[["2251"]],ext_roc[["3251"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1261"]],ext_roc[["2261"]],ext_roc[["3261"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ncol=5)

annotate_figure(fig2,
                left=text_grob("Sensitivity",rot=90,face="bold",size=20),
                bottom=text_grob("1-Specificity",face="bold",size=20))


#ext-figure 2 - external PRC (48 hours, overall)
extfig2<-ggarrange(ggarrange(ext_prc[["1221"]],ext_prc[["2221"]],ext_prc[["3221"]],ncol=1,nrow=3,legend = "none",common.legend =T,
                             labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                   ggarrange(ext_prc[["1231"]],ext_prc[["2231"]],ext_prc[["3231"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1241"]],ext_prc[["2241"]],ext_prc[["3241"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1251"]],ext_prc[["2251"]],ext_prc[["3251"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1261"]],ext_prc[["2261"]],ext_prc[["3261"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ncol=5)

annotate_figure(extfig2,
                left=text_grob("Precision",rot=90,face="bold",size=20),
                bottom=text_grob("Recall (Sensitivity)",face="bold",size=20))


#supplemental figure 2 - external ROC (24 hours,overall)
sfig2<-ggarrange(ggarrange(ext_roc[["1121"]],ext_roc[["2121"]],ext_roc[["3121"]],ncol=1,nrow=3,legend = "none",common.legend =T,
                          labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                ggarrange(ext_roc[["1131"]],ext_roc[["2131"]],ext_roc[["3131"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1141"]],ext_roc[["2141"]],ext_roc[["3141"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1151"]],ext_roc[["2151"]],ext_roc[["3151"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ggarrange(ext_roc[["1161"]],ext_roc[["2161"]],ext_roc[["3161"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                ncol=5)

annotate_figure(sfig2,
                left=text_grob("Sensitivity",rot=90,face="bold",size=20),
                bottom=text_grob("1-Specificity",face="bold",size=20))


#ext-figure 2 - external PRC (24 hours, overall)
sfig2b<-ggarrange(ggarrange(ext_prc[["1121"]],ext_prc[["2121"]],ext_prc[["3121"]],ncol=1,nrow=3,legend = "none",common.legend =T,
                             labels=c("A.Any AKI","B.AKI >= 2","C.AKI 3"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                   ggarrange(ext_prc[["1131"]],ext_prc[["2131"]],ext_prc[["3131"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1141"]],ext_prc[["2141"]],ext_prc[["3141"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1151"]],ext_prc[["2151"]],ext_prc[["3151"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ggarrange(ext_prc[["1161"]],ext_prc[["2161"]],ext_prc[["3161"]],ncol=1,nrow=3,legend = "none",common.legend =T),
                   ncol=5)

annotate_figure(sfig2b,
                left=text_grob("Precision",rot=90,face="bold",size=20),
                bottom=text_grob("Recall (Sensitivity)",face="bold",size=20))


# ext_roc[["1262"]] #generate legend samples
# ext_roc[["1263"]] #generate legend samples
# ext_roc[["1264"]] #generate legend samples


##-------important variables-------------
data_dict<-readRDS("./data/metadata.rda") %>%
  filter(FIELD_NAME %in% c("LOINC","RXNORM_CUI","NDC","PX","RACE","CCS","DEMO")) %>%
  group_by(TABLE_NAME, FIELD_NAME,VALUESET_ITEM,VALUESET_ITEM2) %>%
  dplyr::filter(VALUESET_ITEM!="NDC:00206886201") %>%
  dplyr::slice(1:1) %>% ungroup %>%
  bind_rows(read.csv("./data/metadata_add.csv",stringsAsFactors = F))

incldk<-10000
topk<-100
varimp<-readRDS("./data/varimp_site.rda") %>%
  filter(rank<=incldk) %>%
  mutate(feature_label=case_when(grepl("_cum",Feature) ~ gsub(":.*","",Feature),
                                 grepl("ND:",Feature) ~ gsub("ND:","NDC:",Feature),
                                 grepl("_change",Feature) ~ gsub("_.*","",Feature),
                                 grepl("09:",Feature) ~ gsub("09:","ICD9:",Feature),
                                 grepl("10:",Feature) ~ gsub("10:","ICD10:",Feature),
                                 TRUE ~ Feature),
         feature_modifier=case_when(grepl("_cum",Feature) ~ "cumulative",
                                    grepl("_change",Feature) ~ "change",
                                    TRUE ~ NA_character_)) %>%
  left_join(data_dict %>% select(VALUESET_ITEM2,VALUESET_ITEM_DESCRIPTOR),
            by=c("feature_label"="VALUESET_ITEM2")) %>%
  mutate(feature_descr=case_when(is.na(VALUESET_ITEM_DESCRIPTOR)~Feature,
                                 TRUE ~ VALUESET_ITEM_DESCRIPTOR)) %>%
  mutate(soft_rank=exp(-rank/topk))

label_n<-5
varimp_com<-varimp %>%
  group_by(pred_task,pred_in_d,fs_type,feature_descr,Feature,feature_modifier) %>%
  dplyr::summarise(common_site=round(length(unique(site))/6,2),
                   soft_rank_med=median(soft_rank,na.rm=T),
                   soft_rank_iqr=IQR(soft_rank,na.rm=T)) %>%
  ungroup %>%
  group_by(pred_task,pred_in_d,fs_type,common_site) %>%
  dplyr::mutate(soft_rank_med_rk=rank(-soft_rank_med)) %>%
  ungroup %>%
  mutate(label=case_when(soft_rank_med_rk<=label_n ~ paste0(substr(feature_descr,1,44),"\n",
                                                            substr(feature_descr,45,nchar(feature_descr))),
                         TRUE ~ NA_character_)) %>%
  mutate(label=case_when(feature_modifier=="change"&!is.na(label) ~ paste0(substr(feature_descr,1,44),"\n",
                                                                           substr(feature_descr,45,nchar(feature_descr)),"_",feature_modifier),
                         TRUE ~ label)) %>%
  mutate(fs_type=recode(fs_type,
                        "no_fs"="a.w/ all features",
                        "rm_scr_bun"="b. w/o Scr, BUN"))

#variable comparisons
ggplot(varimp_com %>% filter(pred_task == "stg2up"&pred_in_d==2),
       aes(x=soft_rank_med,y=common_site,color=soft_rank_iqr))+
  geom_point(size=4,alpha=0.5)+
  geom_point(aes(x=1.2,y=0.1),alpha=0)+
  geom_label_repel(aes(label=label),
                   label.size = NA, 
                   fontface = 'bold',
                   alpha = 0.6, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   seed = 1234) +
  geom_label_repel(aes(label=label),
                   label.size = NA, 
                   fontface = 'bold',
                   alpha = 1, 
                   label.padding=.1, 
                   na.rm=TRUE,
                   fill = NA,
                   seed = 1234) +
  scale_y_continuous(breaks=c(0.17,0.34,0.5,0.67,0.83,1),limits = c(0.1,1.05))+
  scale_color_gradient(low="blue", high="red")+
  theme(text=element_text(size=15,face="bold"))+
  labs(x="Importance Ranking (Median of Soft Ranking)",
       y="Commonality Across Sites",
       color="Variation of Ranking\n(IQR of Soft Ranking)")+
  facet_wrap(~fs_type,scales="free",ncol=2)


#--------SHAP interpolation for common variables--------
var_nm<-c("2160-0",
          "2160-0_change",
          "48642-3",
          "48642-3_change",
          "375983:01_cum",
          "AGE",
          "BMI",
          "158",
          "718-7",
          "718-7_change",
          "CH:71010",
          "3094-0",
          "BUN_SCR",
          "2075-0",
          "17861-6",
          "2823-3",
          "2777-1",
          "2075-0",
          "777-3",
          "1920-8",
          "2345-7",
          "788-0",
          "1751-7",
          "6690-2",
          "BP_DIASTOLIC_min",
          "BP_SYSTOLIC_min",
          "BP_DIASTOLIC_slope",
          "BP_SYSTOLIC_slope")

var_in<-varimp_com %>% 
  filter(!is.na(label)&common_site>=0.8) %>%
  filter(Feature %in% var_nm) %>%
  mutate(val_lb=case_when(Feature=="AGE" ~ 18,
                          Feature=="BMI" ~ 10,
                          Feature=="2160-0" ~ 0.001,
                          Feature=="2160-0_change" ~ -3,
                          Feature=="718-7" ~ 10,
                          Feature=="718-7_change" ~ -5),
         val_ub=case_when(Feature=="AGE" ~ 89,
                          Feature=="BMI" ~ 60,
                          Feature=="2160-0" ~ 5,
                          Feature=="2160-0_change" ~ 3,
                          Feature=="718-7" ~ 20,
                          Feature=="718-7_change" ~ 5))

shap_plot<-c()
for(d in c(1,2)){
  
  for(tsk in seq_along(pred_task_lst[1:2])){
    
    for(fs in fs_type_opt){
      
      shap_top<-c()
      for(i in 1:length(params_site)){
        
        params<-params_site[[i]]
        shap<-readRDS(paste0("./data/model_explain/",params$site,"/",d,"d_",fs,"_",pred_task_lst[tsk],".rda")) %>%
          inner_join(var_in %>% select(Feature,label,val_lb,val_ub), by=c("var"="Feature")) %>%
          filter((val > val_lb & val <= val_ub)|is.na(val_lb)) %>%
          mutate(val=round(val,1)) %>%
          group_by(var,label,val) %>%
          dplyr::summarize(eff_med=mean(effect),
                           eff_lb=quantile(effect,0.025),
                           eff_ub=quantile(effect,0.975)) %>%
          ungroup %>% mutate(site=params$site)
        
        shap_top %<>% bind_rows(shap)
      }
      
      var_lst<-unique(shap_top$var)
      for(v in seq_along(var_lst)){
        shap_top_v<-shap_top %>% filter(var==var_lst[v]) %>%
          group_by(site) %>%
          dplyr::mutate(eff_m=median(eff_med)) %>%
          ungroup %>%
          mutate(eff_med2=eff_med-eff_m,
                 eff_lb2=eff_lb-eff_m,
                 eff_ub2=eff_ub-eff_m) %>%
          mutate(site_fac=factor(site,levels=c("KUMC","UTSW","UNMC","MU","MCW","MCRI"),
                                 labels=paste0("Site_",1:6)))
        
        if(length(unique(shap_top_v$val))==2){
          shap_plot[[paste0(d,tsk,"_",fs,"_",var_lst[v])]]<-ggplot(shap_top_v,
                                                         aes(x=val,y=eff_med2,color=site_fac))+
            geom_line()+geom_point()+
            geom_errorbar(aes(ymin=eff_lb2,ymax=eff_ub2),width=0.1)+
            labs(x="Value",y="log(odds ratio)",
                 color="Validation Site",fill="Validation Site")+
            geom_hline(yintercept = 0,linetype=2) +
            theme(text=element_text(size=15,face="bold")) +
            facet_wrap(~label,scales="free",ncol=1)
        }else{
          shap_plot[[paste0(d,tsk,"_",fs,"_",var_lst[v])]]<-ggplot(shap_top_v,
                                                         aes(x=val,y=eff_med2,color=site_fac))+
            geom_smooth(method = 'gam',formula=y ~ s(x, bs = "cs"))+
            geom_point()+
            geom_errorbar(aes(ymin=eff_lb2,ymax=eff_ub2))+
            labs(x="Value",y="log(odds ratio)",
                 color="Validation Site",fill="Validation Site")+
            geom_hline(yintercept = 0,linetype=2) + 
            theme(text=element_text(size=15,face="bold")) +
            facet_wrap(~label,scales="free",ncol=1)
        }
      }
    }
  }
}



#figure 4 - SHAP marginal effects (48 hours,AKI>=2)
fig4<-ggarrange(ggarrange(shap_plot[["22_no_fs_AGE"]],shap_plot[["22_rm_scr_bun_AGE"]],ncol=1,nrow=2,legend = "none",common.legend =T,
                          labels=c("A.w/ all features","B.w/o Scr,BUN"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                ggarrange(shap_plot[["22_no_fs_BMI"]],shap_plot[["22_rm_scr_bun_BMI"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ggarrange(shap_plot[["22_no_fs_2160-0"]],shap_plot[["22_rm_scr_bun_718-7"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ggarrange(shap_plot[["22_no_fs_2160-0_change"]],shap_plot[["22_rm_scr_bun_CH:71010"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ncol=4)

fig4


#sanity checks - SHAP marginal effects of AGE (48 hours,AKI>=1)
ggarrange(shap_plot[["21_no_fs_AGE"]],shap_plot[["21_rm_scr_bun_AGE"]],ncol=1,nrow=2,legend = "none",common.legend =T,
          labels=c("A.w/ all features","B.w/o Scr,BUN"),hjust=-0.1,vjust = 1.5,font.label=list(color="red"))



#extended figure 4 - SHAP marginal effects (48 hours,AKI>=2)
extfig8<-ggarrange(ggarrange(shap_plot[["21_no_fs_AGE"]],shap_plot[["21_rm_scr_bun_AGE"]],ncol=1,nrow=2,legend = "none",common.legend =T,
                          labels=c("A.w/ all features","B.w/o Scr,BUN"),hjust=-0.1,vjust = 1.5,font.label=list(color="red")),
                ggarrange(shap_plot[["21_no_fs_BMI"]],shap_plot[["21_rm_scr_bun_BMI"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ggarrange(shap_plot[["21_no_fs_2160-0"]],shap_plot[["21_rm_scr_bun_718-7"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ggarrange(shap_plot[["21_no_fs_2160-0_change"]],shap_plot[["21_rm_scr_bun_158"]],ncol=1,nrow=2,legend = "none",common.legend =T),
                ncol=4)

extfig8


##-------explain adjMMD-----------
#----case I----
pred_in_d_chk<-2
pred_task_chk<-"stg2up"
fs_type_chk<-"no_fs"

v_out<-readRDS(paste0("./data/mmd/",pred_in_d_chk,"d_",fs_type_chk,"_",pred_task_chk,".rda"))

v_out_mmd<-v_out$v_dist_joint %>% 
  dplyr::select(-kl_div_joint) %>%
  mutate(site_fac=factor(site,levels=c("KUMC","UTSW","UNMC","MU","MCW","MCRI"),labels=paste0("Site_",1:6)))

p1<-ggplot(v_out_mmd %>%
             gather(meas,val,-site,-site_fac,-v_incld),
           aes(x=v_incld,y=val,color=site_fac))+
  geom_line()+geom_point()+
  annotate("segment",x=25,y=0.13,xend=2,yend=0.41,linetype=2)+
  annotate("text",x=25,y=0.1,label='bold("Jump due to\ncomplete missing")',parse=T)+
  annotate("segment",x=75,y=0.23,xend=10,yend=0.3,linetype=2)+
  annotate("text",x=75,y=0.2,label='bold("Recover with better\ndistribution matching")',parse=T)+
  annotate("segment",x=80,y=0.5,xend=80,yend=0.57,linetype=2)+
  annotate("segment",x=80,y=0.5,xend=80,yend=0.42,linetype=2)+
  annotate("text",x=80,y=0.5,label='bold("Convergence")',parse=T)+
  guides(color=guide_legend(nrow=1,position="bottom"))+
  theme(text=element_text(size=13,face="bold"),legend.position = "bottom")+
  labs(x="# of Variables",y="adjMMD",color="Validation Site")


valid<-readRDS("./data/extvalid/ext_valid_summ.rda") %>%
  dplyr::filter(pred_task==pred_task_chk&pred_in_d==pred_in_d_chk&fs_type==fs_type_chk
                &grp=="Overall"&ref_site=="KUMC"&overall_meas=="roauc") %>%
  dplyr::mutate(ref_AUC=max(meas_med)) %>% ungroup %>%
  dplyr::mutate(ROAUC_diff=abs(meas_med-ref_AUC))


joint_dist<-v_out_mmd %>%
  gather(meas,val,-site,-site_fac,-v_incld) %>%
  left_join(valid %>% 
              dplyr::select(-size,-grp,-pred_task,-pred_in_d,-fs_type,-ref_site),
            by="site")

rsq<-joint_dist %>% 
  dplyr::select(v_incld,ROAUC_diff,meas,val) %>%
  nest(-meas,-v_incld) %>%
  mutate(test=map(data, ~cor.test(.x$ROAUC_diff,.x$val)),
         tidied=map(test,tidy)) %>%
  unnest(tidied, .drop=T) %>%
  dplyr::select(v_incld,meas,estimate) %>% 
  dplyr::rename(pearson=estimate) %>%
  mutate(pearson2=scales::rescale(pearson,to=c(0.15,0.95)))


rsq2<-joint_dist %>%
  nest(-meas,-v_incld) %>%
  mutate(test=map(data,~lm(ROAUC_diff~val,data=.)),
         tidied=map(test,tidy)) %>%
  unnest(tidied,.drop=T) %>%
  mutate(term=recode(term,
                     "(Intercept)"="a",
                     "val"="b"))

ref_roc<-round(unique(valid$ref_AUC),2)
p2_annot<-rsq2 %>%
  dplyr::select(v_incld,term,estimate) %>%
  spread(term,estimate) %>%
  group_by(v_incld) %>%
  mutate(eq=as.character(as.expression(substitute(italic(y) == bold(a) + bold(b) %.% italic(x),
                                                  list(a=round(a,3),
                                                       b=round(b,3)))))) %>%
  ungroup


mark_pt<-c(1,2,4,13,50,100)
p2<-ggplot(joint_dist %>% 
             dplyr::filter(v_incld %in% mark_pt) %>%
             mutate(panel_title=paste0("# of Variables:",v_incld)),
           aes(x=val,y=ROAUC_diff,label=site_fac))+
  geom_point(aes(color=site_fac),size=5)+
  geom_label_repel(aes(color=site_fac),size=5)+
  geom_label(data=rsq %>% dplyr::filter(v_incld %in% mark_pt),
             aes(x=c(0.03,0.15,0.15,0.15,0.15,0.15),
                 y=c(0.4,0.3,0.3,0.4,0.4,0.4),
                 color=site_fac,
                 label=paste0("Pearson Corr:",round(pearson2,2))),
             color="red",size=5)+
  geom_label(data=p2_annot %>% dplyr::filter(v_incld %in% mark_pt),
             aes(x=c(0.03,0.15,0.15,0.15,0.15,0.15),
                 y=c(0.25,0.2,0.2,0.25,0.25,0.25),
                 color=site_fac,
                 label=eq),
             parse=T,color="blue",size=5)+
  geom_smooth(method="lm")+
  theme(text=element_text(size=15,face="bold"))+
  labs(x="adjMMD",y="ROAUC drop")+
  facet_wrap(~panel_title,scales="free",ncol=3)


annot<-rsq %>%
  dplyr::filter(pearson2==max(pearson2)|v_incld %in% mark_pt)


p3<-ggplot(rsq, aes(x=v_incld,y=pearson2))+
  geom_line(size=1.5)+geom_point(size=3)+
  geom_point(data=annot,color="red",size=8)+
  geom_label(data=annot,
             aes(y=pearson2-0.03,
                 x=v_incld+1.5,
                 label=paste(v_incld,",",round(pearson2,2))),
             size=5)+
  theme(text=element_text(size=15,face="bold"))+
  labs(x="# of Variables",y="Correlation Coefficients")


#print KUMC top variables
rank_bd<-rsq$v_incld[which.max(rsq$pearson2)]
p4<-ggplot(varimp %>%
             filter(pred_task=="stg2up"&pred_in_d==2&fs_type=="no_fs"&rank<=rank_bd&site=="KUMC") %>%
             mutate(Gain_rescale=round(Gain/Gain[1]*100),
                    feature_rank=paste0(str_pad(rank,2,"left","0"),".",feature_descr,
                                        ifelse(feature_modifier=="change",paste0("_",feature_modifier),""))) %>%
             mutate(feature_rank=paste0(substr(feature_rank,1,44),"\n",substr(feature_rank,45,nchar(feature_rank))),
                    feature_rank_fac=as.factor(feature_rank)) %>%
             mutate(feature_rank_fac=factor(feature_rank_fac,levels=rev(levels(feature_rank_fac)))),
           aes(x=feature_rank_fac,y=Gain_rescale))+
  geom_bar(stat="identity")+
  labs(x="Features",y="Normalized Scale")+
  scale_fill_grey()+
  guides(fill = FALSE) +
  theme(text=element_text(size=17,face="bold",color="black"))+
  coord_flip()+
  scale_y_continuous(trans = "reverse")


#----case II----
pred_in_d_chk<-2
pred_task_chk<-"stg2up"
fs_type_chk<-"rm_scr_bun"

v_out<-readRDS(paste0("./data/mmd/",pred_in_d_chk,"d_",fs_type_chk,"_",pred_task_chk,".rda"))

v_out_mmd<-v_out$v_dist_joint %>% 
  dplyr::select(-kl_div_joint) %>%
  mutate(site_fac=factor(site,levels=c("KUMC","UTSW","UNMC","MU","MCW","MCRI"),labels=paste0("Site_",1:6)))

p1<-ggplot(v_out_mmd %>%
             gather(meas,val,-site,-site_fac,-v_incld),
           aes(x=v_incld,y=val,color=site_fac))+
  geom_line()+geom_point()+
  annotate("segment",x=10,y=0.13,xend=1,yend=0.7,linetype=2)+
  annotate("text",x=10,y=0.1,label='bold("Jump due to\ncomplete missing")',parse=T)+
  annotate("segment",x=35,y=0.23,xend=3,yend=0.5,linetype=2)+
  annotate("text",x=35,y=0.2,label='bold("Recover with better\ndistribution matching")',parse=T)+
  annotate("segment",x=40,y=0.5,xend=40,yend=0.6,linetype=2)+
  annotate("segment",x=40,y=0.5,xend=40,yend=0.48,linetype=2)+
  annotate("text",x=40,y=0.5,label='bold("Convergence")',parse=T)+
  guides(color=guide_legend(nrow=1,position="bottom"))+
  theme(text=element_text(size=15,face="bold"),legend.position = "bottom")+
  labs(x="# of Variables",y="adjMMD",color="Validation Site")


valid<-readRDS("./data/ext_valid_summ.rda") %>%
  dplyr::filter(pred_task==pred_task_chk&pred_in_d==pred_in_d_chk&fs_type==fs_type_chk
                &grp=="Overall"&ref_site=="KUMC"&overall_meas=="roauc") %>%
  dplyr::mutate(ref_AUC=max(meas_med)) %>% ungroup %>%
  dplyr::mutate(ROAUC_diff=abs(meas_med-ref_AUC))


joint_dist<-v_out_mmd %>%
  gather(meas,val,-site,-site_fac,-v_incld) %>%
  left_join(valid %>% 
              dplyr::select(-size,-grp,-pred_task,-pred_in_d,-fs_type,-ref_site),
            by="site")

rsq<-joint_dist %>% 
  dplyr::select(v_incld,ROAUC_diff,meas,val) %>%
  nest(-meas,-v_incld) %>%
  mutate(test=map(data, ~cor.test(.x$ROAUC_diff,.x$val)),
         tidied=map(test,tidy)) %>%
  unnest(tidied, .drop=T) %>%
  dplyr::select(v_incld,meas,estimate) %>% 
  dplyr::rename(pearson=estimate) %>%
  mutate(pearson2=scales::rescale(pearson,to=c(0.15,0.85)))


rsq2<-joint_dist %>%
  nest(-meas,-v_incld) %>%
  mutate(test=map(data,~lm(ROAUC_diff~val,data=.)),
         tidied=map(test,tidy)) %>%
  unnest(tidied,.drop=T) %>%
  mutate(term=recode(term,
                     "(Intercept)"="a",
                     "val"="b"))

ref_roc<-round(unique(valid$ref_AUC),2)
p2_annot<-rsq2 %>%
  dplyr::select(v_incld,term,estimate) %>%
  spread(term,estimate) %>%
  group_by(v_incld) %>%
  mutate(eq=as.character(as.expression(substitute(italic(y) == bold(a) + bold(b) %.% italic(x),
                                                  list(a=round(a,3),
                                                       b=round(b,3)))))) %>%
  ungroup


mark_pt<-c(1,3,7,12,33,50)
p2<-ggplot(joint_dist %>% 
             dplyr::filter(v_incld %in% mark_pt),
           aes(x=val,y=ROAUC_diff,label=site_fac))+
  geom_point(aes(color=site_fac),size=5)+
  geom_label_repel(aes(color=site_fac),size=5)+
  geom_label(data=rsq %>% dplyr::filter(v_incld %in% mark_pt),
             aes(x=c(0.2,0.2,0.2,0.2,0.2,0.2),
                 y=c(0.4,0.3,0.3,0.4,0.4,0.4),
                 color=site_fac,
                 label=paste0("Pearson Corr:",round(pearson2,2))),
             color="red",size=5)+
  geom_label(data=p2_annot %>% dplyr::filter(v_incld %in% mark_pt),
             aes(x=c(0.3,0.3,0.3,0.3,0.3,0.3),
                 y=c(-0.15,-0.15,-0.15,-0.15,-0.15,-0.15),
                 color=site_fac,
                 label=eq),
             parse=T,color="blue",size=5)+
  geom_smooth(method="lm")+
  theme(text=element_text(size=15,face="bold"))+
  labs(x="adjMMD",y="ROAUC drop")+
  facet_wrap(~v_incld,scales="free",ncol=3)


annot<-rsq %>%
  dplyr::filter(pearson2==max(pearson2)|v_incld %in% mark_pt)


p3<-ggplot(rsq, aes(x=v_incld,y=pearson2))+
  geom_line(size=1.5)+geom_point(size=3)+
  geom_point(data=annot,color="red",size=8)+
  geom_label(data=annot,
             aes(y=pearson2-0.03,
                 x=v_incld+1.5,
                 label=paste(v_incld,",",round(pearson2,2))),
             size=5)+
  theme(text=element_text(size=15,face="bold"))+
  labs(x="# of Variables",y="Correlation Coefficients")


#print KUMC top variables
rank_bd<-min(15,rsq$v_incld[which.max(rsq$pearson2)])
p4<-ggplot(varimp %>%
             filter(pred_task==pred_task_chk&pred_in_d==pred_in_d_chk&fs_type==fs_type_chk&rank<=rank_bd&site=="KUMC") %>%
             mutate(Gain_rescale=round(Gain/Gain[1]*100),
                    feature_rank=paste0(str_pad(rank,2,"left","0"),".",feature_descr)) %>%
             mutate(feature_rank=paste0(substr(feature_rank,1,44),"\n",substr(feature_rank,45,nchar(feature_rank))),
                    feature_rank_fac=as.factor(feature_rank)) %>%
             mutate(feature_rank_fac=factor(feature_rank_fac,levels=rev(levels(feature_rank_fac)))),
           aes(x=feature_rank_fac,y=Gain_rescale))+
  geom_bar(stat="identity")+
  labs(x="Features",y="Normalized Scale")+
  scale_fill_grey()+
  guides(fill = FALSE) +
  theme(text=element_text(size=17,face="bold",color="black"))+
  coord_flip()+
  scale_y_continuous(trans = "reverse")






#----Venn Diagram for shared features
venn_diag<-readRDS("./data/venn_diag_shared_feat.rda")

for(i in c(1,2)){
  
  for(d in pred_in_d_opt){
    
    for(f in seq_along(fs_type_opt)){
      
      for(s in 2:6){
        
        for(k in c(100,500)){
          vd_s<-venn_diag %>%
            filter(pred_task==pred_task_lst[i]&
                     pred_in_d==pred_in_d_opt[i]&
                     fs_type==fs_type_opt[f]&
                     site==params_site[[s]]$site&
                     topk==k)
          
          shared<-vd_s %>% filter(varimp_ind=="shared") %>% 
            dplyr::select(var_cnt) %>% unlist
          
          venn.diagram(
            list(Source = 1:k,
                 Target = (k-shared):(2*k-shared-1)),
            main = case_when(vd_s$site_grp==1 ~ "Low Similarity (High adjMMD)",
                            vd_s$site_grp==2 ~ "Medium Similarity (Medium adjMMD)",
                            vd_s$site_grp==3 ~ "High Similarity (Low adjMMD)"),
            main.fontface = 4,
            main.cex = 2,
            sub = paste0("Top k = ",k),
            sub.fontface = 4,
            sub.cex = 2,
            fill = c("red", "green"),
            alpha = c(0.5, 0.5),
            cex = 2,
            cat.fontface = 4,
            lty =2, 
            cat.cex = 1.5,
            filename = paste0("./fig&tbl/venn_diagram/stg",i,"d",d,"fs",f,"/",s,"_",k,".png")
          )
        }
      }
      
    }
  }
}


#---- Subgroup analysis--------
subgrp<-readRDS("./data/subgrp_profile.rda")

#age profile
age_grp<-subgrp$age_profile %>%
  dplyr::select(-age_mean,-age_sd,-age_med,-age_q1,-age_q3) %>%
  gather(var,val,-stg,-n,-site) %>%
  mutate(var=as.numeric(var),
         stg=as.factor(stg),
         p=round(val/n,3))

age_label<-subgrp$age_profile %>%
  dplyr::select(n,stg,site,age_med,age_q1,age_q3) %>%
  mutate(stg=as.factor(stg)) %>%
  gather(var,val,-stg,-site,-n) %>%
  mutate(label=val,
         val=cut(val,breaks=c(-Inf,seq(25,85,by=5),Inf),labels=F,include.lowest=T))


ggplot(age_grp,aes(x=var,y=stg))+
  geom_tile(aes(fill=p))+
  scale_fill_gradient(low="yellow", high="red") +
  geom_line(data=age_label,aes(x=val,y=stg))+
  geom_point(data=age_label,aes(x=val,y=stg))+
  geom_text_repel(data=age_label,aes(x=val,y=stg,label=label))+
  scale_x_continuous(breaks=1:14,
                     labels=c(18,seq(25,85,by=5)))+
  labs(x="Age at visit",y="AKI stage",fill="Proportion")+
  facet_wrap(~site)



#hgb profile
subgrp$hgb_profile %>%
  dplyr::select(stg,site,SEX, dsa_grp, hgb_mean, hgb_sd, hgb_med, hgb_q1, hgb_q3)


