########################################
## Calculate adjMMD and KL-divergence ##
########################################

rm(list=ls()); gc()

source("./R/util.R")
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "scales",
                    "broom",
                    "Matrix"))


##task parameters
#-----source model
src_mod<-"kumc"

#-----target data
tgt_dat<-"test"

#-----prediction point
pred_in_d_opt<-c(2,1)

#-----prediction tasks
pred_task_lst<-c("stg2up","stg1up","stg3")

#-----feature selection type
fs_type_opt<-c("no_fs","rm_scr_bun")

#-----sampling rate
sample_p<-0.90

#top number of variables
topk<-100

bm_nm<-c()
bm_tm<-c()
bm_unit<-c()

for(pred_in_d in pred_in_d_opt){
  
  for(fs_type in fs_type_opt){
    
    ref_dat_d<-readRDS(paste0("./data/preproc/model_",src_mod,"/",pred_in_d,"d_",fs_type,".rda"))
    
    for(pred_task in pred_task_lst){
      #initialization
      v_dist_stk<-c()
      v_dist_joint_stk<-c()
      
      #important variable list based on source model
      ref_var<-readRDS(paste0("./data/",mod,"_validation/",ref,"/",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))$feat_imp %>%
        dplyr::mutate(rank=rank(-Gain),
                      cum_Gain=cumsum(Gain))
      
      #load source data
      ref_feat<-ref_dat_d[[1]] %>%
        left_join(ref_dat[[2]],by=c("ENCOUNTERID","dsa_y")) %>%
        semi_join(ref_dat[[2]] %>% dplyr::select(ENCOUNTERID) %>% 
                    unique %>% sample_frac(sample_p),  #sample for better efficiency
                  by="ENCOUNTERID")
      
      a<-nrow(unique(ref_feat[,c("ENCOUNTERID","dsa_y") ]))   
      ay<-sum(unique(ref_feat[,c("ENCOUNTERID","dsa_y","y")])$y)
      
      
      site_dat<-readRDS(paste0("./data/preproc/",tgt_dat,"/",pred_in_d,"d_",pred_task,".rda"))
      
      #loop over topk features
      for(v in 1:topk){
        start_i<-Sys.time()
        #----------------------------------------------------------------------------------------------
        #ref-q(x)
        pop_ref<-nrow(ref_feat)
        #extract empirical dist for feature v from source data
        q<-ref_feat %>% 
          dplyr::filter(key==ref_var$Feature[v]&!is.na(value)) %>%
          arrange(value) %>%
          distinct(ENCOUNTERID,dsa_y,y,.keep_all = TRUE)
        
        if(nrow(q)<=1){
          next #skip if missing completely from source data
        }
        
        n<-length(q$value)
        ny<-sum(q$y)
        k<-length(unique(q$value))
        
        #------source feature vector-------
        x_q<-q$value
        
        #some set up for density distribution estimation
        if(k<=1){
          var_type<-"binary"
          brk<-data.frame(bin=1:2,
                          brk_val=c(0,1))
        }else if(k>1&k<=20){
          var_type<-"discrete"
          brk<-data.frame(bin=seq_len(k),
                          brk_val=unique(q$value)[order(unique(q$value))])
        }else{
          var_type<-"continuous"
          #use Freedman and Diaconis to estimate reference density
          brk<-bin_fd(q$value)
        }
        
        #prep for density estimation
        v_dist<<-c()
        comp<-data.frame(y=q$y,x=q$value,site="ref",stringsAsFactors = F) %>%
          dplyr::mutate(k=1) %>%
          full_join(brk %>% dplyr::mutate(k=1),by="k") %>%
          dplyr::select(-k) 
        
        #------target feature vector-------
        if(site==ref){
          site_feat<-ref_feat
          pop_site<-pop_ref
          p<-q
          b<-a
          by<-ay
          m<-n
          my<-ny
        }else{
          site_feat<-site_dat[[1]] %>%
            left_join(site_dat[[2]],by=c("ENCOUNTERID","dsa_y")) %>%
            semi_join(site_dat[[2]] %>% dplyr::select(ENCOUNTERID) %>% 
                        unique %>% sample_frac(sample_p),
                      by="ENCOUNTERID")
          pop_site<-nrow(site_feat)
          b<-nrow(unique(site_feat[,c("ENCOUNTERID","dsa_y")]))
          by<-sum(unique(site_feat[,c("ENCOUNTERID","dsa_y","y")])$y)
          
          p<-site_feat %>%
            dplyr::filter(key==ref_var$Feature[v]&!is.na(value)) %>%
            arrange(value) %>%
            distinct(ENCOUNTERID,dsa_y,y,.keep_all = TRUE)
          m<-length(p$value)
          my<-sum(unique(p[,c("ENCOUNTERID","dsa_y","y")])$y)
        }
        
        #track result
        out_site<-c(ref_var$Feature[v],var_type,site,a,ay,n,ny,b,by,m,my)
        
        #--------if not completely missing---------------------
        if(nrow(p)>1){
          #-----------------------compare missing---------------------------------------
          miss_mt<-q %>% dplyr::select(value,y) %>% 
            dplyr::mutate(value=1) %>%
            bind_rows(ref_feat %>% 
                        dplyr::select(ENCOUNTERID,dsa_y,y) %>% unique %>%
                        anti_join(q,by=c("ENCOUNTERID","dsa_y")) %>%
                        dplyr::mutate(value=0) %>%
                        dplyr::select(value,y)) %>% 
            dplyr::mutate(site="ref") %>%
            bind_rows(p %>% dplyr::select(value,y) %>%
                        dplyr::mutate(value=1) %>%
                        bind_rows(site_feat %>% 
                                    dplyr::select(ENCOUNTERID,dsa_y,y) %>% unique %>%
                                    anti_join(p,by=c("ENCOUNTERID","dsa_y")) %>%
                                    dplyr::mutate(value=0) %>%
                                    dplyr::select(value,y)) %>%
                        dplyr::mutate(site=site))
          
          miss_mt %<>%
            dplyr::mutate(site=relevel(as.factor(site),ref="ref"))
          
          #whether missing distribution is different
          fit_na<-glm(value ~ site, data=miss_mt, family="binomial")
          fit_summ_na<-summary(fit_na) 
          
          comp %<>%
            dplyr::mutate(site=as.character(site)) %>%
            bind_rows(data.frame(y=p$y,x=p$value,site=site,stringsAsFactors = F) %>%
                        dplyr::mutate(k=1) %>%
                        full_join(brk %>% dplyr::mutate(k=1),by="k") %>%
                        dplyr::select(-k)) 
          
          #----------------------compare mean--------------------------------------------
          if(var_type=="continuous"){
            comp %<>% dplyr::filter(x>=brk_lb&x<brk_ub) %>%
              unite("brk_val",c("brk_lb","brk_ub"),sep=",")
          }else if(var_type=="discrete"){
            comp %<>% dplyr::filter(x==brk_val)
          }else if(var_type=="binary"){
            comp<-miss_mt %>% left_join(brk,by=c("value"="brk_val")) %>%
              dplyr::mutate(brk_val=value)
          }else{
            stop("data type is not supported for current analysis!")
          }
          
          comp %<>%
            dplyr::mutate(site=relevel(as.factor(site),ref="ref"))
          
          if(var_type!="binary"){
            fit_val<-glm(x ~ site, data=comp %>% dplyr::filter(!is.na(x)), family="gaussian")
            fit_summ_val<-summary(fit_val) #whether value distribution is different
          }else{
            fit_summ_val<-fit_summ_na
          }
          
          #------------------------kl divergence-------------------------------
          p_na<-1-exp(sum(fit_summ_na$coefficients[,1]))/(1+exp(sum(fit_summ_na$coefficients[,1])))
          q_na<-1-exp(fit_summ_na$coefficients[1,1])/(1+exp(fit_summ_na$coefficients[1,1]))
          
          kl_div<-comp %>% 
            group_by(site) %>%
            dplyr::mutate(n=n()) %>%
            ungroup %>%
            group_by(site,brk_val,bin,n) %>%
            dplyr::summarize(freq=n(),.groups="drop") %>%
            ungroup %>%
            dplyr::mutate(px=ifelse(site=="ref",freq/n*(1-q_na),freq/n*(1-p_na))) %>%
            dplyr::select(bin,px,site) %>%
            spread(site,px,fill=0) %>%
            # dplyr::filter(!(ref==0&get(site)>0)) %>%
            dplyr::mutate(ref=ifelse(ref==0,1/n,ref)) %>%
            dplyr::filter(get(site)>0) %>%
            dplyr::mutate(lr=ifelse(get(site)==0&ref==0,0,get(site)/ref)) %>%
            dplyr::mutate(prod=get(site)*log(lr))
          
          # hybrid kl divergence (with missing pattern)
          kl_div_out<-sum(kl_div$prod)+p_na*log(p_na/q_na)
          
          #-------------------------adjusted mmd--------------------------------
          if(site==ref){
            mmd<-0
          }else{
            q_rt<-n/a #=1-q_na
            p_rt<-m/b #=1-p_na
            if(q_rt <= p_rt){
              x_p<-p$value
              
            }else{
              x_p<-c(p$value,
                     penalize_sample(q$value,round(b*q_rt-m))) #penalized sampling
            }
            mmd<-sqrt(abs(get_ks(x_q,x_q)+get_ks(x_p,x_p)-get_ks(x_q,x_p,unbiased=F)-get_ks(x_p,x_q,unbiased=F)))
          }
          
          #--collect results
          out_site<-c(out_site,
                      as.numeric(fit_summ_na$coefficients[2,]),
                      as.numeric(fit_summ_val$coefficients[2,]),
                      as.numeric(kl_div_out),
                      as.numeric(mmd))
        }else{
          #--------if completely missing---------------------
          #------------------------kl divergence-------------------------------
          if(var_type=="continuous"){
            comp %<>% unite("brk_val",c("brk_lb","brk_ub"),sep=",")
          }else if(var_type=="discrete"){
            comp %<>% dplyr::filter(x==brk_val)
          }else if(var_type=="binary"){
            comp<-comp
          }else{
            stop("data type is not supported!")
          }
          
          #kl divergence: when variable is completely missing, impute with the most-unlikely value
          kl_div<-comp %>% 
            group_by(site) %>%
            dplyr::mutate(n=n()) %>%
            ungroup %>%
            group_by(site,brk_val,bin,n) %>%
            dplyr::summarize(freq=n(),.groups="drop") %>%
            dplyr::mutate(qx=freq/n)
          
          kl_div_out<-1/max(kl_div$qx)
          
          #-------------------------adjusted mmd--------------------------------
          #when variable is completely missing, impute a constant to create artificial biases
          if(site==ref){
            mmd<-0
          }else{
            q_rt<-n/a
            x_p<-penalize_sample(q$value,round(b*q_rt-m)) #penalized sampling
            
            mmd<-sqrt(abs(get_ks(x_q,x_q)+get_ks(x_p,x_p)-get_ks(x_q,x_p,unbiased=F)-get_ks(x_p,x_q,unbiased=F)))
          }
          
          #--collect results
          out_site<-c(out_site,
                      rep(NA,8),
                      as.numeric(kl_div_out),
                      as.numeric(mmd))
        }
        
        names(out_site)<-NULL
        v_dist<-rbind(v_dist,out_site) 
        
        ##------model join distribution diparities among sites
        colnames(v_dist)<-c("var","var_type","site",
                            "ref_overall_support","ref_case_support",
                            "ref_overall_support_with","ref_case_support_with",
                            "overall_support","case_support",
                            "overall_support_with","case_support_with",
                            paste0("ind_",c("est","std","zval","pval")),
                            paste0("val_",c("est","std","zval","pval")),
                            "kl_div",
                            "mmd") 
        rownames(v_dist)<-NULL
        
        #collect marginal distribution difference
        v_dist<-as.data.frame(v_dist)
        v_dist_df<-data.frame(v_dist[,1:3] %>%
                                mutate_all(as.character),
                              v_dist[,-(1:3)] %>%
                                mutate_all(as.character) %>%
                                mutate_all(as.numeric))
        v_dist_stk %<>% bind_rows(v_dist_df)
        
        #collect joint distribution difference
        v_dist_stk2<-v_dist_stk %>%
          left_join(ref_var,by=c("var"="Feature")) %>%
          group_by(site) %>%
          dplyr::mutate(Gain_adj=Gain/max(cum_Gain)) %>%
          ungroup %>%
          dplyr::mutate(kl_div_wt=kl_div*Gain_adj,
                        mmd2_wt=mmd^2*Gain_adj) %>%
          arrange(site,rank)
        
        v_dist_joint<-v_dist_stk2 %>%
          group_by(site) %>%
          dplyr::summarize(v_incld=max(.data[["rank"]],na.rm=T),
                           kl_div_joint=sum(.data[["kl_div_wt"]],na.rm=T),
                           mmd_joint=sqrt(sum(.data[["mmd2_wt"]],na.rm=T)),
                           .groups="drop")
        
        v_dist_joint_stk %<>% bind_rows(v_dist_joint)
        
        #----------------------------------------------------------------------------------------------
        lapse<-round(Sys.time()-start_i,2)
        prt_head<-paste(pred_in_d,fs_type,pred_task,i,ref_var$Feature[v], sep=",")
        cat(prt_head,"...in",lapse,units(lapse),".\n")
        
        bm_nm<-c(bm_nm,prt_head)
        bm_tm<-c(bm_tm,lapse)
        bm_unit<-c(bm_unit,units(lapse))
      }

      v_out<-list(v_dist=v_dist_stk,
                  v_dist_joint=v_dist_joint_stk)
      
      saveRDS(v_out,file=paste0("./data/model_",src_mod,"/adjMMD_",tgt_dat,"_",pred_in_d,"d_",fs_type,"_",pred_task,".rda"))
    }
  }
}

# bm<-data.frame(bm_nm=bm_nm,
#                bm_tm=bm_tm,
#                bm_unit=bm_unit,
#                stringsAsFactors = F)
# 
# saveRDS(bm,file="./data/benchmark/bm_adjMMD.rda")




