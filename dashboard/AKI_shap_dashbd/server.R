# Define server logic ----
library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

explain<-readRDS("./data/dashbd_srcdat.rda")
impvar<-readRDS("./data/dashbd_varimp.rda")

server <- function(input, output) {
  
  datainput1<-reactive({
    impvar %>% 
      filter(pred_in_d == input$pred_in_d_sel&
               pred_task == input$aki_sel&
               fs_type == input$fs_sel) %>%
      dplyr::slice(input$impvar_k[1]:input$impvar_k[2])
  })
  
  datainput2<-reactive({
    if(input$show_multi){
      explain %>% 
        inner_join(datainput1(),
                   by=c("pred_task","pred_in_d","fs_type","Feature")) %>%
        mutate(ncol=ceiling((input$impvar_k[2]-input$impvar_k[1])/2))
    }else{
      explain %>% 
        inner_join(datainput1() %>% filter(rank==max(rank)),
                   by=c("pred_task","pred_in_d","fs_type","Feature")) %>%
        mutate(ncol=1)
    }
  })
  
  output$implot<-renderPlot({
    ggplot(datainput1() %>%
             mutate(feature_rank_fac=as.factor(paste0(substr(label,1,44),"\n",substr(label,45,nchar(label)))),
                    feature_rank_fac=factor(feature_rank_fac,levels=rev(levels(feature_rank_fac)))),
           aes(x=feature_rank_fac,y=Norm_Gain))+
      geom_bar(stat="identity")+
      labs(x="Features",y="Normalized Scale")+
      scale_fill_grey()+
      guides(fill = FALSE) +
      theme(text=element_text(size=17,face="bold",color="black"))+
      coord_flip()+
      scale_y_continuous(trans = "reverse")
  })
  
  output$dplot<-renderPlot({
    plt<-ggplot(datainput2(),aes(x=val,y=eff_med))+
      geom_point(alpha=0.5)+
      geom_errorbar(aes(ymin=eff_lb,ymax=eff_ub))+
      labs(x="Value",y="log(odds ratio)",
           color="Validation Site",fill="Validation Site")+
      geom_hline(yintercept = 0,linetype=2) + 
      theme(text=element_text(size=15,face="bold"))
    
    if(max(datainput2()$val_grain) > 2){
      plt<-plt+
        geom_smooth(method = 'gam',formula=y ~ s(x, bs = "cs",k=7))+   #binary variables may lead to warnings
        facet_wrap(~label,scales="free", ncol=datainput2()$ncol[1])
    }else{
      plt<-plt+
        geom_line(linetype=2)+
        facet_wrap(~label,scales="free", ncol=datainput2()$ncol[1])
    }
    plt
  })
  
}
