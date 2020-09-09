library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

APP2_SRCDAT<-readRDS("./data/dashbd_ishap.rda")

# Define server logic ----
server <- function(input, output, session) {
  # Dashboard reactives--------------------------
  datainput1<-reactive({
    APP2_SRCDAT$sample_basic %>% 
      filter(DISPLAY_ROW_ID == input$id)
  })
  
  datainput2<-reactive({
    APP2_SRCDAT$sample_shap %>% 
      semi_join(datainput1(),by="ROW_ID") %>%
      arrange(desc(abs(shap))) %>%
      dplyr::slice(input$n_var[1]:input$n_var[2]) %>%
      inner_join(APP2_SRCDAT$sample_xval,
                 by=c("ROW_ID","vari")) %>%
      mutate(label=coalesce(VALUESET_ITEM_DESCRIPTOR,feature_label),
             label=paste0(substr(label,1,30),"\n..."),
             label=paste0(label,"@",
                          coalesce(feature_modifier,"")," = ",round(xval,2)),
             eff_sign_lab=recode(eff_sign,
                                 "-1"="Decrease(-)",
                                 "1"=" Increase(+)"),
             eff_sign_symbol=recode(eff_sign,
                                    "-1"="-",
                                    "1"="+"),
             eff_rank_fac=as.factor(eff_rank),
             eff_rank_fac=factor(eff_rank_fac,levels=rev(levels(eff_rank_fac))))
  })
  
  datainput3<-reactive({
    pred_vec<-c(round(datainput2()$pred[1],4),
                round(log(datainput2()$pred[1]/(1-datainput2()$pred[1])),2),
                datainput2()$perc_rk[1])
  })
  
  # Dashboard Boxes------------------------------
  output$agebox<-renderInfoBox({
    age<-datainput1() %>%
      select(AGE) %>% unlist
    
    infoBox(
      "AGE:",age,color="green",fill=TRUE
    )
  })
    
  output$sexbox<-renderInfoBox({
    sex<-datainput1() %>%
      select(SEX) %>% unlist
    
    infoBox(
      "SEX:",sex,color="yellow",fill=TRUE
    )
  })
  
  output$racebox<-renderInfoBox({
    race<-paste0(datainput1() %>% select(RACE) %>% unlist, ",",
                 datainput1() %>% select(HISPANIC) %>% unlist)
    
    infoBox(
      "RACE/ETHNICITY:",race,color="blue",fill=TRUE
    )
  })
  
  output$dsabox<-renderInfoBox({
    dsa<-datainput1() %>%
      select(DAY_SINCE_ADMIT) %>% unlist
    
    infoBox(
      "DAY SINCE ADMISSION:",dsa,color="teal",fill=TRUE
    )
  })
  
  output$scr_basebox<-renderInfoBox({
    scr<-datainput1() %>%
      select(SERUM_CREAT_BASE) %>% unlist
    
    infoBox(
      "ADMINISSON SERUM CREATININE:",scr,color="purple",fill=TRUE
    )
  })
  
  output$predbox<-renderInfoBox({
    pred<-paste0(datainput3()[1],"(log odds = ",datainput3()[2],")",";\n",
                 "risk percentile:",datainput3()[3])
    
    infoBox(
      "PREDICTTED RISK SCORE:",pred,color="red",fill=TRUE
    )
  })

  # Dashboard Plots-----------------------------
  output$mainplot<-renderPlot({
    ggplot(datainput2(),
           aes(x=eff_rank_fac,y=shap,fill=eff_sign_lab))+
      geom_bar(stat = "identity",position="identity")+
      ylim(min(datainput2()$shap)-0.1,max(datainput2()$shap)+0.15)+
      coord_flip()+
      labs(x="Ranking for Effect size",y="SHAP value",
           fill="Direction of Effect")+
      geom_text(aes(label=label),fontface="bold",size=4)+
      annotate("text",x=nrow(datainput2()),y=max(datainput2()$shap)+0.1,
               label=paste0(datainput3()[2]," = "),
               color="red",fontface="bold",size=5)+
      geom_text(aes(y=max(datainput2()$shap)+0.13,label=paste0(eff_sign_symbol,round(abs(shap),2))),
                fontface="bold",size=5)+
      theme(text=element_text(size=15,face="bold"),
            plot.subtitle = element_text(hjust = 0.5))
      
  })
}
