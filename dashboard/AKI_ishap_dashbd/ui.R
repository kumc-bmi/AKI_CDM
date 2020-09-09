library(shiny)
library(shinydashboard)

# Define UI for application
ui<-dashboardPage(
  dashboardHeader(title="SHAP Explorer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    h3("Prediction of AKI>=2 in 48-hr using DS-GBT model"),
    fluidRow(
      column(4,
             selectInput("id", 
                         h4("Patient ID (random)"), 
                         choices = c(1:100),
                         selected = 1)),
      column(4,
             sliderInput("n_var",
                         h4("Range of important features included"),
                         min = 1, max = 100, value = c(1,10)))
    ),
    
    fluidRow(
      infoBoxOutput("agebox"),
      infoBoxOutput("sexbox"),
      infoBoxOutput("racebox")
    ),
    
    fluidRow(
      infoBoxOutput("dsabox"),
      infoBoxOutput("scr_basebox"),
      infoBoxOutput("predbox")
    ),

    plotOutput("mainplot",height = 600)
  )
)
