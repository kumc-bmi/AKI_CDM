# Define UI for application
ui<-fluidPage(
    
    # Application title
    titlePanel("SHAP Marginal Effect Dashboard for AKI Prediction Models"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("aki_sel", 
                    h4("AKI Event"), 
                    choices = list("AKI >= 1" = "stg1up",
                                   "AKI >= 2" = "stg2up",
                                   "AKI = 3" = "stg3"),
                    selected = 1),
        
        selectInput("pred_in_d_sel", 
                    h4("Prediction Time window"), 
                    choices = list("Prediction in 24 hours" = 1,
                                   "Prediction in 48 hours" = 2),
                    selected = 2),
        
        selectInput("fs_sel", 
                    h4("Model with/without SCr or BUN"), 
                    choices = list("with SCr or BUN" = "no_fs",
                                   "without SCr or BUN" = "rm_scr_bun"),
                    selected = 1),
        
        sliderInput("impvar_k",
                    h4("Display the partial plot of the kth feature"),
                    min = 1, max = 100, value = c(1,10)),
        
        checkboxInput("show_multi",
                      strong("Show multiple marginal plots?")),
        
        
        p("Note: the second button selects which feature to show the marginal effect as well as the last 
          feature shown in the feature importance ranking; while the first button selects the first feature 
          shown in the feature importance ranking."),
        p("Make sure to only select less than 12 features to show at a time to achive the best visual results.")
      ),
    
    
    mainPanel(
      plotOutput("dplot"),
      plotOutput("implot")
    )
  )
)
