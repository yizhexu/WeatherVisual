shinyUI(
  
  fluidPage(
    theme = 'styles.css',
    
    # show a map first
    fluidRow(
      leafletOutput("map")
    ), 
    
    # Application title
    h2("Daily Weather Summary of Climate Smart Agriculture Indicators"),
    
    includeMarkdown("./www/description.md"),
    
    br(), 
    
    # buttons on top
    h3("Query Data"),
    
    br(), 
    
    fluidRow(
      
      column(3, 
             h4("aWhere Login"), 
             textInput(inputId ="api_key", label = "Username", value = "yizhexu@awhere.com"),
             passwordInput(inputId = "api_code", label = "Password", value = "181225tiancai@X")
      ), 
      
      column(3, 
             h4("Locations"),
             numericInput(inputId = "lat", label = "Latitude", value = 39.925237, min = -90, max = 90, step = NA),
             numericInput(inputId = "lng", label = "Longitude", value = -105.104391, min = -180, max = 180, step = NA)
      ),
      column(3, 
             h4("Parameters"), 
             selectInput(inputId = "forecast", label = "Include Forecast", choices = c("Yes", "No"), selected = "Yes"), 
             selectInput(input = "attribute", label = "Select Variable for Analysis", choices = c("Precipitation & 1 Year Norm" = "precip_1",
                                                                                                  "Precipitation & 3 Year Norm" = "precip_3",
                                                                                                  "Precipitation & 10 Year Norm" = "precip_10", 
                                                                                                  "Precipitation & Potential Evaporation" = "p_pet"), selected = "precip_10")
      ),
      column(3, 
             h4("Access API"),
             actionButton(inputId = "fetchdata", label = "Access API")
             )
    ),
    
    br(),
    
    h3("Select Data Visualization / Aggregation Options"),
    br(), 
    fluidRow(
      column(4, 
             selectInput(inputId = "aggregation", label = "Data Aggregation Level", choices = c("By Day", "By Week", "By Month"), selected = "By Week")
      ),
      column(4,
             selectInput(inputId = "accumulate", label = "View Accumulative Data", choices = c("Yes", "No"), selected = "No")
      ), 
      column(4, 
             numericInput(inputId = "quantile", label = "Percent Quantile to Show", value = 0.99, min = 0, max = 1, step = 0.01)
      )
    ),
    
    br(),
    
    h3("View Data Visualization / Aggregation Result"),
    
    br(),
    
    fluidRow(
      column(12, 
             tabsetPanel(type = "tabs", position = "right", selected = "Data Visualization",
                         
                         tabPanel("Data Visualization", 
                                  
                                  includeMarkdown("./www/description_visual.md"), br(), 
                                  fluidRow(
                                    column(12, 
                                           dygraphOutput("dygraph", height = "900px")
                                           )
                                    ),
                                  fluidRow(
                                    column(12, 
                                           br(), 
                                           uiOutput("ppet_ui")
                                    )
                                  )
                         ), 
                         
                         tabPanel("Data Query Result", br(), 
                                  includeMarkdown("./www/description_data.md"), br(), 
                                  column(12,
                                         DT::dataTableOutput("table")
                                  )
                         )
                         
             )
      )
    ) 
    
    
  )  
)