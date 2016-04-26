shinyUI(navbarPage("Strategy Analyses",
  tabPanel("Correlation Analysis",                 
  titlePanel("Correlation Analysis"),
  sidebarPanel(
    fileInput('file1', 'Load Strategy',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    fileInput('file2', 'Select Second Strategy',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    selectInput('period', 'Select Timeframe', choices = c('1-hour' = "1h", '2-hour' = "2h", '4-hour' = "4h", '6-hour' = "6h", '1-day'="1d")),
    
    actionButton("goButton", "Analyze"),
    tags$hr()
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("contents1")), 
      tabPanel("Summary", "Correlation:", tableOutput("contents2"))
  )
  )
  ),
  tabPanel("Market Condition Analysis",
           titlePanel("Market Condition Analysis"),
           sidebarPanel(
             fluidRow(
               column(8,
             fileInput('ohlc', 'Load OHLC Data',
                       accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
             tags$hr(),
             h4("Select Indicators"),
             selectInput("indicator_1", label = h5("Indicator 1"), 
                         choices = list("RSI" = 1, "CCI" = 2, "Log Returns" = 3), 
                         selected = 1),
             numericInput("indicator_1_period", label = h6("Indicator 1 Period"), value = 14),
             selectInput("indicator_2", label = h5("Indicator 2"), 
                         choices = list("RSI" = 1, "CCI" = 2, "Log Returns" = 3), 
                         selected = 2),
             numericInput("indicator_2_period", label = h6("Indicator 2 Period"), value = 14),
             br(),
             br(),
             actionButton("indicator_run", "Analyze")
             
                      )
                )
           ),
           mainPanel(
             plotOutput("Market_Analysis_Test")
           )
           )
))

