library(shinyBS)

shinyUI(navbarPage("Strategy Analyses",
  tabPanel("Correlation Analysis",                 
    titlePanel("Correlation Analysis"),
      sidebarPanel(
        p('Find the correlation between two strategies'),
        h4('Use Sample Data'),
          actionButton("loadsampleButton", "Load Sample Data"),
          textOutput("loadmessage"),
            bsPopover("loadsampleButton", "Sample Data","Two daily forex strategies.",
                  placement = "right",trigger = "hover"),
        h4('Use Your Data'),
          fileInput('file1', 'Load Strategy',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          fileInput('file2', 'Select Second Strategy',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          bsPopover("file1", "Format","Order number, Buy/Sell, Open Time, Close Time, Open Price, Close Price, Trade Return, Equity",
              placement = "right",trigger = "hover"),
          bsPopover("file2", "Format","Order number, Buy/Sell, Open Time, Close Time, Open Price, Close Price, Trade Return, Equity",
                  placement = "right",trigger = "hover"),
          h4('Timeframe'),
              p('Select the largest common denominator in timeframes between the two strategies'),
          selectInput('period', '', choices = c('1-day'="1d",'6-hour' = "6h", '4-hour' = "4h",'2-hour' = "2h",'1-hour' = "1h")),
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
        p('Identify different market regimes based on two indicators.'),
          h4('Use Sample Data'),
            actionButton("loadsampleButton_ohlc", "Load Sample Data"),
            textOutput("loadmessage_ohlc"),
              bsPopover("loadsampleButton_ohlc", "Sample Data","EURUSD 4-hour data.",
                  placement = "right",trigger = "hover"),
              br(),
            h4('User Your Data'),
          fluidRow(
            column(8,
            fileInput('ohlc', 'Load OHLC Data',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            bsPopover("ohlc", "Format","Timestamp, Open, High, Low, Close, Volume",
                      placement = "right",trigger = "hover"),
             tags$hr(),
            h4("Select Indicators"),
              p('Use these indicators to identify the different market regimes.'),
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

