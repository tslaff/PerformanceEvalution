shinyUI(fluidPage(
  titlePanel(img(src="InovanceFinTechLogo.png",height=275,width=275)),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Performance Evaluation"),
      
      selectInput("var", label = h4("Select Data"), 
                  choices = list("Equity","Trades"), selected = "Equity")),
      
      
    
    mainPanel(
      plotOutput("distPlot")
      )
  )
))
