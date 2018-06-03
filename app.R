
library(shiny)
library(shinydashboard)
source("all_functions.R")

callPath <-'/home/ubuntu/csvFiles/calls/'
putPath <- '/home/ubuntu/csvFiles/puts/'

callFiles<-list.files(callPath)
putFiles<- list.files(putPath)
ticker<- unlist(strsplit(callFiles, "[.]csv"))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = 'ZeroPointZeroSigma DashBoard'),
  dashboardSidebar(
   
      sidebarMenu(
      menuItem('Stock Return', tabName = 'return', icon = icon('line-chart')),
      menuItem('Option Overview', tabName = 'overview', icon =icon('bar-chart')),
      menuItem('Call Option Data', tabName = 'calls', icon =icon('bar-chart')),
      menuItem('Put Option Data', tabName = 'puts', icon =icon('bar-chart')),
      menuItem('Pricing Simulator', tabName = 'simulator', icon =icon('th')),
      menuItem('SPX VS VIX vs VXST', tabName = 'spx', icon =icon('line-chart'))
    ),
    selectInput('symbol', 'Select Symbol', ticker)
   
  ),
  dashboardBody( 

    tabItems(
      tabItem(tabName = 'return',
        h2('return')
      ),
      tabItem(tabName = 'overview',
              
              h4('Calls Summary'),
              fluidRow(
                column(12, 
                       tableOutput('c_summary'),
                       tags$head(tags$style('#c_summary table {background-color: gray; 
                                            border: 2px solid black; 
                                            font-size: 12px;}', 
                                            media='screen',
                                            type='text/css'))
                       
                      )
                
              ),
              
              h4('Puts Summary'),
              fluidRow(
                column(12, 
                       tableOutput('p_summary'),
                       tags$head(tags$style('#p_summary table {background-color: gray; 
                                            border: 2px solid black; 
                                            font-size: 12px;}', 
                                            media='screen',
                                            type='text/css'))
                       
                )
                
              )
      
      ),
      tabItem(tabName = 'calls',
              fluidRow(column(3,
                       selectInput('c_expDate', 'Select Expiration', choices = NULL)
                )
              ),
              fluidRow(highchartOutput(outputId = 'c_x_Volume'
                )
              )
      ),
      tabItem(tabName = 'puts',
              fluidRow(column(3,
                     selectInput('p_expDate', 'Select Expiration', choices = NULL)
                )
              )
      ),
      tabItem(tabName = 'simulator',
        h2('simulator')
      ), 
      tabItem(tabName = 'spx',
        h2("spx"))
    )
    
  )
)


server <- function(input, output, session) {
  
  output$c_summary<-renderTable(optionSummary(input$symbol, 'c'), striped = TRUE, bordered = TRUE, digits = 2, spacing='xs')
  
  output$p_summary<-renderTable(optionSummary(input$symbol, 'p'), striped = TRUE, bordered = TRUE, digits = 2, spacing='xs')
  
  output$c_x_Volume <- renderHighchart({chartVolumePrice(optionWeights('AAPL','c'),'2018-06-08')
  })
  
  
  observe({
    updateSelectInput(session = session, inputId = "c_expDate", choices = expirationDate(input$symbol, 'c'))
  })
  
  observe({
    updateSelectInput(session = session, inputId = "p_expDate", choices = expirationDate(input$symbol, 'p'))
  })
  
  
}

                          

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)

