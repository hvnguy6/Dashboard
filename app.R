
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
              fluidRow(highchartOutput(outputId = 'c_x_Volume', height = 300
                )
              ),
              fluidRow(highchartOutput(outputId = 'c_iv_x', height = 300
                )
              ),
              fluidRow(highchartOutput(outputId = 'c_inMoney', height = 300
                )
              )
      ),
      tabItem(tabName = 'puts',
              fluidRow(column(3,
                     selectInput('p_expDate', 'Select Expiration', choices = NULL)
                )
              ),
              
              fluidRow(highchartOutput(outputId = 'p_x_Volume', height = 300
                )
              ),
              fluidRow(highchartOutput(outputId = 'p_iv_x', height = 300
                )
              ),
              fluidRow(highchartOutput(outputId = 'p_inMoney', height = 300
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
  
  observe({
    updateSelectInput(session = session, inputId = "c_expDate", choices = expirationDate(input$symbol, 'c'))
    c<-optionWeights(input$symbol, 'c')
    output$c_x_Volume <- renderHighchart({chartVolumePrice(c, input$c_expDate) })
    output$c_iv_x <-renderHighchart({chartIVStrike(c, input$c_expDate) })
    output$c_inMoney <-renderHighchart({chartInMoneyStrike(c, input$c_expDate)  })
  })
  observe({
    updateSelectInput(session = session, inputId = "p_expDate", choices = expirationDate(input$symbol, 'p'))
    p<-optionWeights(input$symbol, 'p')
    output$p_x_Volume <- renderHighchart({chartVolumePrice(p, input$p_expDate) })
    output$p_iv_x <-renderHighchart({chartIVStrike(p, input$p_expDate) })
    output$p_inMoney <-renderHighchart({chartInMoneyStrike(p, input$p_expDate)  })
  })
}


                
shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)

