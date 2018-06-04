library(shiny)
library(shinydashboard)
source("all_functions.R")
source("global.R")
source("volcharting.R")


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
        h2('Stock Return'),
        fluidRow(uiOutput("DateSelector_return")
                ),
        fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                             plotOutput(outputId = "distPlot_daily_return", height = "200px"),
                             plotOutput(outputId = "distPlot_daily_volume", height = "200px")
                            )
                ),
        br(),
        fluidRow(highchartOutput(outputId = 'Plot_volume_adjustedPrice', height = "350px")
                ),
        br(),
        #verbatimTextOutput()
        formattableOutput(outputId = 'summary')
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
        h2("SPX VS VIX vs VXST"),
        dygraphOutput("SPX_VS_graph", height = "300px"),
        br(),
        dygraphOutput("UVXY_graph", height = "250px")
        )
    )
    
  )
)


server <- function(input, output, session) {
  ### Stock Return ##########################################
  output$DateSelector_return <- renderUI({
    dateRangeInput(label = 'Date Range:',
                   inputId = 'MySelectedDateRange_return',
                   start = Sys.Date() - 365, end = Sys.Date(),
                   min = '2016-01-01', max = Sys.Date()
                  )
  })
  
  my.current.raw.data <- reactive({stock.raw.data(
      some.stock = input$symbol,
      some.start.date = input$MySelectedDateRange_return[1],
      some.end.date = input$MySelectedDateRange_return[2])
  })
  
  # Daily Return
  my.current.dist_daily_return.data <- reactive({
    data.dist_daily_return(some.raw.data = my.current.raw.data())
  })
  
  
  my.current.dist_daily_return.graph <- reactive({
    graph.dist_daily_return(some.graph.data = my.current.dist_daily_return.data())
  })
  
  
  output$distPlot_daily_return <- renderPlot({
    my.current.dist_daily_return.graph()
  })
  
  
  # Daily Volume
  my.current.dist_daily_volume.data <- reactive({
    data.dist_daily_volume(some.raw.data = my.current.raw.data())
  })
  
  
  my.current.dist_daily_volume.graph <- reactive({
    graph.dist_daily_volume(some.graph.data = my.current.dist_daily_volume.data())
  })
  
  
  output$distPlot_daily_volume <- renderPlot({
    my.current.dist_daily_volume.graph()
  })
  
  
  # Volume and Adjusted price
  my.current.volume_adjustedPrice.graph <- reactive({
    graph.volume_adjustedPrice(some.raw.data = my.current.raw.data())
  })
  
  output$Plot_volume_adjustedPrice <- renderHighchart({
    my.current.volume_adjustedPrice.graph()
  })
  
  # Summary Table on Selected days
  my.curent.summary.data <- reactive({
    data.stats_summary(some.raw.data = my.current.raw.data())
  })
  
  output$summary <- renderFormattable({
    formattable(my.curent.summary.data())
  })
  
  
  
  
  ### Option Overview #######################################
  output$c_summary<-renderTable(optionSummary(input$symbol, 'c'), striped = TRUE, bordered = TRUE, digits = 2, spacing='xs')
  output$p_summary<-renderTable(optionSummary(input$symbol, 'p'), striped = TRUE, bordered = TRUE, digits = 2, spacing='xs')
  
  
  ### Call Option ###########################################
  observe({
    updateSelectInput(session = session, inputId = "c_expDate", choices = expirationDate(input$symbol, 'c'))
    c<-optionWeights(input$symbol, 'c')
    output$c_x_Volume <- renderHighchart({chartVolumePrice(c, input$c_expDate) })
    output$c_iv_x <-renderHighchart({chartIVStrike(c, input$c_expDate) })
    output$c_inMoney <-renderHighchart({chartInMoneyStrike(c, input$c_expDate)  })
  })
  
  ### Put Option ############################################
  observe({
    updateSelectInput(session = session, inputId = "p_expDate", choices = expirationDate(input$symbol, 'p'))
    p<-optionWeights(input$symbol, 'p')
    output$p_x_Volume <- renderHighchart({chartVolumePrice(p, input$p_expDate) })
    output$p_iv_x <-renderHighchart({chartIVStrike(p, input$p_expDate) })
    output$p_inMoney <-renderHighchart({chartInMoneyStrike(p, input$p_expDate)  })
  })
  
  
  ### SPX VS VIX vs VXST #####################################
  # SPX_VS_graph
  my.current.SPX_VS_graph <- reactive({
    SPX_VS_graph()
  })
  
  output$SPX_VS_graph <- renderDygraph({
    my.current.SPX_VS_graph()
  })
  
  # UVXY_graph
  my.current.UVXY_graph <- reactive({
    UVXY_graph()
  })
  
  output$UVXY_graph <-  renderDygraph({
    my.current.UVXY_graph()
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)