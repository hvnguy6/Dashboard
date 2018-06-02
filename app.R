
library(shiny)
library(shinydashboard)

callPath <-'/home/ubuntu/csvFiles/calls/'
putPath <- '/home/ubuntu/csvFiles/puts/'

callFiles<-list.files(callPath)
putFiles<- list.files(putPath)
ticker<- unlist(strsplit(callFiles, "[.]csv"))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "ZeroPointZeroSigma DashBoard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Stock Return', tabName = 'return', icon = icon('line-chart')),
      menuItem('Option Overview', tabName = 'overview', icon =icon('bar-chart')),
      menuItem('Call Option Data', tabName = 'calls', icon =icon('bar-chart')),
      menuItem('Put Option Data', tabName = 'puts', icon =icon('bar-chart')),
      menuItem('Pricing Simulator', tabName = 'simulator', icon =icon('th')),
      menuItem('SPX VS VIX vs VXST', tabName = 'spx', icon =icon('line-chart'))
      
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem(tabName = 'return',
        h2('return')
      ),
      tabItem(tabName = 'overview',
              fluidRow(
                column(2,
                      selectInput('symbol', 'Select Symbol', ticker)
                ),
                column(2, offset = 1,
                       selectInput('expDate', 'Select Expiration', 'PlaceHolder2')
                ), 
                
                column(2, offset = 1,
                     selectInput('strike', 'Select Strike', 'PlaceHolder3')
                )
              ),
              fluidRow(
                column(12, textOutput("test")
                       )
                
              )
      
      ),
      tabItem(tabName = 'calls',
        h2('calls')
      ),
      tabItem(tabName = 'puts',
        h2('puts')
      ),
      tabItem(tabName = 'simulator',
        h2('simulator')
      ), 
      tabItem(tabName = 'spx',
        h2("spx"))
    )
    
  )
)


server <- function(input, output) {
  
  output$test<-renderText({paste("You chose", input$symbol)})
}
                          

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)

