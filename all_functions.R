library('data.table')
library('scales')
library('highcharter')
library('dplyr')
library('quantmod')
library('fBasics')
library('Quandl')
library('dygraphs')
source("global.R")
Quandl.api_key('EcxndBKxcnFeh_sK_yB_')




#This function is the baseline function that will transform the baseline CSV to calculate the weighted IV.  
#input ticker: is the symbol of the stocks: 'AAPL'
#input o_type: is the option type (c, p): 'c' 

optionWeights <- function(ticker, o_type) {
  temp_ticker <-ticker
  temp_type <- o_type
  if (temp_type == 'c') {
    oPath <-'/home/ubuntu/csvFiles/calls/'
  }else {
    oPath <-'/home/ubuntu/csvFiles/puts/'
  }
  oPath <- paste (oPath, temp_ticker, ".csv", sep = "")  
  
  xFrame = fread(oPath, header =TRUE, sep = ',')
  
  #finding the total daily traded volume by option expiration date
  TotalVolume <-xFrame[, list(TotalV = sum(Vol)), by =ExpirationDate]
  TotalVolume$AllVolume <-sum(TotalVolume$TotalV)
  
  setkey(TotalVolume, ExpirationDate)
  setkey(xFrame, ExpirationDate)
  #merging the total volume as colume back to the dataframe
  xFrame <-merge(xFrame, TotalVolume, all.x = TRUE)
 
  #calculating the weighted trading volume by expiration date 
  xFrame$VolumeWeight <-xFrame$Vol/xFrame$TotalV
  
  #calculated field of the implied volitility weight by expiration 
  xFrame$DailyImpliedVolatilityWeighted <- xFrame$VolumeWeight*(xFrame$ImpliedVol/(252^.5))
  WeightedVol <- xFrame[, list(avgWghtIV=sum(DailyImpliedVolatilityWeighted)), by = ExpirationDate]
  setkey(WeightedVol, ExpirationDate)
  
  #WeightedVol is the average weighted implied volitility inferred by the option contract of the expiration term 
  xFrame <-merge(xFrame, WeightedVol, all.x = TRUE)
  return(xFrame)
}

#this function takes a transformed cvs file and output the a weighted summary table of all the options data
#input - ticker 
#input - o_type option type
optionSummary <-function(ticker, o_type) {
  temp_ticker <-ticker
  temp_type <- o_type
  x_frame <- optionWeights(temp_ticker, temp_type)
  o_frame <- x_frame[, list(dailyVolume = sum(Vol), avgBEPrice = mean(BreakEvenStockPrice), DlyGrthX100 = mean(ImpliedStockGrowth_daily)*100, avgWeightedImpliedVol = mean(avgWghtIV)*100, YrHV = mean(U_Vol1year/(252^.5))*100, QuarterHV=mean(U_Vol45days/(252^.5))*100, lastUnderlyingTraded = mean(U_LastTraded), allVolumeTraded =mean(AllVolume)), by = ExpirationDate]
  
  return(o_frame)
}

#a<-optionSummary('BABA', 'c')

#this function will give a character vector of all the expiration date
#input - ticker
#input - o_type option type
expirationDate <-function(ticker, o_type) {
  temp_ticker <-ticker
  temp_type <- o_type
  if (temp_type == 'c') {
    oPath <-'/home/ubuntu/csvFiles/calls/'
  }else {
    oPath <-'/home/ubuntu/csvFiles/puts/'
  }
  oPath <- paste (oPath, temp_ticker, ".csv", sep = "")  
  xFrame = fread(oPath, header =TRUE, sep = ',')
  xDate <- unique(xFrame$ExpirationDate)
  return(xDate)
} 

#this function will generate the first chart of volume vs strike price 
chartVolumePrice <-function(xFrame, xDate) {
  c<-subset(xFrame, xFrame$ExpirationDate == xDate)
 
  
  highchart() %>%
    hc_add_series(c, hcaes(x=Strike, y=Vol), type = "column",
                  name = "Volume Traded",
                  color = '#4682B4', showInLegend = FALSE) %>%
    hc_yAxis(title = list(text = "Volume Traded"), allowDecimals = FALSE, showLastLabel = FALSE) %>%
    
    hc_xAxis(data =c$Strike,
             tickmarkPlacement = "on",
             opposite = FALSE) %>%
    hc_title(text = xDate,
             style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Volume Traded Vs Strike Price") %>%
    hc_tooltip(valueDecimals = 0,
               pointFormat = "{series.name} {point.y:.2f}") %>%
    hc_add_theme(hc_theme_538())
}

#this function will generate the IV vs Strike
chartIVStrike <-function(xFrame, xDate) {
    c<-subset(xFrame, xFrame$ExpirationDate == xDate)
    highchart() %>%
      hc_xAxis(c, hcaes(x=Strike), name = "Strike Price")%>% 
      hc_yAxis(title = list(text = "Daily Implied Volatility"),
                              labels=list(format = '{value}%'), 
               showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE) %>%
      
      hc_add_series(c, hcaes(x=Strike, y=(ImpliedVol/(252^.5))*100), type = "column",
                    name = "Daily Implied Volatility",
                    color = '#4682B4', showInLegend = FALSE) %>%
        
      hc_add_series(c, hcaes(x=Strike, y=(U_Vol45days/(252^.5))*100), type = "line",
                    name = "Last 45 days historical volatility",
                    color = '#ffff00', showInLegend = TRUE) %>%

      hc_add_series(c, hcaes(x=Strike, y=(U_Vol1year/(252^.5))*100), type = "line",
                    name = "Last trailng year historical volatility",
                    color = '#006400', showInLegend = TRUE) %>%
      
      hc_tooltip(valueDecimals = 0,
                 pointFormat = "{series.name} {point.y:.2f}%") %>%
      hc_title(text = xDate,
               style = list(fontWeight = "bold")) %>%
      hc_subtitle(text = "Strike Price vs IV") %>%
      hc_add_theme(hc_theme_538())
    
}


chartInMoneyStrike <-function(xFrame, xDate) {
  c<-subset(xFrame, xFrame$ExpirationDate == xDate)
  highchart() %>%
    hc_xAxis(c, hcaes(x=Strike), name = "Strike Price")%>% 
    hc_yAxis(title = list(text = "Probability of breaking even"),
             labels=list(format = '{value}%'), 
             showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE) %>%
    
    hc_add_series(c, hcaes(x=Strike, y=BreakEvenProbability*100), type = "spline",
                  name = "Probablility of breaking even",
                  color = '#4682B4', showInLegend = FALSE) %>%
    
    hc_tooltip(valueDecimals = 0,
               pointFormat = "{series.name} {point.y:.2f}%") %>%
    hc_title(text = xDate,
             style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Probability of breaking even at different Strike") %>%
    hc_add_theme(hc_theme_538())
  
}

chartPrices <-function(xFrame, xDate) {
  
  c<-subset(xFrame, xFrame$ExpirationDate == xDate)
  highchart() %>%
    hc_xAxis(c, hcaes(x=Strike), name = "Strike Price")%>% 
    hc_yAxis(title = list(text = "ContractPrice"),
             labels=list(format = '{value}'), 
             showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE) %>%
    
    hc_add_series(c, hcaes(x=Strike, y=Last), type = "spline",
                  name = "Last Traded Contract Price",
                  color = '#4682B4', showInLegend = FALSE) %>%
    
    hc_add_series(c, hcaes(x=Strike, y=BreakEvenStockPrice), type = "spline",
                  name = "Break Even Stock Price",
                  color = 'green', showInLegend = FALSE) %>%
    
    hc_add_series(c, hcaes(x=Strike, y=U_LastTraded), type = "spline",
                  name = "Last Traded Underlying Prices",
                  color = 'blue', showInLegend = FALSE) %>%
    
    hc_tooltip(valueDecimals = 0,
               pointFormat = "{series.name} {point.y:.2f}") %>%
    hc_title(text = xDate,
             style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Last Traded Contract Price") %>%
    hc_add_theme(hc_theme_538())
  
}



## SPX vs. VIX lower bound and VXST upper data
Load_SPX_VS_data <- function(some.start.date, some.end.date) {
 
  my.start.date <- as.Date(some.start.date)
  my.end.date <- as.Date(some.end.date)
  
  if(my.end.date- my.start.date < 45) {
    my.start.date  <- my.end.date - 45
  } 
  
  ## SPX VS VIX lower bound and VXST Upper
  
  #UVXY.data <<- stock.raw.data('UVXY', '2017-01-01', Sys.Date())
  
  spx.data <- stock.raw.data('^GSPC', my.start.date, my.end.date)
 
 # spx.data <- stock.raw.data('^GSPC', '2017-01-01', Sys.Date())
  vix.data <- Quandl('CBOE/VIX')
  vix.data <- as.xts((vix.data$'VIX Close') /(12^.5), order.by = vix.data$Date)
  
  vxst.data <- Quandl('CBOE/VXST') 
  vxst.data <- as.xts(vxst.data$Close /(52^.5), order.by = vxst.data$Date)

  xFrame <- as.xts((spx.data$GSPC.Adjusted))
  
  
  colnames(vix.data)[1] <- 'VIX_1STD_1Month'
  colnames(vxst.data)[1] <- 'VXST_1STD_1Week'
  colnames(xFrame)[1] <- 'SPX Close'
  
  xFrame <-merge(xFrame, vix.data, join='left')
  xFrame <-merge(xFrame, vxst.data, join='left')
  xFrame$up <- xFrame$SPX.Close*(1+xFrame$VXST_1STD_1Week/100)
  xFrame$up2 <- xFrame$SPX.Close*(1+2*xFrame$VXST_1STD_1Week/100)
  xFrame$down <-xFrame$SPX.Close*(1-xFrame$VIX_1STD_1Month/100)
  xFrame$down2 <-xFrame$SPX.Close*(1-2*xFrame$VIX_1STD_1Month/100)
  
#Moving the results forward  
  xFrame$up <- lag(xFrame$up, n =5) 
  xFrame$up2 <- lag(xFrame$up2, n =5)
  xFrame$down <- lag(xFrame$down, n = 20)
  xFrame$down2 <- lag(xFrame$down2, n = 20)
  
  return(xFrame)
}

## Plots
SPX_VS_graph <- function(some.data) {
  # plot(x = spx.data.close$GSPC.Adjusted, xlab = "Time", ylab = "Index",
  #      main = "SPX vs VIX lower bound and VXST upper", major.ticks= "months",
  #      minor.ticks = FALSE, col = "red")
  # lines(x = spx.data.close$GSPC.Adjusted.1, col = "darkgreen")
  # lines(x = spx.data.close$GSPC.Adjusted.2, col = "goldenrod")
  # lines(x = spx.data.close$GSPC.Adjusted.3, col = "darkblue")
  # lines(x = spx.data.close$GSPC.Adjusted.4, col = "darkviolet")
  
  colors_pallete <- c('#1aff1a', '#9999ff', '#6666ff', '#ff9999','#ff0000')
  spx.data.close <-some.data
  
   spx.data.close[,c(-2,-3)] %>%
    dygraph(main = "SPX vs VIX lower bound and VXST upper", group = "SPX VS VIX vs VXST") %>%
    dyAxis("y", label = "Index") %>%
    dyHighlight(hideOnMouseOut = T,
                highlightSeriesBackgroundAlpha = 0.15) %>%
    dyOptions(labelsKMB = T,
              gridLineColor = "lightgrey",
              digitsAfterDecimal = 2,
              colors = colors_pallete,
              rightGap = 20,
              strokeWidth = 1) %>%
    dyRangeSelector(dateWindow = c(Sys.Date() - 90, Sys.Date())) %>%
    dyLegend(show = "follow",
             width = 200,
             hideOnMouseOut = T,
             labelsSeparateLines = F)
}

UVXY_graph <- function(some.data) {
  # plot(x = UVXY.data$UVXY.Adjusted, xlab = "Time", ylab = "$$$",
  #      main = "UVXY", major.ticks= "months",
  #      minor.ticks = FALSE, col = "red")
    some.data$UVXY.Adjusted %>%
    dygraph(main = "UVXY", group = "SPX VS VIX vs VXST") %>%
    dyAxis("y", label = "$$$") %>%
    dyHighlight(hideOnMouseOut = T,
                highlightSeriesBackgroundAlpha = 0.15) %>%
    dyOptions(labelsKMB = T,
              gridLineColor = "lightgrey",
              digitsAfterDecimal = 3,
              colors = '#1aff1a',
              rightGap = 20,
              strokeWidth = 1) %>%
    dyRangeSelector(dateWindow = c(Sys.Date() - 90, Sys.Date())) %>%
    dyLegend(show = "follow",
             width = 200,
             hideOnMouseOut = T,
             labelsSeparateLines = F)
}














#testing scripts
# xDate <- '2018-06-08'
# c<-optionWeights('AAPL', 'c')
# c<-subset(c, c$ExpirationDate == xDate)
# c$dailyImpliedVol<-c$ImpliedVol/(252^.5)
# c$daily_U_MeanDailyReturn_1year <-c$U_Vol1year/(252^.5)
# c$daily_U_Mean45 <-c$U_Vol45days/(252^.5)
# max(c$dailyImpliedVol)
# max(c$daily_U_Mean45)



#c<-chartVolumePrice (c, '2018-06-08')

# highchart() %>% 
#   hc_add_series(data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2,
#                          26.5, 23.3, 18.3, 13.9, 9.6),
#                 type = "spline") %>% 
#   hc_xAxis(title = list(text = "x Axis at top"),
#            opposite = TRUE,
#            plotLines = list(
#              list(label = list(text = "This is a plotLine"),
#                   color = "#'FF0000",
#                   width = 2,
#                   value = 5.5))) %>% 
#   hc_yAxis(title = list(text = "y Axis at right"),
#            opposite = TRUE,
#            minorTickInterval = "auto",
#            minorGridLineDashStyle = "LongDashDotDot",
#            showFirstLabel = FALSE,
#            showLastLabel = FALSE,
#            plotBands = list(
#              list(from = 25, to = 80, color = "rgba(100, 0, 0, 0.1)",
#                   label = list(text = "This is a plotBand")))) 
# 
# highchart() %>% 
#   hc_yAxis_multiples(
#     list(top = "0%", height = "30%", lineWidth = 3),
#     list(top = "30%", height = "70%", offset = 0,
#          showFirstLabel = FALSE, showLastLabel = FALSE)
#   ) %>% 
#   hc_add_series(data = rnorm(10)) %>% 
#   hc_add_series(data = rexp(10), type = "spline", yAxis = 1)

#a<-Load_SPX_VS_data('2017-01-01', Sys.Date())

  


