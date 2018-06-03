library('data.table')
library('scales')
library('highcharter')
library("dplyr")

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
  o_frame <- x_frame[, list(dailyVolume = sum(Vol), avgBEPrice = mean(BreakEvenStockPrice), DlyGrthX100 = mean(ImpliedStockGrowth_daily)*100, avgWeightedImpliedVol = percent(mean(avgWghtIV)), YrHV = percent(mean(U_Vol1year/(252^.5))), QuarterHV=percent(mean(U_Vol45days/(252^.5))), lastUnderlyingTraded = mean(U_LastTraded), allVolumeTraded =mean(AllVolume)), by = ExpirationDate]
  
  return(o_frame)
}

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

  


