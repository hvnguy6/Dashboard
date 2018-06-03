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

#this function will generate the first chart of volume vs price 
chartVolumePrice <-function(xFrame, xDate) {
  c<-subset(xFrame, xFrame$ExpirationDate == xDate)
  highchart() %>%
  hc_add_series(c, hcaes(x=Strike, y=Vol, z=VolumeWeight), type = "column",
                  name = "Volume Traded",
                  color = 'blue', showInLegend = FALSE) %>%
  hc_yAxis(title = list(text = "Volume Traded"), allowDecimals = FALSE) %>%
  hc_xAxis(data =c$Strike,
             tickmarkPlacement = "on",
             opposite = FALSE) %>%
  hc_title(text = '2018-06-08',
             style = list(fontWeight = "bold")) %>%
  hc_subtitle(text = "Volume Traded Vs Strike Price") %>%
  hc_tooltip(valueDecimals = 0,
               pointFormat = "Percent: {point.z:.2f} <br> Volume: {point.y}") %>%
  hc_add_theme(hc_theme_538())
}




