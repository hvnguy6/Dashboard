library(quantmod)
library(fBasics)
library(dplyr)
library(highcharter)
library(scales)
library(Quandl)
library(dygraphs)

Quandl.api_key('EcxndBKxcnFeh_sK_yB_')

## SPX vs. VIX lower bound and VXST upper data
Load_SPX_VS_data <- function(some.start.date, some.end.date) {
        my.start.date <- some.start.date
        my.end.date <- some.end.date

        ## SPX VS VIX lower bound and VXST Upper
        # spx.data <- stock.raw.data('^GSPC', '2017-01-01', Sys.Date())
        # UVXY.data <<- stock.raw.data('UVXY', '2017-01-01', Sys.Date())
        spx.data <- stock.raw.data('^GSPC', my.start.date, my.end.date)
        UVXY.data <<- stock.raw.data('UVXY', my.start.date, my.end.date)
        
        
        vix.data <- Quandl('CBOE/VIX')
        vix.data <- as.xts((vix.data$'VIX Close') /(252^.5), order.by = vix.data$Date)
        
        vxst.data <- Quandl('CBOE/VXST') 
        vxst.data <- as.xts(vxst.data$Close /(252^.5), order.by = vxst.data$Date)
        
        colnames(vix.data)[1] <- 'VIX_1STD'
        colnames(vxst.data)[1] <- 'VXST_1STD'
                
        spx.data.close <- as.xts((spx.data$GSPC.Adjusted))
        
        spx.data.close <-merge(spx.data.close, vix.data, join='inner')
        spx.data.close <-merge(spx.data.close, vxst.data, join='inner')
        
        up.data.1std <-lag(spx.data.close$GSPC.Adjusted, k=-1) * (1+ lag(spx.data.close$VXST_1STD, k =-1)/100)
        up.data.2std <-lag(spx.data.close$GSPC.Adjusted, k=-1) * (1+ 2*lag(spx.data.close$VXST_1STD, k =-1)/100)
        
        down.data.1std <-lag(spx.data.close$GSPC.Adjusted, k=-1) * (1- lag(spx.data.close$VIX_1STD, k =-1)/100)
        down.data.2std <-lag(spx.data.close$GSPC.Adjusted, k=-1) * (1- 2 *lag(spx.data.close$VIX_1STD, k =-1)/100)
        
        spx.data.close <-merge(spx.data.close, up.data.1std, join = 'inner' )
        spx.data.close <-merge(spx.data.close, up.data.2std, join = 'inner' )
        spx.data.close <-merge(spx.data.close, down.data.1std, join = 'inner' )
        spx.data.close <<-merge(spx.data.close, down.data.2std, join = 'inner' )
        #return(spx.data.close)
}

Load_SPX_VS_data('2018-01-01', Sys.Date())



## Plots
colors_pallete <- c('red', 'darkgreen', 'goldenrod', 'darkblue','darkviolet')
SPX_VS_graph <- function() {
        # plot(x = spx.data.close$GSPC.Adjusted, xlab = "Time", ylab = "Index",
        #      main = "SPX vs VIX lower bound and VXST upper", major.ticks= "months",
        #      minor.ticks = FALSE, col = "red")
        # lines(x = spx.data.close$GSPC.Adjusted.1, col = "darkgreen")
        # lines(x = spx.data.close$GSPC.Adjusted.2, col = "goldenrod")
        # lines(x = spx.data.close$GSPC.Adjusted.3, col = "darkblue")
        # lines(x = spx.data.close$GSPC.Adjusted.4, col = "darkviolet")
        spx.data.close[,c(-2,-3)] %>%
        dygraph(main = "SPX vs VIX lower bound and VXST upper") %>%
        dyAxis("y", label = "Index") %>%
        dyHighlight(hideOnMouseOut = T,
                    highlightSeriesBackgroundAlpha = 0.15) %>%
        dyOptions(labelsKMB = T,
                  gridLineColor = "lightgrey",
                  digitsAfterDecimal = 2,
                  colors = colors_pallete,
                  rightGap = 20,
                  strokeWidth = 3) %>%
        dyRangeSelector(dateWindow = c(Sys.Date() - 90, Sys.Date())) %>%
        dyLegend(show = "follow",
                 width = 200,
                 hideOnMouseOut = T,
                 labelsSeparateLines = F)
}

SPX_VS_graph()




UVXY_graph <- function() {
        # plot(x = UVXY.data$UVXY.Adjusted, xlab = "Time", ylab = "$$$",
        #      main = "UVXY", major.ticks= "months",
        #      minor.ticks = FALSE, col = "red")
        UVXY.data$UVXY.Adjusted %>%
                dygraph(main = "UVXY") %>%
                dyAxis("y", label = "$$$") %>%
                dyHighlight(hideOnMouseOut = T,
                            highlightSeriesBackgroundAlpha = 0.15) %>%
                dyOptions(labelsKMB = T,
                          gridLineColor = "lightgrey",
                          digitsAfterDecimal = 2,
                          colors = "red",
                          rightGap = 20,
                          strokeWidth = 3) %>%
                dyRangeSelector(dateWindow = c(Sys.Date() - 90, Sys.Date())) %>%
                dyLegend(show = "follow",
                         width = 100,
                         hideOnMouseOut = T,
                         labelsSeparateLines = F) 
}

#UVXY_graph()






