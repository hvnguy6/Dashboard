#Test on FB Data
library(quantmod)
library(fBasics)
library(dplyr)
library(highcharter)
library(scales)


#getSymbols("FB", from="2018-01-01", to=Sys.Date())
#FB.1yr_rtn=diff(log(FB$FB.Adjusted))

#k <- getSymbols("FB", from="2016-01-01", to=Sys.Date(), env = NULL)
#k[, paste0("WB", ".Adjusted")]

#generating a histogram of the return
#hist(FB.1yr_rtn, nclass=60)

#SymbolLists = c("AAPL","AMD", "AMZN", "ATVI", "BA", "BABA", "BAC", "COST", "CRM", "SPLK", "FB", "GOOGL", "GS", "NVDA", "NFLX", "INTC", "MSFT", 
#                "XLK", "TSLA","JD",  "QQQ", "WB", "JPM", "XLF", "WMT", "NKE", "OSTK", "MU")
#SymbolLists <- sort(SymbolLists)

options(scipen=999) #Could be removed when we work on data visulization



############################################################
### Daily return and volume
############################################################

### Raw Data
stock.raw.data <- function(some.stock, some.start.date, some.end.date) {
        
        my.stock <<- some.stock
        my.start.date <<- some.start.date
        my.end.date <<- some.end.date
        
        raw_xts <- getSymbols(my.stock, from = my.start.date, to = my.end.date +1, env = NULL)
        
        #        if (my.end.date==Sys.Date()) {
        #get the last quoted price and add that to the XTS frame
        #                raw_quote <- getQuote(my.stock)
        #                qCols <- c("Open","High","Low","Last","Volume","Last")
        #                qx <- xts(raw_quote[,qCols], as.Date(raw_quote[,"Trade Time"]))
        #                raw_xts <- rbind(raw_xts, qx) }
        
        return(raw_xts) 
}

t <- stock.raw.data("FB", "2017-01-01", Sys.Date())


### Distribution of daily return - Default is 251 Days
data.dist_daily_return <- function(some.raw.data) {
        diff(log(some.raw.data[, paste0(my.stock, ".Adjusted")]))
        
}
#tt <- data.dist_daily_return(t)

graph.dist_daily_return <- function(some.graph.data) {
        my.graph.data <- some.graph.data 
        hist(my.graph.data, xlim=c(min(my.graph.data, na.rm = TRUE), max(my.graph.data, na.rm = TRUE)), 
             nclass=60, main="Distribution of daily return", xlab="Daily Return")
        abline(v = mean(my.graph.data, na.rm =TRUE), col = "royalblue", lwd = 2)
        #        abline(v = mean(my.graph.data, na.rm =TRUE)-stdev(my.graph.data, na.rm =TRUE), col = "red", lwd = 2)
        #        abline(v = mean(my.graph.data, na.rm =TRUE)+stdev(my.graph.data, na.rm =TRUE), col = "red", lwd = 2)
        segments(mean(my.graph.data, na.rm =TRUE)-stdev(my.graph.data, na.rm =TRUE), 3, 
                 mean(my.graph.data, na.rm =TRUE)+stdev(my.graph.data, na.rm =TRUE), 3, col="red", lwd = 2 )
        
}
#graph.dist_daily_return(tt)


### Distribution of daily volume - Default is 251 Days
data.dist_daily_volume <- function(some.raw.data) {
        volume.xts <- some.raw.data[, paste0(my.stock, ".Volume")]
        volume.df <- as.data.frame(volume.xts)
}
#tt <- data.dist_daily_volume(t)

graph.dist_daily_volume <- function(some.graph.data) {
        my.graph.data <- some.graph.data 
        hist(my.graph.data[, paste0(my.stock, ".Volume")], xlim=c(min(my.graph.data[, paste0(my.stock, ".Volume")]), 
            max(my.graph.data[, paste0(my.stock, ".Volume")])), nclass=60,  main="Distribution of daily volume", xlab="Daily Volume", xaxt="n" )
        axis(side=1, at=axTicks(1),labels=formatC(axTicks(1), format="d", big.mark=','))
        abline(v = mean(my.graph.data[, paste0(my.stock, ".Volume")]), col = "royalblue", lwd = 2)
        #        abline(v = mean(my.graph.data[, paste0(my.stock, ".Volume")])-stdev(my.graph.data[, paste0(my.stock, ".Volume")]), col = "red", lwd = 2)
        #        abline(v = mean(my.graph.data[, paste0(my.stock, ".Volume")])+stdev(my.graph.data[, paste0(my.stock, ".Volume")]), col = "red", lwd = 2)
        segments(mean(my.graph.data[, paste0(my.stock, ".Volume")])-stdev(my.graph.data[, paste0(my.stock, ".Volume")]), 3, 
                 mean(my.graph.data[, paste0(my.stock, ".Volume")])+stdev(my.graph.data[, paste0(my.stock, ".Volume")]), 3, col="red", lwd = 2 )
        
        
}
#graph.dist_daily_volume(tt)


### Volume and Adjusted Price in one chart

# j <- as.data.frame(t)[, 5:6]
# j$Date <- rownames(j)
# highchart() %>%
#         hc_xAxis(categories = j$Date) %>%
#         hc_yAxis_multiples(
#                 list(lineWidth = 3),
#                 list(showLastLabel = F, opposite = TRUE)
#         ) %>%
#         hc_add_series(name = "Volume", data = j$FB.Volume, type = "column") %>%
#         hc_add_series(name = "Adjusted Price",data = j$FB.Adjusted, type = "line", yAxis = 1)


graph.volume_adjustedPrice <- function(some.raw.data) {
        my.raw.data <- some.raw.data
        j <- as.data.frame(my.raw.data)[, 5:6]
        j$Date <- rownames(j)
        highchart() %>% 
                hc_title( text = paste0(my.stock, " Volume and Adjusted Price")) %>%
                hc_xAxis(categories = j$Date) %>%
                hc_yAxis_multiples(
                        list(lineWidth = 3),
                        list(showLastLabel = F, opposite = TRUE)
                ) %>% 
                hc_add_series(name = "Volume", data = j[, paste0(my.stock, ".Volume")], type = "column", yAxis = 1) %>% 
                hc_add_series(name = "Adjusted Price",data = j[, paste0(my.stock, ".Adjusted")], type = "line")
}
#graph.volume_adjustedPrice(t)




### Summary on selected days
data.stats_summary <-function(some.raw.data) {
        data.daily_return<-diff(log(some.raw.data[, paste0(my.stock, ".Adjusted")]))
        data.daily_return_summary<-basicStats(data.daily_return)
        data.daily_return_summary<-rbind(data.daily_return_summary, "LastValue" = last(data.daily_return))
        
        data.daily_volume<-some.raw.data[, paste0(my.stock, ".Volume")]
        data.daily_volume_summary<-basicStats(data.daily_volume)
        data.daily_volume_summary<-rbind(data.daily_volume_summary, "LastValue" = last(data.daily_volume))
        data.daily_volume_summary<-format(data.daily_volume_summary, trim=TRUE, big.mark = ",") 
        
        data.daily_combined_summary <- cbind(data.daily_return_summary,data.daily_volume_summary)
        data.daily_combined_summary <- data.daily_combined_summary[-c(1, 2, 5, 6, 9, 10, 11, 12, 13),,drop=F]
        
        return(data.daily_combined_summary)
}
data.stats_summary(t)

