library('data.table')
library('scales')

callPath <-'/home/ubuntu/csvFiles/calls/'
putPath <- '/home/ubuntu/csvFiles/puts/'

callFiles<-list.files(callPath)
putFiles<- list.files(putPath)

print(callFiles)

#loop through each call files here replace 1 with i

xPath = paste(callPath, callFiles[8], sep='')
#xPath =paste(putPath, callFiles[21], sep='')


xFile = fread(xPath, header =TRUE, sep = ',')

TotalVolume <-xFile[, list(TotalV = sum(Vol)), by =ExpirationDate]

setkey(TotalVolume, ExpirationDate)
setkey(xFile, ExpirationDate)
xFile <-merge(xFile, TotalVolume, all.x = TRUE)

xFile$VolumeWeight <-xFile$Vol/xFile$TotalV

xFile$DailyImpliedVolatilityWeighted <- xFile$VolumeWeight*(xFile$ImpliedVol/(252^.5))
WeightedVol <- xFile[, list(avgWghtIV=sum(DailyImpliedVolatilityWeighted)), by = ExpirationDate]
setkey(WeightedVol, ExpirationDate)

xFile <-merge(xFile, WeightedVol, all.x = TRUE)

xFile[, list(dailyVolume = sum(Vol), avgBEPrice = mean(BreakEvenStockPrice), avgDailyGrowth = mean(ImpliedStockGrowth_daily), avgWeightedImpliedVol = percent(mean(avgWghtIV)), YrHV = percent(mean(U_Vol1year/(252^.5))), QuarterHV=percent(mean(U_Vol45days/(252^.5))), lastUnderlyingTraded = mean(U_LastTraded) ), by = ExpirationDate]


#loop through each expiration date
b<-subset(xFile, xFile$ExpirationDate == '2018-06-15')

par(mfrow=c(3,1))
plot((b$ImpliedVol)/(252^.5) ~b$Strike)
lines(b$avgWghtIV ~ b$Strike, col='red') 
lines((b$U_Vol45days)/(252^.5) ~ b$Strike, col ='blue')

plot((b$VolumeWeight ~b$Strike))

plot((b$BreakEvenStockPrice ~b$Strike))
lines((b$U_LastTraded ~ b$Strike), col='black')



