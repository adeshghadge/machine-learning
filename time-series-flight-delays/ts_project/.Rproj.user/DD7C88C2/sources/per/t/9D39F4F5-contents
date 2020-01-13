attach(flight.delay.data)
flights.ord.lga <- flight.delay.data[which(ORIGIN_AIRPORT=='ORD' & DESTINATION_AIRPORT=='DFW'),]
head(flights.ord.lga)
dim(flights.ord.lga)

yearly.data.avg <- aggregate(flights.ord.lga$DEPARTURE_DELAY, by=list(Airline = flights.ord.lga$AIRLINE, Year = flights.ord.lga$YEAR, Month = flights.ord.lga$MONTH, Day = flights.ord.lga$DAY), FUN=mean)
head(yearly.data.avg)

yearly.data.avg$DelayDate <- as.Date(paste(yearly.data.avg$Year,yearly.data.avg$Month,yearly.data.avg$Day, sep="-"))
yearly.data.avg <- yearly.data.avg[order(yearly.data.avg$DelayDate),]
names(yearly.data.avg)[5] <- "Delay"
yearly.data.avg <- yearly.data.avg[, -c(2,3,4)]
head(yearly.data.avg)

yearly.data.avg[which(yearly.data.avg$Airline == 'AS'),]

as.data.frame(table(yearly.data.avg$Airline))

flight.delay.data[which(flights.ord.lga$MONTH == 10),]

as.data.frame(table(flight.delay.data$ORIGIN_AIRPORT))

airline.yearly.ts.avg <- split(yearly.data.avg, as.character(yearly.data.avg$Airline))


airline.yearly.ts.avg$AA[which(month(airline.yearly.ts.avg$AA$DelayDate) == 10),]

#Create time series using xts package
df.xts <- xts(airline.yearly.ts.avg$AA$Delay, start = c(2015,1), order.by = airline.yearly.ts.avg$AA$DelayDate)
dim(df.xts)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01", tz='America/Chicago') #Add date column used in merge
df.xts$date

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01"), to = as.POSIXct("2015-12-31"), by="day"))
all.dates.df$date <- as.POSIXct(as.character(as.Date(all.dates.df$date)), origin="1970-01-01",  tz= 'America/Chicago')
all.dates.df$date
dim(all.dates.df)

library(lubridate)

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)

  
df.agg[which(df.agg$delay == 0),]$delay <- NA

df.agg[which(is.na(df.agg$delay)),]

dim(df.agg)
dim(df.agg[which(is.na(df.agg$delay)),])

library(imputeTS)
  
df.agg$delay[which(df.agg$date < as.POSIXct('2015-01-15 00:00'))]
dim(df.agg[which(is.na(df.agg$delay)),])


plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))

yearly.ord.lga.data.avg <- na.interpolation(df.agg[which(df.agg$date <= as.POSIXct('2015-08-31')),]$delay, option = "spline")
head(yearly.ord.lga.data.avg)

yearly.ord.lga.test.data.avg <- na.interpolation(df.agg[which(df.agg$date >= as.POSIXct('2015-09-1') & df.agg$date <= as.POSIXct('2015-09-30')),]$delay, option = "spline")



library(xts)
library(tseries)
library(TSA)
library(forecast)
library(Metrics)




airline.yearly.ts.avg <- ts(yearly.ord.lga.data.avg, frequency = 7)
head(airline.yearly.ts.avg)

airline.ts.test.avg <- ts(yearly.ord.lga.test.data.avg, start=c(35,6), frequency = 7)
head(airline.ts.test.avg)

plot(decompose(airline.yearly.ts.avg))

acf(airline.yearly.ts.avg, lag = 150)
pacf(airline.yearly.ts.avg, lag = 150)
eacf(airline.yearly.ts.avg)

ndiffs(airline.yearly.ts.avg)

adf.test(airline.yearly.ts.avg)
kpss.test(airline.yearly.ts.avg)




#-----------------------------------------------------------------------------------------------------------------------------------
# ARIMA Model

mdl1 <- auto.arima(airline.yearly.ts.avg, seasonal = T)
mdl1

pred.mdl1 <- forecast(mdl1, h= 60)
pred.mdl1

autoplot(pred.mdl1) + autolayer(airline.ts.test.avg)

forecast::accuracy(airline.ts.test.avg, pred.mdl1$mean)
smape(airline.ts.test.avg, pred.mdl1$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#TBATS model

mdl.tbats <- tbats(airline.yearly.ts.avg)
plot(forecast(mdl.tbats))

airline.yearly.ts.avg.ft <- ts(yearly.ord.lga.data.avg, frequency = 365)
airline.ts.test.avg.ft <- ts(yearly.ord.lga.test.data.avg, start=c(1,273), frequency = 365)

mdl.ft <- auto.arima(airline.yearly.ts.avg, seasonal=FALSE, xreg=fourier(airline.yearly.ts.avg, K=3))
mdl.ft

pred.mdl.ft <- forecast(mdl.ft, xreg = fourier(airline.ts.test.avg, K=3),  h=365)
pred.mdl.ft

autoplot(pred.mdl.ft) + autolayer(airline.ts.test.avg)

forecast::accuracy(airline.ts.test.avg, pred.mdl.ft$mean)
smape(airline.ts.test.avg, pred.mdl.ft$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#ARIMA Model with Fourier terms

mdl.custom.ft <- Arima(airline.yearly.ts.avg, order=c(4,1,4), xreg=fourier(airline.yearly.ts.avg, K=3))
mdl.custom.ft


pred.mdl.custom.ft <- forecast(mdl.custom.ft, xreg = fourier(airline.ts.test.avg, K=3),  h=365)
pred.mdl.custom.ft

autoplot(pred.mdl.custom.ft) + autolayer(airline.ts.test.avg)

forecast::accuracy(airline.ts.test.avg, pred.mdl.ft$mean)
smape(airline.ts.test.avg, pred.mdl.ft$mean)

forecast::accuracy(mdl.custom.ft)


ft.data <- ts(airline.data.avg$WN$Delay, 
              freq=7)
ft.data

ft.ms.ts.data <- msts(airline.yearly.ts.avg, seasonal.periods = c(7, 30.4, 365.25))
ft.ms.ts.data

ft.ms.test.ts.data <- msts(airline.ts.test.avg, start=c(1.915,1), seasonal.periods = c(7, 30.4, 365.25))
ft.ms.test.ts.data

kpss.test(ft.ms.ts.data)
ndiffs(ft.ms.ts.data)

bestfit <- list(aicc=Inf)

for(i in 1:3)
{
  for(j in 1:14) {
    for(k in 1:25) {
      fit <- Arima(ft.ms.ts.data, xreg=fourier(ft.ms.ts.data, K=c(i,j,k)), order = c(4,1,4))
      if(fit$aicc < bestfit$aicc)
        bestfit <- fit
      else break;
    }
  }
}

fc <- forecast(bestfit, xreg=fourier(ft.ms.test.ts.data, K=c(1,1,2), h=365))
autoplot(fc) + autolayer(ft.ms.test.ts.data)

forecast::accuracy(fc, ft.ms.test.ts.data)

#-----------------------------------------------------------------------------------------------------------------------------------

