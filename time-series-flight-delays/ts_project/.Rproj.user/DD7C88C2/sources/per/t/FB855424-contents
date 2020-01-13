
dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.data <- read.csv(file=paste0(dataPath, "flight_data_2015.csv"), header = TRUE)
head(flight.delay.data)


dim(flight.delay.data)
cbind(names(flight.delay.data))

summary(flight.delay.data$DEP_DELAY)
summary(flight.delay.data$ARRIVAL_DELAY)

describe(flight.delay.data$DEP_DELAY) 
describe(flight.delay.data$ARRIVAL_DELAY) 

library(corrplot)

flight.numerica.data <- flight.delay.data[, c(12,13)]
flight.numerica.data[is.na(flight.numerica.data)] <- 0
head(flight.numerica.data)
corr.flight.data <- cor(flight.numerica.data)
head(corr.flight.data)

corrplot(corr.flight.data, method="number")

lm.mdl <- lm(DEP_DELAY~., flight.numerica.data)
summary(lm.mdl)

#Remove NAs and negative values
#flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY > 0),]


#Remove NAs
flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY != 0 & flight.delay.data$DEP_DELAY <= 90),]
head(flight.delay.data)
dim(flight.delay.data)

flight.delay.test.data <- flight.delay.data.2016

flight.delay.test.data <- flight.delay.test.data[which(is.na(flight.delay.test.data$DEP_DELAY) == FALSE & flight.delay.test.data$DEP_DELAY != 0 & flight.delay.test.data$DEP_DELAY <= 90),]
head(flight.delay.test.data)
dim(flight.delay.test.data)

# Flight Frequency by Airline
library(plyr)
flights.count.by.airline <- count(flight.delay.data, "OP_UNIQUE_CARRIER")
flights.count.by.airline <- flights.count.by.airline[order(flights.count.by.airline$freq, decreasing = T),]
flights.count.by.airline

flights.test.count.by.airline <- count(flight.delay.test.data, "OP_UNIQUE_CARRIER")
flights.test.count.by.airline <- flights.test.count.by.airline[order(flights.test.count.by.airline$freq, decreasing = T),]
flights.test.count.by.airline

library(ggplot2)

bp<- ggplot(flights.count.by.airline[c(1:10),], aes(x="", y=freq, fill=OP_UNIQUE_CARRIER))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

unique(flight.delay.data$OP_UNIQUE_CARRIER)


#Min Delays
flight.delay.data.min <- aggregate(flight.delay.data$DEP_DELAY, by=list(Airline = flight.delay.data$OP_UNIQUE_CARRIER, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY_OF_MONTH), FUN=min)
head(flight.delay.data.min)

flight.delay.data.min$DelayDate <- as.Date(paste(flight.delay.data.min$Year,flight.delay.data.min$Month,flight.delay.data.min$Day, sep="-"))
flight.delay.data.min <- flight.delay.data.min[order(flight.delay.data.min$DelayDate),]
names(flight.delay.data.min)[5] <- "Delay"
flight.delay.data.min <- flight.delay.data.min[, -c(2,3,4)]

#Max Delays
flight.delay.data.max <- aggregate(flight.delay.data$DEP_DELAY, by=list(Airline = flight.delay.data$OP_UNIQUE_CARRIER, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=max)
head(flight.delay.data.max)

flight.delay.data.max$DelayDate <- as.Date(paste(flight.delay.data.max$Year,flight.delay.data.max$Month,flight.delay.data.max$Day, sep="-"))
flight.delay.data.max <- flight.delay.data.max[order(flight.delay.data.max$DelayDate),]
names(flight.delay.data.max)[5] <- "Delay"
flight.delay.data.max <- flight.delay.data.max[, -c(2,3,4)]

#Median Delays
flight.delay.data.mdn <- aggregate(flight.delay.data$DEP_DELAY, by=list(Airline = flight.delay.data$OP_UNIQUE_CARRIER, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=median)
head(flight.delay.data.mdn)

flight.delay.data.mdn$DelayDate <- as.Date(paste(flight.delay.data.mdn$Year,flight.delay.data.mdn$Month,flight.delay.data.mdn$Day, sep="-"))
flight.delay.data.mdn <- flight.delay.data.mdn[order(flight.delay.data.mdn$DelayDate),]
names(flight.delay.data.mdn)[5] <- "Delay"
flight.delay.data.mdn <- flight.delay.data.mdn[, -c(2,3,4)]
head(flight.delay.data.mdn)



#Avg Delays
flight.delay.data.avg <- aggregate(flight.delay.data$DEP_DELAY, by=list(Airline = flight.delay.data$OP_UNIQUE_CARRIER, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY_OF_MONTH), FUN=mean)
head(flight.delay.data.avg)

flight.delay.data.avg$DelayDate <- as.Date(paste(flight.delay.data.avg$Year,flight.delay.data.avg$Month,flight.delay.data.avg$Day, sep="-"))
flight.delay.data.avg <- flight.delay.data.avg[order(flight.delay.data.avg$DelayDate),]
names(flight.delay.data.avg)[5] <- "Delay"
flight.delay.data.avg <- flight.delay.data.avg[, -c(2,3,4)]
head(flight.delay.data.avg)


flight.delay.test.data.avg <- aggregate(flight.delay.test.data$DEP_DELAY, by=list(Airline = flight.delay.test.data$OP_UNIQUE_CARRIER, Year = flight.delay.test.data$YEAR, Month = flight.delay.test.data$MONTH, Day = flight.delay.test.data$DAY_OF_MONTH), FUN=mean)
head(flight.delay.test.data.avg)

flight.delay.test.data.avg$DelayDate <- as.Date(paste(flight.delay.test.data.avg$Year,flight.delay.test.data.avg$Month,flight.delay.test.data.avg$Day, sep="-"))
flight.delay.test.data.avg <- flight.delay.test.data.avg[order(flight.delay.test.data.avg$DelayDate),]
names(flight.delay.test.data.avg)[5] <- "Delay"
flight.delay.test.data.avg <- flight.delay.test.data.avg[, -c(2,3,4)]
head(flight.delay.test.data.avg)



#Split data per airline - 365 rows per airline
airline.data.min <- split(flight.delay.data.min, flight.delay.data.min$Airline)
airline.data.max <- split(flight.delay.data.max, flight.delay.data.max$Airline)
airline.data.mdn <- split(flight.delay.data.mdn, flight.delay.data.mdn$Airline)
airline.data.avg <- split(flight.delay.data.avg, flight.delay.data.avg$Airline)

airline.test.data.avg <- split(flight.delay.test.data.avg, flight.delay.test.data.avg$Airline)




calcAICc.seasonal <- function(vars) {
  p <- vars[1L]
  d <- vars[2L]
  q <- vars[3L]
  
  P <- vars[4L]
  D <- vars[5L]
  Q <- vars[6L]
  
  mdl.data <- vars[7L]
  
  try({
    mdl <- Arima(mdl.data, order = c(p,d,q), seasonal = c(P, D, Q))
    mdl$aicc
  })
  
}

calcBIC.seasonal <- function(vars) {
  p <- vars[1L]
  d <- vars[2L]
  q <- vars[3L]
  
  P <- vars[4L]
  D <- vars[5L]
  Q <- vars[6L]
  
  mdl.data <- vars[7L]
  
  try({
    mdl <- Arima(mdl.data, order = c(p,d,q), seasonal = c(P, D, Q), pe)
    mdl$bic
  })
  
}



library(xts)
library(tseries)
library(TSA)
library(forecast)
library(Metrics)

for (airline.data in airline.data.avg){
  print(nrow(airline.data))
}


runModelForAirlines <- function(data) {
  
  results.df <- NULL
  
  lapply(data, function(x) {
    
    if(nrow(x) == 365) {
      ts.data <- ts(subset(x, DelayDate < '2015-12-01')$Delay, frequency = 7)
      
      ts.test.data <- ts(subset(x, DelayDate > '2015-12-01')$Delay, start=c(48,6), frequency = 7)
      
      mdl <- auto.arima(ts.data, seasonal = T)
      
      pred.mdl <- forecast(mdl1, h= 31)
      
      autoplot(pred.mdl) + autolayer(ts.test.data)
      #name = deparse(substitute(x))
      
      results.df <- rbind(results.df, cbind("ME" = forecast::accuracy(ts.test.data, pred.mdl$mean)[1,1]))
      #smape(airline.ts.test.avg, pred.mdl1$mean)
      }
  })
  print(results.df)
  return(results.df)
}

accuracy.df <- runModelForAirlines(airline.data.avg)
accuracy.df

airline.ts.avg <- ts(airline.data.avg$AA$Delay, frequency = 7)
head(airline.ts.avg)

airline.ts.test.avg <- ts(airline.test.data.avg$AA$Delay, start=c(53,2), frequency = 7)
head(airline.ts.test.avg)

plot(decompose(airline.ts.avg))

acf(airline.ts.avg, lag = 40)
pacf(airline.ts.avg, lag = 40)
eacf(airline.ts.avg)

ndiffs(airline.ts.avg)

acf(diff(airline.ts.avg), lag = 400)
pacf(diff(airline.ts.avg), lag = 400)
eacf(diff(airline.ts.avg))


adf.test(airline.ts.avg)
kpss.test(airline.ts.avg)


#-----------------------------------------------------------------------------------------------------------------------------------
# ARIMA Model

arima.mdl.yearly <- auto.arima(airline.ts.avg, seasonal = T, stepwise = F, max.order = 8)
arima.mdl.yearly

pred.yearly.mdl <- forecast(arima.mdl.yearly, h= 365)
pred.yearly.mdl

autoplot(pred.yearly.mdl) + autolayer(airline.ts.test.avg)

forecast::accuracy(pred.yearly.mdl, airline.ts.test.avg)
smape(airline.ts.test.avg, pred.yearly.mdl$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#TBATS model

mdl.tbats <- tbats(airline.ts.avg)
plot(forecast(mdl.tbats))

airline.ts.avg.ft <- ts(airline.data.avg$AA$Delay, frequency = 7)
airline.ts.test.avg.ft <- ts(airline.data.avg$AA$Delay, start=c(53,2), frequency = 7)

mdl.ft <- auto.arima(airline.ts.avg, seasonal=FALSE, xreg=fourier(airline.ts.avg, K=3))
mdl.ft

pred.mdl.ft <- forecast(mdl.ft, xreg = fourier(airline.ts.test.avg, K=3),  h=7)
pred.mdl.ft

autoplot(pred.mdl.ft) + autolayer(airline.ts.test.avg)

forecast::accuracy(pred.mdl.ft, airline.ts.test.avg)
smape(airline.ts.test.avg, pred.mdl.ft$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#ARIMA Model with Fourier terms

mdl.custom.ft <- Arima(airline.ts.avg, order=c(4,1,4), xreg=fourier(airline.ts.avg, K=3))
mdl.custom.ft


pred.mdl.custom.ft <- forecast(mdl.custom.ft, xreg = fourier(airline.ts.test.avg, K=3),  h=365)
pred.mdl.custom.ft

autoplot(pred.mdl.custom.ft) + autolayer(airline.ts.test.avg)

forecast::accuracy(pred.mdl.ft, airline.ts.test.avg)
smape(airline.ts.test.avg, pred.mdl.ft$mean)



ft.data <- ts(airline.data.avg$AA$Delay, freq=7)
ft.data

ft.ms.ts.data <- msts(airline.data.avg$AA$Delay, seasonal.periods = c(7, 30.4, 365.25))
ft.ms.ts.data

ft.ms.test.ts.data <- msts(airline.data.avg$AA$Delay, start=c(2,1), seasonal.periods = c(7, 30.4, 365.25))
ft.ms.test.ts.data

kpss.test(ft.ms.ts.data)
ndiffs(ft.ms.ts.data)

bestfit <- list(aicc=Inf)

for(i in 1:3)
{
  for(j in 1:14) {
    for(k in 1:25) {
      fit <- Arima(ft.ms.ts.data, xreg=fourier(ft.ms.ts.data, K=c(i,j,k)), order = c(4,1,4))
      if(fit$aicc < bestfit$aicc) {
        bestfit <- fit
        best.f.terms <- cbind("i"=i, "j"=j, "k"=k)
      }
      else break;
    }
  }
}

fc <- forecast(bestfit, xreg=fourier(ft.ms.test.ts.data, K=c(2,1,1), h=365))
autoplot(fc) + autolayer(ft.ms.test.ts.data)

bestfit

forecast::accuracy(fc, ft.ms.test.ts.data)
smape(ft.ms.test.ts.data, fc$mean)

#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
# Running Median 

airline.data.avg.run.med <- runmed(airline.data.avg$AA$Delay, k = 31)

airline.ts.run.med <- ts(airline.data.avg.run.med[1:334], start=c(1,1), end=c(48,5), frequency = 7)
airline.ts.test.run.med <- ts(airline.data.avg.run.med[335:365], start= c(48, 6), frequency = 7)

plot(decompose(airline.ts.run.med))

mdl.run.med <- auto.arima(airline.ts.run.med, seasonal = T)
mdl.run.med

pred.run.med <- forecast(mdl.run.med, h = 365)
pred.run.med

autoplot(pred.run.med) + autolayer(airline.ts.test.run.med)

forecast::accuracy(airline.ts.test.run.med, pred.run.med$mean)
smape(airline.ts.test.run.med, pred.run.med$mean)
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#Running Mean

airline.data.avg.run.mean <- runMean(airline.data.avg$AA$Delay, n = 61)

airline.ts.run.mean <- ts(airline.data.avg.run.mean[1:334], start=c(1,1), end=c(48,5), frequency = 7)
airline.ts.test.run.mean <- ts(airline.data.avg.run.mean[335:365], start= c(48, 6), frequency = 7)

plot(decompose(airline.ts.run.mean))

mdl.run.mean <- auto.arima(airline.ts.run.mean, seasonal = T)
mdl.run.mean

pred.run.mean <- forecast(mdl.run.mean, h = 365)
pred.run.mean

autoplot(pred.run.mean) + autolayer(airline.ts.test.run.mean)

forecast::accuracy(airline.ts.test.run.mean, pred.run.mean$mean)
smape(airline.ts.test.run.mean, pred.run.mean$mean)

#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------

#ARFIMA

suppressMessages(suppressWarnings(library(lmtest)))
suppressMessages(suppressWarnings(library(car)))
suppressMessages(suppressWarnings(library(pracma)))

hurstexp(airline.ts.avg)

mdl.af <- arfima(airline.ts.avg)
summary(mdl.af)

acf(mdl.af$residuals, lag=400)
pacf(mdl.af$residuals, lag=400)

qqnorm(mdl.af$residuals)
qqline(mdl.af$residuals)

library("ggpubr")
ggqqplot(mdl.af$residuals)

dwtest(lm(mdl.af$residuals~1), alternative="two.sided")
Box.test(mdl.af$residuals, lag = 5, type = "Ljung-Box", fitdf = 0)

plot(forecast(mdl.af, h = 365))

forecast::accuracy(forecast(mdl.af, h = 31), airline.ts.test.avg)

#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------

airline.ts.avg <- airline.data.avg$AA$Delay


aic_vals_temp <- NULL
aic_vals <- NULL

for(i in 1:3)
{
  for(j in 1:3)
  {
    xreg1 <- fourier(1:length(airline.ts.avg), K = i, h = 24)
    xreg2 <- fourier(1:length(airline.ts.avg), K = j, h = 24*7)
    xtrain <- cbind(xreg1, xreg2)
    fitma1 <- auto.arima(airline.ts.avg, seasonal = F, xreg = train)
    aic_vals_temp <- cbind(i,j, fitma1$aic)
    aic_vals <- rbind(aic_vals, aic_vals_temp)
  }
}

fourier(airline.ts.avg, K = 1, h = 24)

colnames(aic_vals) <- c("FourierTerms24", "FourierTerms168", "AICValue")
aic_vals <- data.frame(aic_vals)
minAICVal <- min(aic_vals$AICValue)
minvals <- aic_vals[which(aic_vals$AICValue == minAICVal),]
returnval <- c(minvals$FourierTerms24, minvals$FourierTerms168)
returnval

#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
y <- msts(airline.ts.avg, seasonal.periods = c(7,30.4, 365.25)) # multiseasonal ts
y.t <- msts(airline.ts.test.avg, c(7,30.4, 365.25), start=c(2,1)) # multiseasonal ts
fit <- auto.arima(y, seasonal=F, xreg=fourier(y, K=c(2,3,4)))

fit.tb <- tbats(y)
fit.tb

plot(fit.tb)

components <- tbats.components(fit.tb)
plot(components)

autoplot(forecast(fit.tb, h = 365)) + autolayer(y.t)

fit_f <- forecast(fit, xreg= fourier(y.t, K=c(2,3,4)), 365.25)
plot(fit_f)
autoplot(fit_f)+autolayer(y.t)

checkresiduals(fit_f$residuals)

forecast::accuracy(fit_f, y.t)
smape(y.t, fit_f$mean)
#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------
#Prophet Library

library(prophet)

prophet.data <-as.data.frame(cbind("ds" = airline.data.avg$AA$DelayDate, "y" = airline.data.avg$AA$Delay))
head(prophet.data)
prophet.data$ds <- as.Date(prophet.data$ds)

prophet.test.data <-as.data.frame(cbind("ds" = airline.test.data.avg$AA$DelayDate, "y" = airline.test.data.avg$AA$Delay))
head(prophet.test.data)
prophet.test.data$ds <- as.Date(prophet.test.data$ds)

m <- prophet(prophet.data, yearly.seasonality = T, daily.seasonality = T, weekly.seasonality = T)
m$seasonalities

m.test <- prophet(prophet.test.data, yearly.seasonality = T, daily.seasonality = T, weekly.seasonality = T)

future <- make_future_dataframe(m, periods = 365)
tail(future)

future.test <- make_future_dataframe(m.test, periods = 365)


forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m.test, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)


forecast.test <- predict(m, future.test)
tail(forecast.test[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast.test)


df.cv <- cross_validation(m, initial = 300, period = 180, horizon = 30, units = 'days')
head(df.cv)

df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mape')

#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#Holtz-Winter

fit.hw <- HoltWinters(airline.ts.avg)
summary(fit.hw)

pred.hw <- forecast(fit.hw, h =365)
pred.hw

autoplot(pred.hw) + autolayer(airline.ts.test.avg)

forecast::accuracy(pred.hw, airline.ts.test.avg)
smape(airline.ts.test.avg, pred.hw$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#GARCH

library(fGarch)
library(WeightedPortTest)

r.yearly.delay <- diff(log(100+airline.data.avg$AA$Delay))*100

McLeod.Li.test(y=r.yearly.delay)

yearly.garch.model <- garchFit(formula = ~ arma(0,80) + garch(1, 1), data = r.yearly.delay, cond.dist="std")

McLeod.Li.test(y=yearly.garch.model@residuals)

Weighted.LM.test(yearly.garch.model@residuals, yearly.garch.model@h.t, lag = 10)


checkresiduals(yearly.garch.model@residuals)

plot(yearly.garch.model)

pred.yearly.garch <- fGarch::predict(yearly.garch.model, n.ahead=4000, plot=TRUE)
pred.yearly.garch

plot(pred.yearly.garch)

checkresiduals(yearly.garch.model@residuals)

stresi.yearly = residuals(yearly.garch.model, standardize=T)
plot(stresi.yearly,type="l")
Box.test(stresi.yearly, 100, type="Ljung")



#-----------------------------------------------------------------------------------------------------------------------------------


fit.ets <- ets(airline.ts.avg)
plot(forecast(fit.ets, h=365))





airline.ts.monthly.avg <- ts(subset(airline.data.avg$AS, DelayDate < '2015-04-01')$Delay, frequency = 7)
head(airline.ts.monthly.avg)

airline.ts.monthly.test.avg <- ts(subset(airline.data.avg$AS, DelayDate >= '2015-04-01' & DelayDate < '2015-05-01')$Delay, start=c(13,7), frequency = 7)
head(airline.ts.monthly.test.avg)

plot(decompose(airline.ts.monthly.avg))

acf(airline.ts.monthly.avg)
pacf(airline.ts.monthly.avg)
eacf(airline.ts.monthly.avg)

ndiffs(airline.ts.monthly.avg)

mdl.monthly <- auto.arima(airline.ts.monthly.avg)
mdl.monthly

pred.monthly <- forecast(mdl.monthly, h=30)

autoplot(pred.monthly) + autolayer(airline.ts.monthly.test.avg)

forecast::accuracy(airline.ts.monthly.test.avg, pred.monthly$mean)

airline.ts.mdn <- ts(subset(airline.data.mdn$AS, DelayDate < '2015-12-01')$Delay, frequency = 7)
head(airline.ts.mdn)

airline.ts.test.mdn <- ts(subset(airline.data.mdn$AS, DelayDate >= '2015-12-01')$Delay, start=c(48,6), frequency = 7)
head(airline.ts.test.mdn)

plot(decompose(airline.ts.mdn))

acf(airline.ts.mdn, lag = 400)
pacf(airline.ts.mdn, lag = 400)
eacf(airline.ts.mdn)

ndiffs(airline.ts.mdn)

acf(diff(airline.ts.mdn), lag = 400)
pacf(diff(airline.ts.mdn), lag = 400)
eacf(diff(airline.ts.mdn))


adf.test(airline.ts.mdn)
kpss.test(airline.ts.mdn)


mdl.mdn <- auto.arima(airline.ts.mdn, seasonal = T)
mdl.mdn

pred.mdl.mdn <- forecast(mdl1, h= 31)

autoplot(pred.mdl.mdn) + autolayer(airline.ts.test.mdn)

forecast::accuracy(airline.ts.test.mdn, pred.mdl.mdn$mean)
smape(airline.ts.test.mdn, pred.mdl.mdn$mean)


airline.ts.mdn <- xts(airline.data.mdn$AA$Delay, order.by=airline.data.mdn$AA$DelayDate)
head(airline.ts.mdn)

acf(airline.ts.mdn, lag = 100)
pacf(airline.ts.mdn, lag = 100)
eacf(airline.ts.mdn)


adf.test(airline.ts.mdn)
kpss.test(airline.ts.mdn)

ndiffs(airline.ts.mdn)


auto.arima(airline.ts.mdn, seasonal = T)


airline.ts.avg <- ts(airline.data.avg$AA$Delay, frequency = 1)
head(airline.ts.avg)

plot(decompose(airline.ts.avg))

acf(airline.ts.avg, lag = 100)
pacf(airline.ts.avg, lag = 100)
eacf(airline.ts.avg)

m1 <- auto.arima(airline.ts.avg, seasonal = T)

pred.m1 <- forecast(m1, h=14)
autoplot(pred.m1)+autolayer(pred.m1)



library(NMOF)

aicc.seasonal.gs <- suppressMessages(gridSearch(fun = calcAICc.seasonal, levels = list(p = 1:3, d = 1, q = 1:3, P = 1:3, D = 0:1, Q=1:3, mdl.data = airline.ts.avg)))
bic.seasonal.gs <- suppressWarnings(suppressMessages(gridSearch(fun = calcBIC.seasonal, levels = list(p = 1:3, d = 1, q = 1:3, P = 1:3, D = 0:1, Q=1:3, mdl.data = airline.ts.avg))))

cbind("min AICc" = aicc.seasonal.gs$minfun, "min BIC" = bic.seasonal.gs$minfun)
cbind("min AICc p,q" = aicc.seasonal.gs$minlevels, "min BIC p,q" = bic.seasonal.gs$minlevels)

m1.bst <- Arima(airline.ts.avg, order = c(2,1,2), seasonal = c(1,1,1))

pred.m1.bst <- forecast(m1.bst, h = 14)
pred.m1.bst

autoplot(pred.m1.bst) + autolayer(pred.m1.bst)

head(flight.delay.data[, c(12, 13, 14,17,18)])

lm.mdl <- lm(DEP_DELAY~., flight.delay.data[, c(12, 18)])
summary(lm.mdl)





airline.ts.avg.weeks <- ts(subset(airline.data.avg$AA, DelayDate < '2015-4-1')$Delay, frequency = 7)

airline.test.ts.avg.weeks <- ts(subset(airline.data.avg$AA, DelayDate >= '2015-4-1' & DelayDate <= '2015-4-15')$Delay, start=c(13,7), frequency = 7)

plot(decompose(airline.ts.avg.weeks))

mdl.weeks <- auto.arima(airline.ts.avg.weeks, seasonal = T)
mdl.weeks

pred.mdl.weeks <- forecast(mdl.weeks, h = 14)
pred.mdl.weeks

autoplot(pred.mdl.weeks) + autolayer(airline.test.ts.avg.weeks)



aicc.seasonal.weeks <- suppressMessages(gridSearch(fun = calcAICc.seasonal, levels = list(p = 1:5, d = 1, q = 1:5, P = 1:3, D = 0:1, Q=1:3)))
bic.seasonal.weeks <- suppressWarnings(suppressMessages(gridSearch(fun = calcBIC.seasonal, levels = list(p = 1:5, d = 1, q = 1:5, P = 1:3, D = 0:1, Q=1:3))))

cbind("min AICc" = aicc.seasonal.weeks$minfun, "min BIC" = bic.seasonal.weeks$minfun)
cbind("min AICc p,q" = aicc.seasonal.weeks$minlevels, "min BIC p,q" = bic.seasonal.weeks$minlevels)

acf(airline.ts.avg.weeks, lag=100)

mdl.weeks.bst <- Arima(airline.ts.avg.weeks, order = c(1,1,1), seasonal = c(10,1,4))
mdl.weeks.bst

pred.mdl.weeks.bst <- forecast(mdl.weeks.bst, h = 14)

autoplot(pred.mdl.weeks.bst) + autolayer(airline.test.ts.avg.weeks)



library(lubridate)



dep.delay.ts.aa <- ts(airline.data$AA, start = c(2015, 1), frequency = 24)
head(dep.delay.ts.aa)
length(dep.delay.ts.aa)

library(zoo)

dep.daily.max.ts.aa <- zoo(aa.delays$x, order.by = aa.delays$DelayDate)



plot(decompose(dep.delay.ts.aa))


auto.arima(dep.daily.max.ts.aa)



names(flight.delay.data)

flight.tree.analysis.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE),]
flight.tree.analysis.data$FLIGHT_STATE <- NA
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEP_DELAY == 0] <- "ON TIME"
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEP_DELAY > 0] <- "DELAYED"
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEP_DELAY < 0] <- "BEFORE TIME"
flight.tree.analysis.data$FLIGHT_DATE <- as.Date(paste(flight.tree.analysis.data$YEAR,flight.tree.analysis.data$MONTH,flight.tree.analysis.data$DAY, sep="-"))

typeof(flight.tree.analysis.data)
names(flight.tree.analysis.data)

keep.cols <- c("FLIGHT_DATE", "FLIGHT_STATE", "DAY_OF_WEEK","OP_UNIQUE_CARRIER","FLIGHT_NUMBER","TAIL_NUMBER","ORIGIN_AIRPORT","SCHEDULED_DEPARTURE","DESTINATION_AIRPORT","DISTANCE")

flight.tree.analysis.data <- flight.tree.analysis.data[, (names(flight.tree.analysis.data) %in% keep.cols)]

head(flight.tree.analysis.data)

unique(flight.tree.analysis.data$CANCELLATION_REASON)

library(caret)

set.seed(1234)
trainIndex <- createDataPartition(flight.tree.analysis.data$FLIGHT_STATE, 
                                  p = .70, 
                                  list = FALSE, 
                                  times = 1)

flight.train <- flight.tree.analysis.data[trainIndex,]
flight.test <- flight.tree.analysis.data[-trainIndex,]

dim(flight.train)
dim(flight.test)

library(rpart)
library(rpart.plot)

tree.fit <- rpart(FLIGHT_STATE~., data = flight.train[,c(1,2,9)], method = 'class')
rpart.plot(tree.fit)

tree.Pred <- predict(tree.fit, new = flight.train[, c(1,2)], type="prob")
head(tree.Pred)

summary(tree.fit)

printcp(tree.fit)
plotcp(tree.fit)

suppressWarnings(library(randomForest))

flight.tree.analysis.data$FLIGHT_STATE <- as.factor(flight.tree.analysis.data$FLIGHT_STATE)
rf.tree.fit <- randomForest(FLIGHT_STATE~.,ntree=250, data=flight.tree.analysis.data, importance=TRUE)

importance(rf.tree.fit)

varImpPlot(rf.tree.fit, main="Variable Importance")

plot(rf.tree.fit)

replace(flight.tree.analysis.data, TRUE, lapply(flight.tree.analysis.data, factor))

apply(flight.tree.analysis.data, 2, function(x) factor(x))

rfPred <- predict(rf.tree.fit, new = flight.tree.analysis.data[, -9], type="prob")


unique(flight.tree.analysis.data$FLIGHT_DATE)


library(nnet)
multinomModel <- multinom(FLIGHT_STATE ~ ., data=flight.train) # multinom Model





library(dplyr)

flights.data.binom <- select(flight.delay.data, QUARTER, MONTH, DAY_OF_MONTH, DAY_OF_WEEK, OP_UNIQUE_CARRIER, TAIL_NUM, OP_CARRIER_FL_NUM, ORIGIN, DEST, DEP_DELAY, TAXI_OUT, WHEELS_OFF, AIR_TIME, DISTANCE)

flights.data.binom <- flights.data.binom[which(flights.data.binom$DEP_DELAY >= -90 & flights.data.binom$DEP_DELAY <= 90),]
dim(flights.data.binom)

flights.data.binom$DELAY_TYPE <- NA
flights.data.binom$DELAY_TYPE[which(flights.data.binom$DEP_DELAY <= 0)] <- 0
flights.data.binom$DELAY_TYPE[which(flights.data.binom$DEP_DELAY > 0)] <- 1
head(flights.data.binom)

head(flights.data.binom[,-c(10)])
unique(flights.data.binom$MONTH)

plot(flights.data.binom$DEP_DELAY)

log.flights.data <- log(flights.data.binom$DEP_DELAY + 1 - min(flights.data.binom$DEP_DELAY))


length(log.flights.data[which(log.flights.data > 4)])

hist(log.flights.data[which(log.flights.data > 4)])

log.flights.data <- log.flights.data[which(log.flights.data > 4)]

den.delay <- density(log.flights.data)
head(den.delay)
plot(den.delay)

qqnorm(den.delay)
qqline(den.delay)

library("ggpubr")
ggqqplot(flights.data.binom$DEP_DELAY)

ks.test(unique(log.flights.data), y = "pnorm")

head(flights.data.binom[,-c(2,3,6,7,9,10,11,12)])

glm.delay <- glm(DELAY_TYPE~., data=flights.data.binom[,-c(2,3,6,7,8,9,10,11,12)], family = binomial(link = "logit"))
summary(glm.delay)
