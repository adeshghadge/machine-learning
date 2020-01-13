dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.data <- read.csv(file=paste0(dataPath, "flight_data_2015.csv"), header = TRUE)
head(flight.delay.data)

#Remove NAs
flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY != 0 & flight.delay.data$DEP_DELAY <= 90),]
head(flight.delay.data)
dim(flight.delay.data)

flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data$CRS_DEP_TIME)

flight.delay.data$DELAY_DATE <- ISOdatetime(flight.delay.data$YEAR, flight.delay.data$MONTH, flight.delay.data$DAY_OF_MONTH, substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))

format(head(flight.delay.data$DELAY_DATE), "%Y-%m-%d %H")

flights.ord.data <- flight.delay.data[which(flight.delay.data$ORIGIN == 'ORD'),]
dim(flights.ord.data)

#-----------------------------------------------------------------------------------------------------------------------------------
#Aggregate to hourly

hourly.data.avg <- aggregate(flights.ord.data$DEP_DELAY, by=list(format(flights.ord.data$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.data.avg, 30)

colnames(hourly.data.avg) <- c("date", "delay")

hourly.data.avg[which(hourly.data.avg$delay == 0), ]
dim(hourly.data.avg)

hist(hourly.data.avg$delay, breaks = 200)

dim(hourly.data.avg[which(hourly.data.avg$delay < 0),])
dim(hourly.data.avg[which(hourly.data.avg$delay > 0),])
dim(hourly.data.avg[which(hourly.data.avg$delay == 0),])

length(hourly.data.avg[which(hourly.data.avg$delay > 90),]$delay)


hourly.data.avg$date <- as.POSIXct(hourly.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#Impute data

library(xts)

#Create time series using xts package
df.xts <- xts(hourly.data.avg$delay, start = c(2015,1), order.by = hourly.data.avg$date)
head(df.xts)
dim(df.xts)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
head(df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge
class(df.xts$date)
dim(df.xts)

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))
tail(all.dates.df, 30)
class(all.dates.df$date)
dim(all.dates.df)

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)
head(df.agg)
dim(df.agg)

df.agg[which(df.agg$delay == 0),]$delay <- NA
dim(df.agg)

library(lubridate)
df.agg$date.hour <- hour(df.agg$date)

#Get data between 5 am and 10 pm
df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ]


df.agg[which(is.na(df.agg$delay) == T),]$delay

library(imputeTS)

plotNA.distribution(df.agg$delay)
statsNA(df.agg$delay)

plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))

hourly.data.avg <- na.interpolation(df.agg$delay, option = "spline")
head(hourly.data.avg)
length(hourly.data.avg)

plot.ts(hourly.data.avg)

library(xts)
library(tseries)
library(TSA)
library(forecast)
library(fpp)
library(DescTools)
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#
ts.hourly.data.avg <- ts(hourly.data.avg, frequency = 18)
head(ts.hourly.data.avg)
ts.hourly.test.data.avg <- ts(hourly.test.data.avg, start=c(366,1), frequency = 18)
head(ts.hourly.test.data.avg)

ts.hourly.cv <- tsCV(hourly.data.avg, frequency = 18)

ts.test.1st.month <- ts(ts.hourly.test.data.avg[1:558], frequency = 18, start=c(366,1))

plot(decompose(ts.hourly.data.avg))

adf.test(ts.hourly.data.avg)
kpss.test(ts.hourly.data.avg)

ndiffs(ts.hourly.data.avg)

acf.plt <- acf(ts.hourly.data.avg, lag=400, plot = F)
plot(acf.plt, main = "ACF for Avg Hourly Delay For Flights Departing from ORD")
pacf.plt <- pacf(ts.hourly.data.avg, lag=100, plot = F)
plot(pacf.plt, main = "PACF for Avg Hourly Delay For Flights Departing from ORD")

eacf(ts.hourly.data.avg)

plot(ts.hourly.data.avg, col="blue", ylab="Avg Hourly Delay", main="ORD - 2015 Avg Hourly Delay from 5 am to 10 pm")


acf.diff.plt <- acf(diff(diff(ts.hourly.data.avg)), lag=100, plot = F)
plot(acf.diff.plt, main = "ACF for Differenced Avg Hourly Delay For Flights Departing from ORD")
pacf.diff.plt <- pacf(diff(diff(ts.hourly.data.avg)), lag=100, plot = F)
plot(pacf.diff.plt, main = "PACF for Differenced Avg Hourly Delay For Flights Departing from ORD")
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#LOESS Transformation

loess.data <- data.frame(ind = as.numeric(all.dates.df$date), delay = predict(loess(delay~ind, data = sm.data, span=0.1)))
plot.ts(hourly.data.avg)
lines(loess.data$delay, col="red")

library(ggplot2)
ggplot(rbind(loess.data), aes(ind, delay)) + 
  geom_point(dat = sm.data, aes(ind, delay), alpha = 0.2, col = "red") +
  geom_line(col = "blue") +
  facet_wrap(~"loess()") +
  ggtitle("Interpolation and smoothing functions in R") +
  theme_bw(16)

ts.loess.data <- ts(loess.data$delay, frequency = 24)
ts.loess.data
plot(decompose(ts.loess.data))

auto.arima(ts.loess.data, seasonal = T)
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#TS Decomposition
decomposed.hourly  <- stl(ts.hourly.data.avg, s.window="periodic")
plot(decomposed.hourly)
seasonal.hourly <- decomposed.hourly$time.series[,1]
trend.hourly <- decomposed.hourly$time.series[,2]
remainder.hourly <- decomposed.hourly$time.series[,3]

plot(trend.hourly)
plot(trend.hourly+remainder.hourly)
plot(trend.hourly+seasonal.hourly)
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#Frequency Domain Representation
p <- periodogram(ts.hourly.data.avg, main="Avg Hourly Delay Periodogram")
max_freq <- p$freq[which.max(p$spec)]
max_freq
1/max_freq

frequency.representation <- as.data.frame(x = cbind(head(p$freq[order(p$spec, decreasing = T)]), 1/head(p$freq[order(p$spec, decreasing = T)])))
colnames(frequency.representation) <- c("Frequency", "Seasonality (hours)")
kable(frequency.representation)
write.csv(frequency.representation, file = paste0(dataPath, "freq.csv"))

head(p$spec[order(p$spec, decreasing = T)])

1/head(p$freq[order(p$spec, decreasing = T)])
#-----------------------------------------------------------------------------------------------------------------------------------


#1 Month prediction
pred.period <- 558

#-----------------------------------------------------------------------------------------------------------------------------------
#ETS 

mdl.ets <- ets(ts.hourly.data.avg)
mdl.ets

pred.ets <- forecast(mdl.ets, h = pred.period)
pred.ets

autoplot(pred.ets) + autolayer(ts.test.1st.month)

forecast::accuracy(pred.ets, ts.test.1st.month)
SMAPE(pred.ets$mean, ts.test.1st.month)/2


plot.ts(c(ts.test.1st.month), type="l")
lines(c(pred.ets$mean), type="l", col="blue")
lines(c(pred.tb$mean), type="l", col="green")
lines(c(pred.ft$mean), type="l", col="red")
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#Box-Cox transformation
lambda.fl <- BoxCox.lambda(ts.hourly.data.avg)

ts.hourly.data.avg.bxcx <- BoxCox(ts.hourly.data.avg, lambda = lambda.fl)

mdl.bxcx <- auto.arima(BoxCox(ts.hourly.data.avg, lambda = lambda.fl))
mdl.bxcx

pred.bxcx <- forecast(mdl.bxcx, h = pred.period)

autoplot(pred.bxcx) + autolayer(ts.test.1st.month)

plot.ts(c(ts.test.1st.month[1:40]), type="l")
lines(c(pred.bxcx$mean[1:40]), type="l", col="blue")

forecast:accuracy(pred.bxcx$mean, BoxCox(ts.test.1st.month, lambda = lambda.fl))
SMAPE(pred.bxcx$mean, BoxCox(ts.test.1st.month, lambda = lambda.fl))/2
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#Multi-Seasonality
hourly.data.avg.ms <- mstl(ts.hourly.data.avg)

autoplot(hourly.data.avg.ms)

ts.hourly.data.avg %>%  stlf() %>%
  autoplot()
#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------
#ARIMA - Non-seasonal
library(forecast)
library(DescTools)
hourly.mdl.ns <- auto.arima(ts.hourly.data.avg, seasonal = F, stepwise = F, max.order = 8)
hourly.mdl.ns

tsdisplay(hourly.mdl.ns$residuals)

pred.hourly.ns <- forecast(hourly.mdl.ns, h = pred.period)
pred.hourly.ns

autoplot(pred.hourly.ns) + autolayer(ts.test.1st.month)

plot.ts(c(ts.test.1st.month), type="l")
lines(c(pred.hourly.ns$mean), type="l", col="blue")

forecast.metrics.arima <- forecast::accuracy(pred.hourly.ns, ts.hourly.test.data.avg)
forecast.metrics.arima

smape.arima <- SMAPE(pred.hourly.ns$mean, ts.hourly.test.data.avg)/2
smape.arima

#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#SARIMA

hourly.mdl <- auto.arima(ts.hourly.data.avg, seasonal = T)
summary(hourly.mdl)

pred.hourly <- forecast(hourly.mdl, h = pred.period)
pred.hourly

autoplot(pred.hourly) + autolayer(ts.test.1st.month)

plot(c(ts.hourly.test.data.avg[1:18]), type="l")
lines(c(pred.hourly$mean), col="red")

tsdisplay(hourly.mdl$residuals)
checkresiduals(hourly.mdl$residuals)

forecast.metrics.sarima <- forecast::accuracy(pred.hourly, ts.test.1st.month)
forecast.metrics.sarima
smape.sarima <- SMAPE(pred.hourly$mean, ts.test.1st.month)/2
smape.sarima

#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#Holt Winters
mdl.hw <- HoltWinters(ts.hourly.data.avg)
summary(mdl.hw)

pred.hw <- forecast(mdl.hw, h=pred.period)

autoplot(pred.hw) + autolayer(ts.test.1st.month)

plot(c(ts.hourly.test.data.avg[1:18]), type="l")
lines(c(pred.hw$mean), col="red")

tsdisplay(residuals(mdl.hw))

forecast.metrics.hw <- forecast::accuracy(pred.hw, ts.test.1st.month)
forecast.metrics.hw

smape.hw <- SMAPE(pred.hw$mean, ts.test.1st.month)/2
smape.hw


#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------

#ARFIMA

suppressMessages(suppressWarnings(library(lmtest)))
suppressMessages(suppressWarnings(library(car)))
suppressMessages(suppressWarnings(library(pracma)))

hurstexp(ts.hourly.data.avg)
acf(ts.hourly.data.avg)

library(fracdiff)

fracdiff(ts.hourly.data.avg)

mdl.af <- arfima(ts.hourly.data.avg)
summary(mdl.af)

acf(mdl.af$residuals, lag=400)
pacf(mdl.af$residuals, lag=400)

dwtest(lm(mdl.af$residuals~1), alternative="two.sided")
Box.test(mdl.af$residuals, lag = 10, type = "Ljung-Box", fitdf = 0)

pred.mdl.arf <- forecast(mdl.af, h = pred.period)
plot(pred.mdl.arf)

autoplot(pred.mdl.arf) + autolayer(ts.test.1st.month)

plot(c(ts.test.1st.month), type="l")
lines(c(pred.mdl.arf$mean), col="red")

par(mfrow=c(2,1))
acf.plt <- acf(mdl.af$residuals, lag=100)
plot(acf.plt, main="ARFIMA - Residuals ACF")
pacf.plt <- pacf(mdl.af$residuals, lag=100)
plot(pacf.plt, main="ARFIMA - Residuals PACF")
par(mfrow=c(1,1))


plot.ts(c(ts.test.1st.month), type="l", col="red", main="ARFIMA - Test Data vs Predicted (Jan, 2016)")
lines(c(pred.mdl.arf$mean), type="l", col="blue")

mdl.arf.1 <- arfima(ts.test.1st.month, model = mdl.af)

forecast.metrics.arfima <-forecast::accuracy(mdl.arf.1$fitted, ts.test.1st.month)
SMAPE(mdl.arf.1$fitted, ts.test.1st.month)/2

forecast.metrics.arfima <-forecast::accuracy(pred.mdl.arf, ts.test.1st.month)
forecast.metrics.arfima
smape.arfima <- SMAPE(pred.mdl.arf$mean, ts.test.1st.month)/2
smape.arfima
#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------
#TBATS Model

y <- msts(hourly.data.avg, seasonal.periods = c(18, 3375, 6750)) # multiseasonal ts
y.t <- msts(hourly.test.data.avg[1:pred.period], c(18, 3375, 6750), start=c(1.97,1)) # multiseasonal ts

fit <- auto.arima(y, seasonal=F, xreg=fourier(y, K=c(8, 10, 12)))
fit

pred.ft <- forecast(fit, xreg=fourier(y.t, K=c(8, 10, 12)), h = pred.period)

plot.ts(pred.ft$mean, ylim=c(0,50))
lines(c(ts.test.1st.month), type="l")

plot.ts(c(y.t), type="l")
lines(c(pred.ft$mean), type="l", col="blue")

forecast::accuracy(pred.ft, y.t)
SMAPE(y.t, pred.ft$mean)/2

fit.tb <- tbats(y)
fit.tb



plot(fit.tb)

checkresiduals(fit.tb$errors)
tsdisplay(fit.tb$errors)

par(mfrow=c(2,1))
acf.plt <- acf(fit.tb$errors, lag=100)
plot(acf.plt, main="TBATS Residuals ACF")
pacf.plt <- pacf(fit.tb$errors, lag=100)
plot(pacf.plt, main="TBATS Residuals PACF")
par(mfrow=c(1,1))

components <- tbats.components(fit.tb)
plot(components)

pred.tb <- forecast(fit.tb, h = pred.period)

length(pred.tb$mean)
length(ts.test.1st.month)
plot.ts(pred.tb$mean)
plot(ts.hourly.test.data.avg, type="l")
lines(x=c(1:100),y = pred.tb$mean, col="red")

par(mfrow=c(1,1))
autoplot(pred.tb) + autolayer(y.t)


plot.ts(c(y.t), type="l", col="red", main="TBATS - Test Data vs Predicted (Jan, 2016)")
lines(c(pred.tb$mean), type="l", col="blue")


forecast.metrics.tbats <- forecast::accuracy(pred.tb, ts.test.1st.month)
forecast.metrics.tbats
smape.tbats <- SMAPE(c(ts.test.1st.month), pred.tb$mean)/2
smape.tbats
#-----------------------------------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------------------------------
#Fourier Terms

y.ft <- msts(hourly.data.avg, seasonal.periods = c(18)) # multiseasonal ts
y.ft.test <- msts(hourly.test.data.avg[1:pred.period], c(18), start=c(366,1)) # multiseasonal ts


mdl.hourly.ft <- Arima(y.ft, order=c(5,1,4), xreg=fourier(y.ft, K=c(8)))
mdl.hourly.ft

mdl.hourly.ft.p <- Arima(y.ft.test, model = mdl.hourly.ft, xreg=fourier(y.ft.test, K=c(8)))
SMAPE(mdl.hourly.ft.p$fitted, y.ft.test)/2


plot.ts(c(y.ft.test), type="l", col="red", main="ARIMA with FT - Test Data vs Predicted (Jan, 2016)")
lines(c(mdl.hourly.ft.p$fitted), type="l", col="blue")



forecast::accuracy(mdl.hourly.ft.p$fitted, y.ft.test)

pred.mdl.hourly.ft <- forecast(mdl.hourly.ft, xreg = fourier(y.ft, K=c(8)),  h=pred.period)
pred.mdl.hourly.ft

autoplot(pred.mdl.hourly.ft) + autolayer(y.ft.test)

par(mfrow=c(2,1))
acf.plt <- acf(mdl.hourly.ft$residuals, lag=100)
plot(acf.plt, main="ARIMA with FT - Residuals ACF")
pacf.plt <- pacf(mdl.hourly.ft$residuals, lag=100)
plot(pacf.plt, main="ARIMA with FT - Residuals PACF")
par(mfrow=c(1,1))


plot.ts(c(y.ft.test), type="l", col="red", main="ARIMA with FT - Test Data vs Predicted (Jan, 2016)")
lines(c(pred.mdl.hourly.ft$mean), type="l", col="blue")



forecast.metrics.ft <- forecast::accuracy(pred.mdl.hourly.ft, y.ft.test)
forecast.metrics.ft
smape.ft <- SMAPE(pred.mdl.hourly.ft$mean, y.ft.test)/2
smape.ft





ft.ms.ts.data <- msts(hourly.data.avg, seasonal.periods = c(18, 8))
ft.ms.ts.data

ft.ms.test.ts.data <- msts(hourly.test.data.avg, start=c(366,1), seasonal.periods = c(18, 8))
ft.ms.test.ts.data

ft.ms.test.100 <- msts(hourly.test.data.avg[1:100], start=c(366,1), seasonal.periods = c(18, 8))
ft.ms.test.100

kpss.test(ft.ms.ts.data)
ndiffs(ft.ms.ts.data)

bestfit <- list(aicc=Inf)

for(j in 1:4) {
  for(k in 1:8) {
    fit <- Arima(ft.ms.ts.data, xreg=fourier(ft.ms.ts.data, K=c(j,k)), order = c(5,1,4))
    if(fit$aicc < bestfit$aicc) {
      bestfit <- fit
      best.f.terms <- cbind("j"=j, "k"=k)
    }
    else break;
  }
}


bestfit
best.f.terms

fit.ft <- Arima(ft.ms.ts.data, xreg=fourier(ft.ms.ts.data, K=c(1,8)), order = c(4,1,4))  
fit.ft

fc <- forecast(bestfit, xreg=fourier(ft.ms.test.100, K=c(1,8), h=100))
autoplot(fc) + autolayer(ft.ms.test.100)


forecast::accuracy(fc, ft.ms.test.100)
SMAPE(fc$mean, ft.ms.test.100)/2



#-----------------------------------------------------------------------------------------------------------------------------------

aa<-forecast(ts.hourly.data.avg, h = 558)
plot(aa)
summary(aa)

SMAPE(aa$mean, ts.test.1st.month)/2

smape.models <- as.data.frame(cbind(
  rbind(hourly.mdl.ns$aic, hourly.mdl$aic, AIC(mdl.af), 0, fit.tb$AIC, mdl.hourly.ft$aic),
  rbind(smape.arima, smape.sarima, smape.arfima, smape.hw, smape.tbats, smape.ft),
  rbind(forecast.metrics.arima[,5][2], forecast.metrics.sarima[,5][2], forecast.metrics.arfima[,5][2], forecast.metrics.hw[,5][2], forecast.metrics.tbats[,5][2], forecast.metrics.ft[,5][2]),
  rbind(forecast.metrics.arima[,2][2], forecast.metrics.sarima[,2][2], forecast.metrics.arfima[,2][2], forecast.metrics.hw[,2][2], forecast.metrics.tbats[,2][2], forecast.metrics.ft[,2][2])
))
colnames(smape.models) <- c("AIC", "sMAPE", "MAPE", "RMSE")
rownames(smape.models) <- c("ARIMA", "SARIMA", "ARFIMA", "Holt-Winters", "TBATS", "ARIMA - Fourier Terms")

library(knitr)

kable(smape.models[order(smape.models$sMAPE),])



par(mfrow=c(6,1))

plot(pred.hourly.ns)
plot(pred.hourly)
plot(pred.mdl.arf)
plot(pred.hw)
plot(pred.tb)
plot(pred.mdl.hourly.ft)

par(mfrow=c(1,1))



#-----------------------------------------------------------------------------------------------------------------------------------
#GARCH Model

library(fGarch)

r.delay <- diff(log(ts.hourly.data.avg + 1 - min(ts.hourly.data.avg)))
head(r.delay)

McLeod.Li.test(y=ts.hourly.data.avg)

hourly.garch.model <- garchFit(formula = ~ arma(5,4) + garch(1, 1), data = ts.hourly.data.avg, cond.dist="std")

McLeod.Li.test(y=hourly.garch.model@residuals)

checkresiduals(hourly.garch.model@residuals)

plot(hourly.garch.model)

pred.garch <- fGarch::predict(hourly.garch.model, n.ahead=4000, plot=TRUE)
pred.garch

pred.garch.values <- ts.hourly.data.avg[length(ts.hourly.data.avg)] * cumsum(exp(pred.garch$meanForecast))

plot(pred.garch.values)

checkresiduals(hourly.garch.model@residuals)

forecast::accuracy(pred.garch.values, ts.hourly.test.data.avg[1:4000])

stresi=residuals(hourly.garch.model, standardize=T)
plot(stresi,type="l")
Box.test(stresi, 100, type="Ljung")
#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------
#Neural Network
library(nnfor)

fit.nn <- elm(ts.hourly.data.avg, hd=30, m = 18, reps = 50)
print(fit.nn)
plot(fit.nn)

# Use THieF
library(thief)
mlp.thief <- thief(ts.hourly.data.avg,h=10,forecastfunction=mlp.thief)
print(mlp.thief)
mlp.thief(ts.hourly.data.avg, h = 10)

pred.nn <- forecast(fit.nn, h=6588)
head(pred.nn)

plot(pred.nn)

autoplot(pred.nn) + autolayer(ts.hourly.test.data.avg)

forecast::accuracy(pred.nn, ts.hourly.test.data.avg)
SMAPE(pred.nn$mean, ts.hourly.test.data.avg)/2
#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------
#Prophet Library
length(df.agg$date)
length(ts.hourly.data.avg)
library(prophet)
length(seq(as.POSIXct("2016-01-01 00:00"), to = as.POSIXct("2016-12-31 23:00"), by="hour"))
dim(all.dates.df)
df.agg$date
prophet.data <-as.data.frame(cbind("ds" = df.agg$date, "y" = ts.hourly.data.avg))
head(prophet.data)
prophet.data$ds <- as.Date(prophet.data$ds)
m <- prophet(prophet.data, yearly.seasonality = F, daily.seasonality = T, weekly.seasonality = F)
m$seasonalities

future <- make_future_dataframe(m, periods = 100, freq=60*60)
future[which(as.numeric(format(future$ds, "%H")) >= 5 & as.numeric(format(future$ds, "%H")) <= 22),]

future2 <- future %>% 
  filter(as.numeric(format(future$ds, "%H")) >= 5 & as.numeric(format(future$ds, "%H")) <= 22)

tail(future)
as.Date(future[,1], '%Y-%m-%d')

pred.prophet <- predict(m, future)
tail(pred.prophet[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, pred.prophet)
plot(m)
plot(c(ts.test.1st.month), type = "l")
lines(pred.prophet$yhat[1:100], col="red")
plot(pred.prophet$yhat[1:100])
prophet_plot_components(m, pred.prophet)

dyplot.prophet(m, pred.prophet)

mean(abs(pred.prophet$yhat[1:100] - ts.test.1st.month)/(abs(pred.prophet$yhat[1:100]) + abs(ts.test.1st.month)))

df.cv <- cross_validation(m, horizon = 12000, units = 'hours')
head(df.cv)

df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mape')

#-----------------------------------------------------------------------------------------------------------------------------------










#-----------------------------------------------------------------------------------------------------------------------------------
#Below code I tried running analysis for delays less than or equal to (20,-20) and (10,-10)



hourly.data.limited.avg <- aggregate(flights.ord.data$DEP_DELAY[which(flights.ord.data$DEP_DELAY <= 10 & flights.ord.data$DEP_DELAY >=-10)], by=list(format(flights.ord.data$DELAY_DATE[which(flights.ord.data$DEP_DELAY <= 10 & flights.ord.data$DEP_DELAY >=-10)], "%Y-%m-%d %H:00")), FUN=mean)

colnames(hourly.data.limited.avg) <- c("date", "delay")
hourly.data.limited.avg$date <- as.POSIXct(hourly.data.limited.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")

#Create time series using xts package
df.xts <- xts(hourly.data.limited.avg$delay, start = c(2015,1), order.by = hourly.data.limited.avg$date)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)

df.agg[which(df.agg$delay == 0),]$delay <- NA
df.agg$date.hour <- hour(df.agg$date)

df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ]
dim(df.agg)

library(imputeTS)

plotNA.distribution(df.agg$delay)
statsNA(df.agg$delay)

plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))

hourly.data.limited.avg <- na.interpolation(df.agg$delay, option = "spline")
plot.ts(hourly.data.limited.avg)




ts.hourly.data.limited.avg <- ts(hourly.data.limited.avg, frequency = 18)
head(ts.hourly.data.limited.avg)
ts.hourly.test.data.limited.avg <- ts(hourly.test.data.limited.avg, start=c(366,1), frequency = 18)
head(ts.hourly.test.data.avg)





p <- periodogram(ts.hourly.data.limited.avg)
max_freq <- p$freq[which.max(p$spec)]
max_freq
1/max_freq

1/head(p$freq[order(p$spec, decreasing = T)])




#-----------------------------------------------------------------------------------------------------------------------------------
#ARIMA - Non-seasonal
library(forecast)
library(DescTools)
hourly.mdl.limited.ns <- auto.arima(ts.hourly.data.limited.avg, seasonal = F, stepwise = F, max.order = 8)
summary(hourly.mdl.limited.ns)

pred.hourly.limited.ns <- forecast(hourly.mdl.limited.ns, h = 6588)
pred.hourly.limited.ns

autoplot(pred.hourly.limited.ns) + autolayer(ts.hourly.test.data.limited.avg)

forecast::accuracy(pred.hourly.limited.ns, ts.hourly.test.data.limited.avg)
SMAPE(pred.hourly.limited.ns$mean, ts.hourly.test.data.limited.avg)/2

#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#SARIMA

hourly.limited.mdl <- auto.arima(ts.hourly.data.limited.avg, seasonal = T, stepwise = F, max.order = 8)
summary(hourly.limited.mdl)

pred.limited.hourly <- forecast(hourly.limited.mdl, h = 6588)
pred.limited.hourly

autoplot(pred.limited.hourly) + autolayer(ts.hourly.test.data.limited.avg)

checkresiduals(hourly.limited.mdl$residuals)


forecast::accuracy(pred.limited.hourly, ts.hourly.test.data.limited.avg)
SMAPE(pred.limited.hourly$mean, ts.hourly.test.data.limited.avg)/2


hourly.limited.mdl.2 <- Arima(ts.hourly.data.limited.avg, order = c(3,1,1), seasonal = c(3,0,1))
summary(hourly.limited.mdl.2)


pred.limited.hourly.2 <- forecast(hourly.limited.mdl.2, h = 6588)
pred.limited.hourly.2

autoplot(pred.limited.hourly.2)
forecast::accuracy(pred.limited.hourly.2, ts.hourly.test.data.limited.avg)

#-----------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------

ft.ms.ts.data.limited <- msts(hourly.data.limited.avg, seasonal.periods = c(18))
ft.ms.ts.data.limited

ft.ms.test.ts.data.limited <- msts(hourly.test.data.limited.avg, start=c(366,1), seasonal.periods = c(18))
ft.ms.test.ts.data.limited

fit.ft.limited <- Arima(ft.ms.ts.data.limited, xreg=fourier(ft.ms.ts.data.limited, K=c(9)), order = c(3,1,3))  
fit.ft.limited

fc <- forecast(fit.ft.limited, xreg=fourier(ft.ms.test.ts.data.limited, K=c(9), h=6588))
autoplot(fc) + autolayer(ft.ms.test.ts.data.limited)

forecast::accuracy(fc, ft.ms.test.ts.data.limited)
SMAPE(fc$mean, ft.ms.test.ts.data.limited)/2

#-----------------------------------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------------------------------
#ARIMA - Non-seasonal
library(forecast)
library(DescTools)

hourly.mdl.limited.ns.2 <- auto.arima(ts.hourly.data.limited.avg, seasonal = F, stepwise = F, max.order = 8)
summary(hourly.mdl.limited.ns.2)

pred.hourly.limited.ns.2 <- forecast(hourly.mdl.limited.ns.2, h = 6588)
pred.hourly.limited.ns.2

autoplot(pred.hourly.limited.ns.2) + autolayer(ts.hourly.test.data.limited.avg)

forecast::accuracy(pred.hourly.limited.ns.2, ts.hourly.test.data.limited.avg)
SMAPE(pred.hourly.limited.ns.2$mean, ts.hourly.test.data.limited.avg)/2

#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
#TBATS

mdl.tbats.2 <- tbats(ts.hourly.data.limited.avg)
mdl.tbats.2

pred.tbats.2 <- forecast(mdl.tbats, h=6588)
head(pred.tbats.2)

length(ts.hourly.data.limited.avg)
length(ts.hourly.test.data.limited.avg)

autoplot(pred.tbats.2) + autolayer(ts.hourly.test.data.limited.avg)

forecast::accuracy(pred.tbats.2, ts.hourly.test.data.limited.avg)
SMAPE(pred.tbats.2$mean, ts.hourly.test.data.limited.avg)/2

#-----------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------------------------
#Fourier Terms
ft.ms.ts.data.limited.2 <- msts(hourly.data.limited.avg, seasonal.periods = c(18))
ft.ms.ts.data.limited.2

ft.ms.test.ts.data.limited.2 <- msts(hourly.test.data.limited.avg, start=c(366,1), seasonal.periods = c(18))
ft.ms.test.ts.data.limited.2

fit.ft.limited.2 <- Arima(ft.ms.ts.data.limited.2, xreg=fourier(ft.ms.ts.data.limited.2, K=c(9)), order = c(3,1,1))  
fit.ft.limited.2

fc.2 <- forecast(fit.ft.limited.2, xreg=fourier(ft.ms.test.ts.data.limited.2, K=c(9), h=6588))
autoplot(fc.2) + autolayer(ft.ms.test.ts.data.limited.2)

forecast::accuracy(fc.2, ft.ms.test.ts.data.limited.2)
SMAPE(fc.2$mean, ft.ms.test.ts.data.limited.2)/2

#-----------------------------------------------------------------------------------------------------------------------------------








