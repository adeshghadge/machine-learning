#-----------------------------------------------------------------------------------------------------------------------------------
#Analyze 3 Routes
detach(flight.delay.data)
attach(flight.delay.data)
head(flight.delay.data)

freq.origin.airport <- as.data.frame(table(ORIGIN))
freq.origin.airport <- freq.origin.airport[order(freq.origin.airport$Freq, decreasing = T),]
head(freq.origin.airport)

freq.dest.airport <- as.data.frame(table(DEST))
freq.dest.airport <- freq.dest.airport[order(freq.dest.airport$Freq, decreasing = T),]
head(freq.dest.airport)

flights.ord.airport <- flight.delay.data[which(ORIGIN=='ORD'),]
freq.ord.airport <- as.data.frame(table(paste(flights.ord.airport$ORIGIN, flights.ord.airport$DEST,sep=" - ")))
freq.ord.airport <- freq.ord.airport[order(freq.ord.airport$Freq, decreasing = T),]
head(freq.ord.airport)

flights.ord.lga <- flight.delay.data[which(ORIGIN=='ORD' & DEST=='LGA'),]
head(flights.ord.lga)
dim(flights.ord.lga)

flights.per.hour <- aggregate(flights.ord.lga$DEP_DELAY, by=list(format(flights.ord.lga$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=length)
head(flights.per.hour, 30)
colnames(flights.per.hour) <- c("date", "delay")

flights.per.hour$date <- as.POSIXct(flights.per.hour$date, origin="1970-01-01")

df.xts <- xts(flights.per.hour$delay, start = c(2015,1), order.by = flights.per.hour$date)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)

flights.per.hour <- df.agg

dim(flights.per.hour)
dim(flights.per.hour[which(is.na(flights.per.hour$delay) == T),])
dim(flights.per.hour[which(flights.per.hour$delay == 0),] )

flights.per.hour[which(is.na(flights.per.hour$delay) == T),]$delay = 0

plot(y = flights.per.hour$delay, x = flights.per.hour$date)

ts.flights.per.hour <- ts(flights.per.hour$delay, frequency = 24)

plot.ts(ts.flights.per.hour)

plot(decompose(ts.flights.per.hour))

#Every hour of every day for every destination
hourly.ord.lga.data.avg <- aggregate(flights.ord.lga$DEP_DELAY, by=list(format(flights.ord.lga$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.ord.lga.data.avg, 30)

colnames(hourly.ord.lga.data.avg) <- c("date", "delay")

dim(hourly.ord.lga.data.avg[which(hourly.ord.lga.data.avg$delay < 0),])
dim(hourly.ord.lga.data.avg[which(hourly.ord.lga.data.avg$delay > 0),])
dim(hourly.ord.lga.data.avg[which(hourly.ord.lga.data.avg$delay == 0),])

length(hourly.ord.lga.data.avg[which(hourly.ord.lga.data.avg$delay > 90),]$delay)


hourly.ord.lga.data.avg$date <- as.POSIXct(hourly.ord.lga.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")



#Create time series using xts package
df.xts <- xts(hourly.ord.lga.data.avg$delay, start = c(2015,1), order.by = hourly.ord.lga.data.avg$date)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)

df.agg[which(df.agg$delay == 0),]$delay <- NA

dim(df.agg)
dim(df.agg[which(is.na(df.agg$delay)),])

library(imputeTS)

plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))

hourly.ord.lga.data.avg <- na.interpolation(df.agg$delay, option = "spline")

ts.hourly.data.avg <- xts(hourly.ord.lga.data.avg, order.by = hourly.data.avg$date)

ts.hourly.ord.lga.data.avg <- ts(hourly.ord.lga.data.avg, frequency = 24)
head(ts.hourly.ord.lga.data.avg)

plot(decompose(ts.hourly.ord.lga.data.avg))


adf.test(ts.hourly.ord.lga.data.avg)
kpss.test(ts.hourly.ord.lga.data.avg)

acf(ts.hourly.ord.lga.data.avg, lag=800)
pacf(ts.hourly.ord.lga.data.avg, lag=300)

ndiffs(ts.hourly.ord.lga.data.avg)

auto.arima(ts.hourly.ord.lga.data.avg)

#-----------------------------------------------------------------------------------------------------------------------------------
y <- msts(hourly.data.avg, seasonal.periods = c(24, 7, 30.4)) # multiseasonal ts
y.t <- msts(hourly.test.data.avg, c(24, 7, 30.4), start=c(290,1)) # multiseasonal ts
fit <- auto.arima(y, seasonal=F, xreg=fourier(y, K=c(3, 3, 8)))
fit

fit.tb <- tbats(y)
fit.tb

plot(fit.tb)

components <- tbats.components(fit.tb)
plot(components)

plot(forecast(fit.tb, h = 365))

forecast::accuracy(fit.tb)

checkresiduals(fit.tb$errors)


fit_f <- forecast(fit, xreg= fourier(y, K=c(3,3,8), 8780), 8780)
plot(fit_f)
autoplot(fit_f)+autolayer(y.t)

checkresiduals(fit_f$residuals)

forecast::accuracy(fit_f, ts.hourly.test.data.avg)
smape(ts.hourly.test.data.avg, fit_f$mean)
#-----------------------------------------------------------------------------------------------------------------------------------

