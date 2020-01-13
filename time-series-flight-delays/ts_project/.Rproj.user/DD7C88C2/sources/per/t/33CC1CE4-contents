dpath <- "/Users/adeshghadge/Downloads/flight-delays/data/4 years/"

LoadDataFromDir = function(dpath, format) {
  
  load.df <- NULL
  fnames <- list.files(dpath, pattern = paste0("*.",format))
  
  for (f in fnames) {
    
    if(grepl("_2014_", f) == T) {
      tempData <- as.data.frame(read.csv(paste0(dpath, f), header = TRUE))
      
      if(is.null(load.df) == TRUE) {
        load.df <- tempData
      }
      else {
        load.df <- rbind(load.df, tempData)
      }
    }
  }
  
  return(load.df)
}

flight.delay.data.2014.multi <- LoadDataFromDir(dpath, "csv")
write.csv(flight.delay.data.2014.multi, file = paste0(dpath, "flight_data_2015.csv"))


flight.delay.data.2014 <- LoadDataFromDir(dpath, "csv")
write.csv(flight.delay.data.2014, file = paste0(dpath, "flight_data_2014.csv"))

flight.delay.data.2015 <- LoadDataFromDir(dpath, "csv")
write.csv(flight.delay.data.2015, file = paste0(dpath, "flight_data_2015.csv"))

flight.delay.data.2016 <- LoadDataFromDir(dpath, "csv")
head(flight.delay.data.2016)
write.csv(flight.delay.data.2016, file = paste0(dpath, "flight_data_2016.csv"))

dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.data.2014 <- read.csv(file=paste0(dataPath, "flight_data_2014.csv"), header = TRUE)
head(flight.delay.data.2014)

flight.delay.data.2014 <- flight.delay.data.2014[which(is.na(flight.delay.data.2014$DEP_DELAY) == FALSE & flight.delay.data.2014$DEP_DELAY != 0 & flight.delay.data.2014$DEP_DELAY <= 90),]

flight.delay.data.2014$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data.2014$CRS_DEP_TIME)

flight.delay.data.2014$DELAY_DATE <- ISOdatetime(flight.delay.data.2014$YEAR, flight.delay.data.2014$MONTH, flight.delay.data.2014$DAY_OF_MONTH, substr(flight.delay.data.2014$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data.2014$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))

flights.ord.data.2014 <- flight.delay.data.2014[which(flight.delay.data.2014$ORIGIN == 'ORD'),]
head(flights.ord.data.2014)
dim(flights.ord.data.2014)

hourly.data.avg.2014 <- aggregate(flights.ord.data.2014$DEP_DELAY, by=list(format(flights.ord.data.2014$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.data.avg.2014, 30)

colnames(hourly.data.avg.2014) <- c("date", "delay")

hourly.data.avg.2014$date <- as.POSIXct(hourly.data.avg.2014$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")


library(xts)

#Create time series using xts package
df.xts <- xts(hourly.data.avg.2014$delay, start = c(2014,1), order.by = hourly.data.avg.2014$date)
head(df.xts)
dim(df.xts)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
head(df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge
class(df.xts$date)
dim(df.xts)

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2014-01-01 00:00"), to = as.POSIXct("2014-12-31 23:00"), by="hour"))
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

hourly.data.avg.2014 <- na.interpolation(df.agg$delay, option = "spline")

plot.ts(hourly.data.avg.2014)


hourly.data.avg.multi <- c(hourly.data.avg.2014, hourly.data.avg)
head(hourly.data.avg.multi)
length(hourly.data.avg.multi)

ts.hourly.data.avg.multi <- ts(hourly.data.avg.multi, freq = 18)
ts.hourly.data.avg.multi

ts.hourly.test.data.avg.multi <- ts(hourly.test.data.avg, start=c(731,1), frequency = 18)
head(ts.hourly.test.data.avg.multi)


p <- periodogram(ts.hourly.data.avg.multi, main="Avg Hourly Delay Periodogram")
max_freq <- p$freq[which.max(p$spec)]
max_freq
1/max_freq




hourly.mdl.multi <- auto.arima(ts.hourly.data.avg.multi, seasonal = T)
summary(hourly.mdl.multi)

pred.hourly.multi <- forecast(hourly.mdl.multi, h = pred.period)
pred.hourly.multi



plot(c(ts.test.1st.month), type="l")
lines(c(pred.hourly.multi$mean), col="red")


y.ft.multi <- msts(hourly.data.avg.multi, seasonal.periods = c(18)) # multiseasonal ts
y.ft.multi.test <- msts(hourly.test.data.avg[1:pred.period], c(18), start=c(731,1)) # multiseasonal ts


mdl.hourly.ft.multi <- Arima(y.ft.multi, order=c(5,1,4), xreg=fourier(y.ft.multi, K=c(8)))
mdl.hourly.ft.multi

mdl.hourly.ft.multi.p <- Arima(y.ft.multi.test, model = mdl.hourly.ft.multi, xreg=fourier(y.ft.multi.test, K=c(8)))
SMAPE(mdl.hourly.ft.multi.p$fitted, y.ft.multi.test)/2


plot.ts(c(y.ft.multi.test), type="l", col="red", main="ARIMA with FT - Test Data vs Predicted (Jan, 2016)")
lines(c(y.ft.multi.test), type="l", col="blue")



forecast::accuracy(mdl.hourly.ft.p$fitted, y.ft.test)

pred.mdl.hourly.ft.multi <- forecast(mdl.hourly.ft.multi, xreg = fourier(y.ft.multi.test, K=c(8)),  h=pred.period)
pred.mdl.hourly.ft.multi

autoplot(pred.mdl.hourly.ft.multi) + autolayer(y.ft.multi.test)


plot.ts(c(y.ft.multi.test), type="l", col="red", main="ARIMA with FT - Test Data vs Predicted (Jan, 2016)")
lines(c(pred.mdl.hourly.ft.multi$mean), type="l", col="blue")



