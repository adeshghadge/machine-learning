dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.test.data <- read.csv(file=paste0(dataPath, "flight_data_2016.csv"), header = TRUE)
head(flight.delay.test.data)

#Remove NAs
flight.delay.test.data <- flight.delay.test.data[which(is.na(flight.delay.test.data$DEP_DELAY) == FALSE & flight.delay.test.data$DEP_DELAY != 0 & flight.delay.test.data$DEP_DELAY <= 90),]
head(flight.delay.test.data)
dim(flight.delay.test.data)

flight.delay.test.data$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.test.data$CRS_DEP_TIME)

flight.delay.test.data$DELAY_DATE <- ISOdatetime(flight.delay.test.data$YEAR, flight.delay.test.data$MONTH, flight.delay.test.data$DAY_OF_MONTH, substr(flight.delay.test.data$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.test.data$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))

format(head(flight.delay.test.data$DELAY_DATE), "%Y-%m-%d %H")

flights.ord.test.data <- flight.delay.test.data[which(flight.delay.test.data$ORIGIN == 'ORD'),]
dim(flights.ord.test.data)

hourly.test.data.avg <- aggregate(flights.ord.test.data$DEP_DELAY, by=list(format(flights.ord.test.data$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.test.data.avg, 30)

hourly.test.data.avg[which(hourly.test.data.avg$delay == 0), ]
dim(hourly.test.data.avg)

colnames(hourly.test.data.avg) <- c("date", "delay")

hist(hourly.test.data.avg$delay, breaks = 200)

dim(hourly.test.data.avg[which(hourly.test.data.avg$delay < 0),])
dim(hourly.test.data.avg[which(hourly.test.data.avg$delay > 0),])
dim(hourly.test.data.avg[which(hourly.test.data.avg$delay == 0),])

length(hourly.test.data.avg[which(hourly.test.data.avg$delay > 90),]$delay)


hourly.test.data.avg$date <- as.POSIXct(hourly.test.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")



library(imputeTS)

#Create time series using xts package
df.xts <- xts(hourly.test.data.avg$delay, start = c(2016,1), order.by = hourly.test.data.avg$date)
head(df.xts)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
head(df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge
class(df.xts$date)
dim(df.xts)

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2016-01-01 00:00"), to = as.POSIXct("2016-12-31 23:00"), by="hour"))
tail(all.dates.df, 30)
class(all.dates.df$date)
dim(all.dates.df)

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)
head(df.agg)


df.agg[which(df.agg$delay == 0),]$delay <- NA
dim(df.agg)

df.agg$date.hour <- hour(df.agg$date)

df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ]
dim(df.agg)

hourly.test.data.avg <- na.interpolation(df.agg$delay, option = "spline")

plot.ts(hourly.test.data.avg[which(hourly.test.data.avg$date > '2016-07-18' & hourly.test.data.avg$date < '2016-07-19'), ]$delay)


impute.data(hourly.test.data.avg)
hourly.test.data.avg <- impute.data(hourly.test.data.avg)

head(hourly.test.data.avg)
plot.ts(hourly.test.data.avg)















hourly.test.data.limited.avg <- aggregate(flights.ord.test.data$DEP_DELAY[which(flights.ord.test.data$DEP_DELAY <= 10 & flights.ord.test.data$DEP_DELAY >=-10)], by=list(format(flights.ord.test.data$DELAY_DATE[which(flights.ord.test.data$DEP_DELAY <= 10 & flights.ord.test.data$DEP_DELAY >=-10)], "%Y-%m-%d %H:00")), FUN=mean)

colnames(hourly.test.data.limited.avg) <- c("date", "delay")
hourly.test.data.limited.avg$date <- as.POSIXct(hourly.test.data.limited.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")

#Create time series using xts package
df.xts <- xts(hourly.test.data.limited.avg$delay, start = c(2016,1), order.by = hourly.test.data.limited.avg$date)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge

#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2016-01-01 00:00"), to = as.POSIXct("2016-12-31 23:00"), by="hour"))

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)

df.agg[which(df.agg$delay == 0),]$delay <- NA
df.agg$date.hour <- hour(df.agg$date)

df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ]


library(imputeTS)

plotNA.distribution(df.agg$delay)
statsNA(df.agg$delay)

plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))

hourly.test.data.limited.avg <- na.interpolation(df.agg$delay, option = "spline")
plot.ts(hourly.test.data.limited.avg)

