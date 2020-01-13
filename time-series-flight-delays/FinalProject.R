
dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.data <- read.csv(file=paste0(dataPath, "flights.csv"), header = TRUE)
head(flight.delay.data)

dim(flight.delay.data)
names(flight.delay.data)

summary(flight.delay.data$DEPARTURE_DELAY)
summary(flight.delay.data$ARRIVAL_DELAY)

describe(flight.delay.data$DEPARTURE_DELAY) 
describe(flight.delay.data$ARRIVAL_DELAY) 

flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEPARTURE_DELAY) == FALSE & flight.delay.data$DEPARTURE_DELAY > 0),]
head(flight.delay.data)
dim(flight.delay.data)

length(which((flight.delay.data$DEPARTURE_DELAY > 0) == TRUE))

#Min Delays
flight.delay.data.min <- aggregate(flight.delay.data$DEPARTURE_DELAY, by=list(Airline = flight.delay.data$AIRLINE, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=min)
head(flight.delay.data.min)

flight.delay.data.min$DelayDate <- as.Date(paste(flight.delay.data.min$Year,flight.delay.data.min$Month,flight.delay.data.min$Day, sep="-"))
flight.delay.data.min <- flight.delay.data.min[order(flight.delay.data.min$DelayDate),]
names(flight.delay.data.min)[5] <- "Delay"
flight.delay.data.min <- flight.delay.data.min[, -c(2,3,4)]

#Max Delays
flight.delay.data.max <- aggregate(flight.delay.data$DEPARTURE_DELAY, by=list(Airline = flight.delay.data$AIRLINE, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=max)
head(flight.delay.data.max)

flight.delay.data.max$DelayDate <- as.Date(paste(flight.delay.data.max$Year,flight.delay.data.max$Month,flight.delay.data.max$Day, sep="-"))
flight.delay.data.max <- flight.delay.data.max[order(flight.delay.data.max$DelayDate),]
names(flight.delay.data.max)[5] <- "Delay"
flight.delay.data.max <- flight.delay.data.max[, -c(2,3,4)]

#Median Delays
flight.delay.data.mdn <- aggregate(flight.delay.data$DEPARTURE_DELAY, by=list(Airline = flight.delay.data$AIRLINE, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=median)
head(flight.delay.data.mdn)

flight.delay.data.mdn$DelayDate <- as.Date(paste(flight.delay.data.mdn$Year,flight.delay.data.mdn$Month,flight.delay.data.mdn$Day, sep="-"))
flight.delay.data.mdn <- flight.delay.data.mdn[order(flight.delay.data.mdn$DelayDate),]
names(flight.delay.data.mdn)[5] <- "Delay"
flight.delay.data.mdn <- flight.delay.data.mdn[, -c(2,3,4)]
head(flight.delay.data.mdn)

#Avg Delays
flight.delay.data.avg <- aggregate(flight.delay.data$DEPARTURE_DELAY, by=list(Airline = flight.delay.data$AIRLINE, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY), FUN=mean)
head(flight.delay.data.avg)

flight.delay.data.avg$DelayDate <- as.Date(paste(flight.delay.data.avg$Year,flight.delay.data.avg$Month,flight.delay.data.avg$Day, sep="-"))
flight.delay.data.avg <- flight.delay.data.avg[order(flight.delay.data.avg$DelayDate),]
names(flight.delay.data.avg)[5] <- "Delay"
flight.delay.data.avg <- flight.delay.data.avg[, -c(2,3,4)]
head(flight.delay.data.avg)

#Split data per airline - 365 rows per airline
airline.data.min <- split(flight.delay.data.min, flight.delay.data.min$Airline)
airline.data.max <- split(flight.delay.data.max, flight.delay.data.max$Airline)
airline.data.mdn <- split(flight.delay.data.mdn, flight.delay.data.mdn$Airline)
airline.data.avg <- split(flight.delay.data.avg, flight.delay.data.avg$Airline)




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
    mdl <- Arima(mdl.data, order = c(p,d,q), seasonal = c(P, D, Q))
    mdl$bic
  })
  
}



library(xts)
library(tseries)
library(TSA)
library(forecast)

airline.ts.avg <- ts(subset(airline.data.avg$AA, DelayDate < '2015-12-01')$Delay, frequency = 7)
head(airline.ts.avg)

airline.ts.test.avg <- ts(subset(airline.data.avg$AA, DelayDate > '2015-12-01')$Delay, start=c(48,6), frequency = 7)
head(airline.ts.test.avg)

plot(decompose(airline.ts.avg))

acf(airline.ts.avg, lag = 100)
pacf(airline.ts.avg, lag = 100)
eacf(airline.ts.avg)


adf.test(airline.ts.avg)
kpss.test(airline.ts.avg)

ndiffs(airline.ts.avg)


mdl1 <- auto.arima(airline.ts.avg, seasonal = T)

pred.mdl1 <- forecast(mdl1, h= 31)
pred.mdl1

autoplot(pred.mdl1) + autolayer(airline.ts.test.avg)


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

aicc.seasonal.gs <- suppressMessages(gridSearch(fun = calcAICc.seasonal, levels = list(p = 1:5, d = 1, q = 1:5, P = 1:3, D = 0:1, Q=1:3)))
bic.seasonal.gs <- suppressWarnings(suppressMessages(gridSearch(fun = calcBIC.seasonal, levels = list(p = 1:5, d = 1, q = 1:5, P = 1:3, D = 0:1, Q=1:3))))

cbind("min AICc" = aicc.seasonal.gs$minfun, "min BIC" = bic.seasonal.gs$minfun)
cbind("min AICc p,q" = aicc.seasonal.gs$minlevels, "min BIC p,q" = bic.seasonal.gs$minlevels)

m1.bst <- Arima(airline.ts.avg, order = c(2,1,2), seasonal = c(1,1,1))

pred.m1.bst <- forecast(m1.bst, h = 14)
pred.m1.bst

autoplot(pred.m1.bst) + autolayer(pred.m1.bst)

head(flight.delay.data[, c(12, 13, 14,17,18)])

lm.mdl <- lm(DEPARTURE_DELAY~., flight.delay.data[, c(12, 18)])
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

flight.tree.analysis.data <- flight.delay.data[which(is.na(flight.delay.data$DEPARTURE_DELAY) == FALSE),]
flight.tree.analysis.data$FLIGHT_STATE <- NA
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEPARTURE_DELAY == 0] <- "ON TIME"
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEPARTURE_DELAY > 0] <- "DELAYED"
flight.tree.analysis.data$FLIGHT_STATE[flight.tree.analysis.data$DEPARTURE_DELAY < 0] <- "BEFORE TIME"
flight.tree.analysis.data$FLIGHT_DATE <- as.Date(paste(flight.tree.analysis.data$YEAR,flight.tree.analysis.data$MONTH,flight.tree.analysis.data$DAY, sep="-"))

typeof(flight.tree.analysis.data)
names(flight.tree.analysis.data)

keep.cols <- c("FLIGHT_DATE", "FLIGHT_STATE", "DAY_OF_WEEK","AIRLINE","FLIGHT_NUMBER","TAIL_NUMBER","ORIGIN_AIRPORT","SCHEDULED_DEPARTURE","DESTINATION_AIRPORT","DISTANCE")

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

