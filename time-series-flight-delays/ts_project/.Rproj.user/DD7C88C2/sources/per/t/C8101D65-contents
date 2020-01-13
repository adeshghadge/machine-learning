dataPath <- "/Users/adeshghadge/Downloads/flight-delays/"
flight.delay.data <- read.csv(file=paste0(dataPath, "flights.csv"), header = TRUE)

#Remove NAs
flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY != 0 & flight.delay.data$DEP_DELAY <= 90),]



#-----------------------------------------------------------------------------------------------------------------------------------
#Avg Delays per day for each day of year
flight.delay.data.avg <- aggregate(flight.delay.data$DEP_DELAY, by=list(Airline = flight.delay.data$OP_UNIQUE_CARRIER, Year = flight.delay.data$YEAR, Month = flight.delay.data$MONTH, Day = flight.delay.data$DAY_OF_MONTH), FUN=mean)
head(flight.delay.data.avg)

flight.delay.data.avg$DelayDate <- as.Date(paste(flight.delay.data.avg$Year,flight.delay.data.avg$Month,flight.delay.data.avg$Day, sep="-"))
flight.delay.data.avg <- flight.delay.data.avg[order(flight.delay.data.avg$DelayDate),]
names(flight.delay.data.avg)[5] <- "Delay"
flight.delay.data.avg <- flight.delay.data.avg[, -c(2,3,4)]
head(flight.delay.data.avg)
#-----------------------------------------------------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------------------------------------------------
#Generate hourly data - average of delay time per hour for all hours of the day for the whole year

flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data$CRS_DEP_TIME)

flight.delay.data$DELAY_DATE <- ISOdatetime(flight.delay.data$YEAR, flight.delay.data$MONTH, flight.delay.data$DAY_OF_MONTH, substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))

format(head(flight.delay.data$DELAY_DATE), "%Y-%m-%d %H")

#Flights originating from ORD
flights.ord.data <- flight.delay.data[which(flight.delay.data$ORIGIN == 'ORD'),]


hourly.data.avg <- aggregate(flights.ord.data$DEP_DELAY, by=list(format(flights.ord.data$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
head(hourly.data.avg, 30)

colnames(hourly.data.avg) <- c("date", "delay")

hist(hourly.data.avg$delay, breaks = 200)

dim(hourly.data.avg[which(hourly.data.avg$delay < 0),])
dim(hourly.data.avg[which(hourly.data.avg$delay > 0),])
dim(hourly.data.avg[which(hourly.data.avg$delay == 0),])

hourly.data.avg$date <- as.POSIXct(hourly.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")


#-----------------------------------------------------------------------------------------------------------------------------------
#Impute missing values

library(imputeTS)

#Create time series using xts package
df.xts <- xts(hourly.data.avg$delay, start = c(2015,1), order.by = hourly.data.avg$date)
head(df.xts)

#Set dates for all months to the 1st of the month so that merging to create NA rows is easier
df.xts <- data.frame(delay = df.xts)
head(df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge


#Create data frame containing dates for all months starting from 2001 to 2013
all.dates.df <- data.frame(date = seq(as.POSIXct("2015-01-01 00:00"), to = as.POSIXct("2015-12-31 23:00"), by="hour"))
head(all.dates.df)

#Merge data frames to create new data frame with missing months having NAs
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)
head(df.agg)

#df.agg[which(is.na(df.agg$delay) == T),]$delay <- 0

df.agg[which(df.agg$delay == 0),]$delay <- NA
dim(df.agg)

df.agg$date.hour <- hour(df.agg$date)

#Get data between 5 am and 10 pm
df.agg <- df.agg[which(df.agg$date.hour >= 5 & df.agg$date.hour <= 22), ]

#plot.ts(df.agg[which(df.agg$date > '2015-01-01' & df.agg$date < '2015-01-02'), ]$delay)

hist(aggregate(df.agg[which(df.agg$delay != 0), ]$delay, by=list(df.agg[which(df.agg$delay != 0), ]$date.hour), FUN=length))

hist(df.agg[which(df.agg$delay != 0), ]$date.hour)


plotNA.distribution(df.agg$delay)
statsNA(df.agg$delay)

plotNA.imputations(df.agg$delay, x.withImputations = na.interpolation(df.agg$delay, option = "spline"))


hourly.data.avg <- na.interpolation(df.agg$delay, option = "spline")

#-----------------------------------------------------------------------------------------------------------------------------------


head(hourly.data.avg)
plot.ts(hourly.data.avg)


#Set frequency to 18 hours - 5am to 10pm
ts.hourly.data.avg <- ts(hourly.data.avg, frequency = 18)
head(ts.hourly.data.avg)










#-----------------------------ANTHONY CODE------------------------------------------------------------------------------------------
#Re-establish datetimes for hourly data avg data (so we can line up with xreg weather data using a join function)
formatted_date<-data.frame("Date"=as.POSIXct(df.agg$date, origin="1970-01-01"))
hourly.data.avg<-cbind(formatted_date,hourly.data.avg)
head(hourly.data.avg)
nrow(hourly.data.avg)

#Weather - https://www.ncdc.noaa.gov/cdo-web/datasets
weather<-read.csv(file=paste0(dataPath, "weather2015.csv"), header=TRUE)
formatted_date<-data.frame("Date"=as.POSIXct(weather[,1],format="%Y-%m-%d %H:%M:%OS"))
weather<-cbind(formatted_date+60*9,weather[,-1]) #rounding to nearest hour (all weather info is 9 minutes early)
head(weather)
nrow(weather)
str(weather)

library(plyr)
weather<-join(data.frame("Date"=hourly.data.avg$Date),weather,by="Date")
nrow(weather)
head(weather)
weather<-na.interpolation(weather[,3:ncol(weather)], option = "spline")

weather<-weather[,3:ncol(weather)]
# Check for NAs:
# for (i in 1:ncol(weather)){
#   print(sum(is.na(weather[,i])))
#   }



#Data examination
library(tseries)
library(forecast)
dat<-ts(hourly.data.avg[,2],frequency=18)
plot(dat)
adf.test(dat) #stationary
kpss.test(dat) #nonstationary
##We note that the adf and kpss tests conflict.

#Examining for improvement with 1 difference: it improves
dat_diff<-diff(dat, lag=1, differences=1) 
plot(dat_diff)
adf.test(dat_diff) #stationary
kpss.test(dat_diff) #stationary
##We note that the 1 difference improves the stationarity tests.

#Examining visually for suitable MA and AR values:
acf(dat_diff) #MA(2), seasonal MA(24)
pacf(dat_diff) #AR(6), seasonal AR(24)

#Testing different arima models.
auto<-auto.arima(dat) #(1,1,5) #AIC: 55116.67
arima0<-Arima(dat,order = c(0,1,0)) #AIC:58207
arima1<-Arima(dat,order = c(2,1,6)) #AIC:55058
#arima2<-Arima(dat,order = c(2,1,6),seasonal=list(order = c(24, 0, 24))) #Takes too long, don't run.
arima3<-Arima(dat,order = c(2,1,6),seasonal=list(order = c(0, 1, 0))) #AIC: 55490

#Testing for improvement with a difference of every 24th-lag:
dat_diff2<-diff(dat, lag=24, differences=1)

#Examining for suitable MA and AR values after using 24th-lag difference:
acf(dat_diff2) #MA(15), seasonal MA(24)
pacf(dat_diff2) #AR(7), seasonal AR(24)

auto0.1<-auto.arima(dat_diff2) #(2,0,4) #AIC:57847 #WORSE
arima1.1<-Arima(dat_diff2,order = c(7,0,15),seasonal=list(order = c(0, 0, 0))) #WORSE #AIC:57194.74 



#XREG models
cor(cbind(hourly.data.avg$hourly.data.avg,weather)) 
##We note that wind speed is the most significant predictor with .11 correlation, followed by humidity at -0.09
##The worst predictor is probably Hourly dry/wet bulbs, since they are least correlated to the target variable while highly correlated to other predictors

#Choosing optimal set of weather predictors using 3 different methods: regsubsets, step, and drop1:
library(leaps) 
subsets1<-regsubsets(x=weather,y=hourly.data.avg[,2]) #Regsubset: shows the optimal combination of predictors to include in order of least to most. This is useful for removing unnecessary variables in data while considering the correlations between variables (removing one impacts the rest's ability in explaining Y).
summary(subsets1)$which
##Regsubsets suggests that Windspeed and humidity are valuable if we want a small model.

mdl<-lm(hourly.data.avg[,2]~weather[,1]+weather[,2]+weather[,3]+weather[,4]+weather[,5]+weather[,6]+weather[,7]+weather[,8]+weather[,9])
summary(step(mdl)) 
summary(mdl)
#Step function suggests most predictors are worth keeping, with the exception of weather[,3] (DryBulb).

drop1(mdl) #summarizes multinomial in a way you can choose which variables might be good for removing. It starts with non removed, and each one examines the candidate's removal. You want to remove ones that would result in a lower AIC. Once removed, recheck summary(lm) to see if R^2 and R^2-adj converges and p-values still significant. Regsubsets is useful for considering the ordering of removal (or addition) so you don't have to keep re-checking lm t-stats sequentially.
##It seems only weather[,3] could have some improvement, however it is virtually nothing.

#We now narrow doesn our selection of different models and check performance:
small_mdl<-lm(hourly.data.avg[,2]~weather[,6]) #Wind speed
med_mdl<-lm(hourly.data.avg[,2]~weather[,5]+weather[,6]) #Wind speed, humidity
big_mdl<-lm(hourly.data.avg[,2]~weather[,1]+weather[,2]+weather[,4]+weather[,5]+weather[,6]+weather[,7]+weather[,8]+weather[,9]) #all predictors suggested by step and drop1
summary(small_mdl)
summary(med_mdl)
summary(big_mdl)
##As drop1 and step function suggested: the big model is optimal. A smaller model is more tempting to use, but since we have such large degrees of freedom due to high samples, we will use the bigger model.
##We note that R^2 is nevertheless very low.

#Using our optimal lm, we look to see for the optimal ARIMA parameters
plot(resid(big_mdl))
adf.test(resid(big_mdl)) #stationary
kpss.test(resid(big_mdl)) #nonstationary
acf(resid(big_mdl)) #MA(33), seasonal MA(0)
pacf(resid(big_mdl)) #AR(5), seasonal AR(24)

#We do the same but take a first difference so that adf and kpss tests agree with each other
resid_diff<-diff(resid(big_mdl), lag=1, differences=1) 
adf.test(resid_diff) #stationary
kpss.test(resid_diff) #stationary
acf(resid_diff) #MA(2), seasonal MA(24)
pacf(resid_diff) #AR(6), seasonal AR(24)

auto2<-auto.arima(dat,xreg=(weather[,c(1,2,4,5,6,7,8,9)])) #(1,1,5) #AIC:55036.9
summary(auto2)
xreg_arima<-Arima(dat,order = c(5,0,33),seasonal=list(order = c(0, 0, 0)),xreg=(weather[,c(1,2,4,5,6,7,8,9)])) #AIC:54411.46 #BEST PERFORMING
xreg_arima2<-Arima(dat,order = c(6,1,2),seasonal=list(order = c(0, 0, 0)),xreg=(weather[,c(1,2,4,5,6,7,8,9)])) #AIC:55056.61


#We declade our best model
arima1<-xreg_arima



#We now choose our best performing model and examine the errors:

plot(dat)
lines(fitted(arima1),col="blue")
##The plots show a generally good fit, although there seems to be issues matching extreme values. 

##We now examine the residuals further by visualizing...
par(mfrow=c(2,2))
hist(resid(arima1),main="Frequency Distribution") #shows total frequency distribution
qqnorm(y=resid(arima1),main="Quantiles Comparison: Normality") #shows comparison to normality
qqline(y=resid(arima1))
plot(arima1$residuals[order(fitted(arima1))],main="Residuals vs. Ordered Fitted values: correlation") #Shows if errors are related to fitted value levels. Low fitted values correspond to underestimation, high fitted values correspond to overestimation
plot(resid(arima1),type="b",main="Residuals: autocorrelation")  #Errors ordered chronologically, shows autocorrelation
par(mfrow=c(1,1))
##The distribution of errors show that they are leptokurtic (fat tails). It also shows that low fitted values tend to underestimate, and high fitted values tend to overestimate. There does not seem to be material heteroskedasticity or autocorrelation in errors after applying our model.

##We now continue to examine the residuals further by testing them quantitatively...
acf(resid(arima1)) 
pacf(resid(arima1)) 
adf.test(resid(arima1))  #stationary
kpss.test(resid(arima1)) #stationary
Box.test(resid(arima1),lag=1,type="Ljung-Box") #residuals are independent


#MAE, MPE, MAPE, SMAPE
Corr<-cor(arima1$fitted,hourly.data.avg[,2]) 
(MAE<-mean(resid(arima1)))
(MPE<-mean(resid(arima1)/hourly.data.avg[,2]))
(MAPE<-mean(abs(resid(arima1)/hourly.data.avg[,2])))
(SMAPE<-mean(abs(resid(arima1))/((abs(hourly.data.avg[,2])+abs(fitted(arima1)))*.5)))
##The model seems useful in predicting directional movements of delays, as can be seen by the model correlation of .64 between fitted values and delays and low MAE/MPE, however it  has difficulty predicting the extent of delays due to some tail samples exhibiting extremely large delays. MAE shows better performance because the large errors offset each other.

#Save results for later comparison:
train.scores<-data.frame(matrix(c(Corr,MAE,MPE,MAPE,SMAPE)),row.names=c("Corr","MAE","MPE","MAPE","SMAPE"))
colnames(train.scores)<-"Train"




#TEST ANALYSIS - test using 2016

#Prep 2016 flight data
flight.delay.data <- read.csv(file=paste0(dataPath, "flight_data_2016.csv"), header = TRUE)
flight.delay.data <- flight.delay.data[which(is.na(flight.delay.data$DEP_DELAY) == FALSE & flight.delay.data$DEP_DELAY != 0 & flight.delay.data$DEP_DELAY <= 90),]
flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED <- sprintf("%04d", flight.delay.data$CRS_DEP_TIME)
flight.delay.data$DELAY_DATE <- ISOdatetime(flight.delay.data$YEAR, flight.delay.data$MONTH, flight.delay.data$DAY_OF_MONTH, substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 1, stop = 2), substr(flight.delay.data$SCHEDULED_DEPARTURE_FORMATTED, start = 3, stop = 4), c(0))
hourly.data.avg <- aggregate(flight.delay.data$DEP_DELAY, by=list(format(flight.delay.data$DELAY_DATE, "%Y-%m-%d %H:00")), FUN=mean)
colnames(hourly.data.avg) <- c("date", "delay")
hourly.data.avg$date <- as.POSIXct(hourly.data.avg$date, origin="1970-01-01", format="%Y-%m-%d %H:%M")
df.xts <- xts(hourly.data.avg$delay, start = c(2016,1), order.by = hourly.data.avg$date)
df.xts <- data.frame(delay = df.xts)
df.xts$date <- as.POSIXct(rownames(df.xts), origin="1970-01-01") #Add date column used in merge
all.dates.df <- data.frame(date = seq(as.POSIXct("2016-01-01 00:00"), to = as.POSIXct("2016-12-31 23:00"), by="hour"))
df.agg <- merge(all.dates.df, df.xts, by.x="date", by.y="date", all.x=T, all.y=T)
df.agg[which(df.agg$delay == 0),]$delay <- NA
hourly.data.avg <- na.interpolation(df.agg$delay, option = "spline")
length(hourly.data.avg)
formatted_date<-data.frame("Date"=as.POSIXct(df.agg$date, origin="1970-01-01"))
hourly.data.avg<-cbind(formatted_date,hourly.data.avg)
head(hourly.data.avg)
nrow(hourly.data.avg)


#Prep 2016 Weather data
weather<-read.csv(file=paste0(dataPath, "weather2016.csv"), header=TRUE)
formatted_date<-data.frame("Date"=as.POSIXct(weather[,1],format="%Y-%m-%d %H:%M:%OS"))
weather<-cbind(formatted_date+60*9,weather[,-1]) #rounding to nearest hour (all weather info is 9 minutes early)
weather<-join(data.frame("Date"=hourly.data.avg$Date),weather,by="Date")
weather<-na.interpolation(weather[,3:ncol(weather)], option = "spline")

#Check for NAs:
# for (i in 1:ncol(weather)){
#   print(sum(is.na(weather[,i])))
#   }



#We examine the same steps with TEST data
dat<-hourly.data.avg[,2]
pred<-matrix(Arima(dat,model=arima1,xreg=(weather[,c(1,2,4,5,6,7,8,9)]))$fitted) #Uses our train model
res<-dat-pred

plot(dat,type="l")
lines(pred,col="blue")
##Still has issues with predicting extreme values

##We now examine the residuals further by visualizing...
par(mfrow=c(2,2))
hist(res,main="Frequency Distribution") 
qqnorm(y=res,main="Quantiles Comparison: Normality") 
qqline(y=res)
plot(res[order(pred)],main="Residuals vs. Ordered Fitted values: correlation") 
plot(res,type="b",main="Residuals: autocorrelation") 
par(mfrow=c(1,1))
##Same as with training: The distribution of errors show that they are leptokurtic (fat tails). It also shows that low fitted values tend to underestimate, and high fitted values tend to overestimate. There does not seem to be material heteroskedasticity or autocorrelation in errors.

##We now examine the residuals further by testing them quantitatively...
acf(res) 
pacf(res) 
adf.test(res)  #stationary
kpss.test(res) #stationary
Box.test(res,lag=1,type="Ljung-Box") #residuals are independent



#MAE, MPE, MAPE, SMAPE
Corr=cor(pred,dat) 
(MAE<-mean(res))
(MPE<-mean(res/dat))
(MAPE<-mean(abs(res/dat)))
(SMAPE<-mean(abs(res)/((abs(dat)+abs(pred))*.5)))
##The model seems useful in predicting directional movements of delays, as can be seen by the model correlation of .64 between fitted values and delays and low MAE, however it  has difficulty predicting the extent of delays due to some tail samples exhibiting extremely large delays. MAE shows better performance because the large errors offset each other.

test.scores<-data.frame(matrix(c(Corr,MAE,MPE,MAPE,SMAPE)),row.names=c("Corr","MAE","MPE","MAPE","SMAPE"))
colnames(test.scores)<-"Test"
scores<-cbind(train.scores,test.scores)
#The test scores were overall better than train. This is likely due to 2015 data having more spikes in sudden/extreme delays


#Final test: Does the model add value over using the last value?
dat0<-dat[1:8783]
pred0<-dat[2:8784]
res0<-dat0-pred0
plot(dat0,type="l")
lines(pred0,col="blue")

par(mfrow=c(2,2))
hist(res0,main="Frequency Distribution") 
qqnorm(y=res0,main="Quantiles Comparison: Normality") y
qqline(y=res0)
plot(res0[order(pred0)],main="Residuals vs. Ordered Fitted values: correlation") 
plot(res0,type="b",main="Residuals: autocorrelation")  
par(mfrow=c(1,1))

acf(res0) 
pacf(res0) 
adf.test(res0)  #stationary
kpss.test(res0) #nonstationary
Box.test(res0,lag=1,type="Ljung-Box") #residuals are independent
#Does not lead to white-noise errors

(Corr<-cor(pred0,dat0))
(MAE<-mean(res0))
(MPE<-mean(res0/dat0))
(MAPE<-mean(abs(res0/dat0)))
(SMAPE<-mean(abs(res0)/((abs(dat0)+abs(pred0))*.5)))

#Compare all model results:
last.scores<-data.frame(matrix(c(Corr,MAE,MPE,MAPE,SMAPE)),row.names=c("Corr","MAE","MPE","MAPE","SMAPE"))
colnames(last.scores)<-"Previous"
(scores<-round(cbind(train.scores,test.scores,last.scores),4))

#Using the last value as the next prediction is shown to be better at predicting the extreme spikes in tails and overall led to a more balanced mean error. This makes sense since our arima model is smoother since it is a function of many past values. 
#However, the arima model was slightly better with mean percent error (MPE) and correlation, meaning that more accurate predictions in modest delays or delay directional movements more than compensated for the loss in extreme delay values.





