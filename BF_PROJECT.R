
library(forecast)
library(fpp)
library(zoo)
library(tseries)

# Importing Data
cigarette <- read.csv(file.choose())
View(cigarette)

# Plots
plot(cigarette, main = "Scatter Plot")
plot(cigarette$Year, cigarette$Total, xlab = "Year", ylab = "Total", main = "Total Consumption of cigarettes")
plot(cigarette$Total, cigarette$Expectancy, xlab = "Total", ylab = "Age", main = "Total Vs Life Expectancy")

#Summary
summary(cigarette)

#Converting the data to a Time Series dataset
raw_data<- cigarette[,-1]
cigarette_ts <- ts(raw_data, start=c(1990,1), end=c(2015,1), frequency = 1)
summary(cigarette_ts)
View(cigarette_ts)

#Plots
plot(cigarette_ts, main = "Time Series Scatter Graph")

# Extracting trend/seasonality
cigarette_ts_seatre <- decompose(cigarette_ts,type = "mult")
Stl = stl(cigarette_ts[,2],s.window="periodic")
# Series is not periodic or has less than two periods. Hence not Seasonal

# Imputing missing values using Holt Winters
HW <- HoltWinters(cigarette_ts[1:22],alpha = 0.1,beta = 0.9,gamma = F)
predict(HW,n.ahead=4)

#Inserting the missing data into the original dataset
cigarette[!complete.cases(cigarette),]
cigarette[is.na(cigarette$Total) & cigarette$Year==2012,"Total"] <- 281.8
cigarette[is.na(cigarette$Total) & cigarette$Year==2013,"Total"] <- 261.9
cigarette[is.na(cigarette$Total) & cigarette$Year==2014,"Total"] <- 242.0
cigarette[is.na(cigarette$Total) & cigarette$Year==2015,"Total"] <- 222.1
cigarette[c(23,24,25,26),]
View(cigarette)

######## New Time Series Data ##############

# Convert the new cigarette data to time series
cigarette_ts1<- cigarette[,-1]
cigarette_ts1 <- ts(cigarette_ts1, start=c(1990,1), end=c(2015,1), frequency = 1)
summary(cigarette_ts1)
View(cigarette_ts1)


## Stationary/Non-Stationary
#ADF
adf = adf.test(cigarette_ts1[,1])
adf
#not stationary as p-value > 0.05

#KPSS
#P-value > 0.05, usually does not require differencing
kpss = kpss.test(cigarette_ts1[,1])
kpss
# As it is less than 0.05 differencing is required, null hypothesis is rejected and 
#the series is not stationary

# Forecasting using Holt Winters
HW <- HoltWinters(cigarette_ts1[1:26],alpha = 0.1,beta = 0.9,gamma = F)
predict(HW,n.ahead=5)
HoltWintersFit <- HoltWinters(subset_total_ts, gamma=FALSE)
HoltWintersFit
plot(HoltWintersFit)
HoltWintersFitForecast <- forecast.HoltWinters(HoltWintersFit, h=5)
plot.forecast(HoltWintersFitForecast)
accuracy(HoltWintersFitForecast)

# Converting a multivariate time series to an univariate time series
subset_total <- cigarette[ , c(2)]
subset_total_ts <- ts(subset_total, start=c(1990,1), end=c(2015,1), frequency = 1)
View(subset_total_ts)

## Training & testing
h <- 5   # #of forecast
cigstr <- subset_total_ts[1:(length(subset_total_ts)-h)]   # training data
cigstt <- subset_total_ts[(length(subset_total_ts)-h+1):length(subset_total_ts)]   # testing data

fit <- auto.arima(cigstr,seasonal=FALSE); fit
fcst <- forecast(fit,h=5); fcst$mean
plot(forecast(fit,h=5))

fit1 <- Arima(cigstr,order=c(1,0,0)); fit1   # AR(1)
fit2 <- Arima(cigstr,order=c(0,1,0)); fit2   # Random walk
fit3 <- Arima(cigstr,order=c(0,0,1)); fit3   # MA(1)
fit4 <- Arima(cigstr,order=c(1,1,0)); fit4   
fit5 <- Arima(cigstr,order=c(1,0,1)); fit5
fit6 <- Arima(cigstr,order=c(0,1,1)); fit6   # EWMA (Exp. smoothing)
fit7 <- Arima(cigstr,order=c(1,1,1)); fit7
fit8 <- Arima(cigstr,order=c(2,1,1)); fit8
fit9 <- Arima(cigstr,order=c(1,2,1)); fit9
fit10 <- Arima(cigstr,order=c(1,1,2)); fit10


fcst1 <- forecast(fit1,h=5); fcst1$mean
fcst2 <- forecast(fit2,h=5); fcst2$mean
fcst3 <- forecast(fit3,h=5); fcst3$mean
fcst4 <- forecast(fit4,h=5); fcst4$mean
fcst5 <- forecast(fit5,h=5); fcst5$mean
fcst6 <- forecast(fit6,h=5); fcst6$mean   # EWMA model
fcst7 <- forecast(fit7,h=5); fcst7$mean
fcst8 <- forecast(fit8,h=5); fcst8$mean
fcst9 <- forecast(fit9,h=5); fcst9$mean
fcst10 <- forecast(fit10,h=5); fcst10$mean


acc <- accuracy(cigstt,fcst$mean)
acc1 <- accuracy(cigstt,fcst1$mean)
acc2 <- accuracy(cigstt,fcst2$mean)
acc3 <- accuracy(cigstt,fcst3$mean)
acc4 <- accuracy(cigstt,fcst4$mean)
acc5 <- accuracy(cigstt,fcst5$mean)
acc6 <- accuracy(cigstt,fcst6$mean)
acc7 <- accuracy(cigstt,fcst7$mean)
acc8 <- accuracy(cigstt,fcst8$mean)
acc9 <- accuracy(cigstt,fcst9$mean)
acc10 <- accuracy(cigstt,fcst10$mean)

acc;acc1;acc2;acc3;acc4;acc5;acc6;acc7;acc8;acc9;acc10

plot(fcst);fcst

# Plot ACF and PACF to identify potential AR and MA model
par(mfrow = c(1,2))
acf(ts(diff(log10(subset_total_ts))),main='ACF')
pacf(ts(diff(log10(subset_total_ts))),main='PACF')

require(forecast)
ARIMAfit = auto.arima(log10(subset_total_ts), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
accuracy(ARIMAfit)

# forecast using ARIMA model
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 60)
pred
plot(subset_total_ts,type='l',xlim=c(1990,2020),ylim=c(1,600),xlab = 'Year',ylab = 'Total consumption of Cigarettes')
lines(10^(pred$pred),col='blue') #Forecasted value
lines(10^(pred$pred+2*pred$se),col='red') # expected error for 2 standard deviations
lines(10^(pred$pred-2*pred$se),col='red')

accuracy(HoltWintersFitForecast,cigstt)

#ARIMA model fitting 
fit11 <- auto.arima(subset_total_ts,seasonal=FALSE); fit11
plot(forecast(fit11,h=5),xlab = "Year", ylab = "Total")

#Plot ACF and PACF for residuals of ARIMA model to ensure no more information is left for extraction
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')

##### Dynamic Regression ######
# for total and life expectancy
subset_total_expectancy <- cigarette[ , c(2,7)]
subset_total_expectancy_ts <- ts(subset_total_expectancy, start=c(1990,1), end=c(2011,1), frequency = 1)
View(subset_total_expectancy_ts)

plot(subset_total_expectancy_ts, xlab="Year", main="Graph")
fit12 <- auto.arima(subset_total_expectancy_ts[,1], xreg=subset_total_expectancy_ts[,2]); fit12
#fcast <- forecast(fit12,xreg=subset_total_expectancy_ts[,2],xlab = "Year", ylab = "Total", main = "Dynamic Regression for Total VS Expectancy")
#plot(fcast)

# for total and lung cancer
subset_total_lungcancer <- cigarette[ , c(2,5)]
subset_total_lungcancer_ts <- ts(subset_total_lungcancer, start=c(1990,1), end=c(2016,2), frequency = 1)
View(subset_total_lungcancer_ts)

plot(subset_total_lungcancer_ts, xlab="Year", main="Graph")
fit13 <- auto.arima(subset_total_lungcancer_ts[,1], xreg=subset_total_lungcancer_ts[,2]); fit13
fcast1 <- forecast(fit13,xreg=subset_total_lungcancer_ts[,2], xlab = "Year", ylab = "Total", main = "Dynamic Regression for Total VS Lung Cancer")
plot(fcast1)

# for total and cancer
subset_total_cancer <- cigarette[ , c(2,4)]
subset_total_cancer_ts <- ts(subset_total_cancer, start=c(1990,1), end=c(2016,2), frequency = 1)
View(subset_total_cancer_ts)

plot(subset_total_cancer_ts, xlab="Year", main="Graph")
fit14 <- auto.arima(subset_total_cancer_ts[,1], xreg=subset_total_cancer_ts[,2]); fit14
fcast2 <- forecast(fit14,xreg=subset_total_cancer_ts[,2])
plot(fcast2, xlab = "Year", ylab = "Total", main = "Dynamic Regression for Total VS Cancer")




