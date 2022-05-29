#fetch in data
fashion=read.csv("fashion.csv")
#change date to proper format
fashion$Invoice.Date=as.Date(fashion$Invoice.Date,'%d/%m/%Y')
#aggregate the amount of sales based on date
fashion2=aggregate(fashion$Total.Sales.Amount, by=list(fashion$Invoice.Date), sum)
#plot time series
ts1=ts(fashion2$x,c(2019,1),frequency=365)
plot(ts1,xlab="Total Sales",ylab="Year",main="Total Sales vs Time")
#plot distribution of sales
d1=density(fashion2$x)
plot(d1$x,d1$y,xlab="Total Sales",ylab="Density",type="l",main="Fashion Distribution")
range(fashion2$x)
x=seq(4586,252076,1)
y=dnorm(x,mean(fashion2$x),sd(fashion2$x))
lines(x,y,lty=2,col="blue")
#plot ACF and PACF
par(mfrow = c(1,2))
acf(ts1, main = "Total Sales")
pacf(ts1, main = "Total Sales")
#check for stationary
Box.test(ts1,lag=6,type="Ljung")

#perform transformation using difference of 1
tsstationary = diff(ts1, differences=1)
#plot the transformed data
par(mfrow=c(1,1))
plot(tsstationary)
#plot ACF and PACf of trtansformed data
par(mfrow=c(2,1))
acf(tsstationary,main="ACF test")
pacf(tsstationary,main="PACF test")

#check for stationarity of transformed data
library(tseries)
adf.test(tsstationary)
#Since the p-vaue is less than 0.05, we could confidently say that the differenced data is already stationary.
#ACF damped sine,pacf damped sine

#Using AUTO ARIMA to find the best model
library(forecast)
auto.arima(ts1, trace=TRUE)
#try to find best model
set.seed(1234)
fitARIMA <- arima(ts1, order=c(1,1,1))

#test the important of the parameters.
library(lmtest)
coeftest(fitARIMA)
confint(fitARIMA)
# All parameters are significant

# residuals must from the ARIMA model have no autocorrelation.
#plot ACF of residual
acf(fitARIMA$residuals)
library(FitAR)
#test for autocorrelation
Box.test(fitARIMA$residuals,lag=6,type="Ljung")
#no serial correlations
#plot normality graph
par(mfrow=c(1,1))
qqnorm(fitARIMA$residuals,main="Normality of Residual")
qqline(fitARIMA$residuals)
#check the mean and distribution
hist(fitARIMA$residuals,main="Residual Distribution")

#plot the forecast 30days ahead
par(mfrow=c(1,1))
library(forecast)
pre=predict(fitARIMA,n.ahead = 30)
futurVal <- forecast(fitARIMA,h=30, level=c(99.5))
plot(futurVal,main="Forecasts from ARIMA(1,1,1) 30days ahead")
#calculate the mean of prediction
mean(pre$pred)
