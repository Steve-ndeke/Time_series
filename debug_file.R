#Libraries

library(tidyverse)   # For data manipulation
library(ggplot2)     # For data visualization
library(forecast)    # For time series forecasting and ARIMA
library(tseries)



data <- read.csv(("data/data.csv"))
data
length(data)
head(data)



cases <- ts(data[,6], frequency = 1, start = 2005)
cases
length(cases)

first_13 <- cases[1:17]
first_13


layout(1:3)  # This will show all three plots in the same window
plot.ts(first_13)
acf(first_13, ylim=c(-1, 1))
pacf(first_13, ylim=c(-1, 1))




diff1 <- diff(first_13, difference=1)  # First-order differencing
diff2 <- diff(first_13, difference=2)
diff3 <- diff(first_13, difference=3)



layout(1:2)
acf(diff2, lag.max=20, ylim=c(-1, 1)) # Plot second-order differencing
pacf(diff2, lag.max=20, ylim=c(-1, 1))



#testing for stationarity
adf.test(diff3, alternative = "stationary")


#fitting arima models with possible combinations of pdq

ar1 <- arima(diff2, order=c(2, 3, 1))
ar1
ar2 <- arima(diff2, order=c(2, 3, 2))
ar2
ar3 <- arima(diff2, order=c(2, 3, 1))
ar3


#fitting residuals
layout(1:2)  
acf(ar2$residuals, lag.max=20, ylim=c(-1, 1))
pacf(ar2$residuals, lag.max=20, ylim=c(-1, 1))

#H0: residuals are randomly distributed
Box.test(ar1$residuals, type="Ljung-Box")
Box.test(ar2$residuals, type="Ljung-Box")
Box.test(ar3$residuals, type="Ljung-Box")


#Forcasting
forecast <- predict(ar1, n.ahead=14)
forecast
length(diff2)
upper <- forecast$pred + 1.96*forecast$se
lower <- forecast$pred - 1.96*forecast$se
ts.plot(diff2,forecast$pred, col=c("black","red"))
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")





ts.plot(diff2,forecast$pred, col=c("black","red"), xlim=c(2017, 2021), ylim=c(1000, 2000))
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")




ar4 <- arima(diff2, order=c(2, 3, 3), seasonal=list(order=c(0,0,1), period=12))

forecast <- predict(ar4, n.ahead=60)
upper <- forecast$pred + 1.96*forecast$se
lower <- forecast$pred - 1.96*forecast$se
ts.plot(diff2,forecast$pred, col=c("black","red"))
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")




























