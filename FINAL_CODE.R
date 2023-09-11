library(tidyverse)   # For data manipulation
library(ggplot2)     # For data visualization
library(forecast)    # For time series forecasting and ARIMA
library(tseries)

#*********************************************************************************************
data <- read.csv("data/data.csv")
data

cases <- data$Reported.cases.Leishmaniasis
cases
yearz <- 2005:2021
yearz

summary(cases)

#*********************************************************************************************

#********************************************************************************************
#Splitting the data

train_data <- cases[1:13]
train_data
test_yearz <- 2005:2017

test_data <- cases[14:17]
test_data


#********************************************************************************************

#Time series plot
windows(width = 8, height = 6)

plot(test_yearz, train_data, xlim = c(2005, 2018), ylim = c(0, max(train_data)), type = "o", xlab = "Year", ylab = "Cases")

#Plotting ACF and PACF
acf(cases, lag.max = 100)

pacf(cases, lag.max = 100)

#**********************************************************************************************

# From the graphs above, the data is not stationary. Carrying out Differencing. We will Test first using ADF test
adf_test <- adf.test(train_data)
adf_test



#If your data is not stationary, apply differencing to make it stationary.
original_data <- train_data
order_of_differencing <- 0
adf_p_value <- 1

while (adf_p_value >= 0.05) {  # Check for stationarity using ADF test
  if (order_of_differencing > 0) {
    differenced_data <- diff(original_data, differences = order_of_differencing)
  } else {
    differenced_data <- original_data
  }
  
  adf_result <- adf.test(differenced_data)
  adf_p_value <- adf_result$p.value
  
  if (adf_p_value >= 0.05) {
    order_of_differencing <- order_of_differencing + 1
    original_data <- differenced_data
  }
}

cat("Order of differencing:", order_of_differencing, "\n")
cat("Final ADF p-value:", adf_p_value, "\n")




acf(original_data, lag.max=100)
pacf(original_data, lag.max=100)

#***************************************************************************************************************

#Looking for the best model

#model 1 ARIMA(0,4,1)
fit1 = arima(train_data, order = c(2,2,3))
tsdiag(fit1)
summary(fit1)

#model 2 ARIMA(1,4,0)

fit2 = arima(train_data, order = c(0,2,3))
tsdiag(fit2)
summary(fit2)

#model 3 ARIMA(2,2,0)
fit3 = arima(train_data, order = c(2,2,0))
tsdiag(fit3)
summary(fit3)


#model 3 auto
library(forecast)
fit3 = auto.arima(train_data)
tsdiag(fit3)
summary(fit3)

#**************************************************************************************************************************

#Conclusion

x_limits <- c(2005, 2024)

plot(test_yearz, train_data, xlim =x_limits , ylim = c(-5000, 8000), type = "o", xlab = "Year", ylab = "Cases")
lines(test_yearz, train_data, type="l" )
lines(test_yearz, train_data-fit3$residuals, type="l", col="red")


#***************************************************************************************************************************

# forecast for 4 steps ahead
forecast = predict(fit3, n.ahead=4)
forecast
forecasted_values <- forecast$pred
forecasted_values

# Ensure you have the last observed value from your original time series
last_observed_value <- cases[length(train_data)]
last_observed_value

# Initialize a vector to store recovered predictions
recovered_predictions <- numeric(length(forecasted_values))
recovered_predictions

# Reverse the differencing operation
for (i in 1:length(forecasted_values)) {
  recovered_predictions[i] <- last_observed_value + forecasted_values[i]
  last_observed_value <- recovered_predictions[i]
}
recovered_predictions
# Now, 'recovered_predictions' contains the predictions in the original scale

forecasted_years <- 2018:2021
lines(forecasted_years, recovered_predictions, type = "o", col = "red")
lines(forecasted_years, recovered_predictions-1.96*forecast$se, col="blue")
lines(forecasted_years, recovered_predictions+1.96*forecast$se, col="blue")
# Customize the x-axis ticks to increment by 1
axis(1, at = seq(2005, 2024, by = 1))


#Calculating model accuracy

MAPE <- mean(abs((test_data - recovered_predictions) / test_data)) * 100
MAPE

RMSE <- sqrt(mean((test_data - recovered_predictions)^2))
RMSE

























