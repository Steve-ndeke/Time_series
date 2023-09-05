library(tidyverse)   # For data manipulation
library(ggplot2)     # For data visualization
library(forecast)    # For time series forecasting and ARIMA
library(tseries)

data <- read.csv("data/data.csv")
data

cases <- data$Reported.cases.Leishmaniasis
cases
yearz <- 2005:2021
yearz

#Time series plot
windows(width = 8, height = 6)

plot(yearz, cases, xlim = c(2005, 2021), ylim = c(0, max(cases)), type = "o", xlab = "Year", ylab = "Cases")

#Plotting ACF and PACF
acf(cases, lag.max = 100)

pacf(cases, lag.max = 100)


differenced_data1 <- diff(cases, 1,1)
differenced_data1
yearz <- 2006:2021
yearz


plot(yearz, differenced_data1, xlim = c(2006, 2021), ylim = c(-300, max(cases)), type = "o", xlab = "Year", ylab = "Cases")


differenced_data2 <- diff(cases, 1,2)
differenced_data2
length(differenced_data2)

yearz <- 2007:2021
yearz
plot(yearz, differenced_data2, xlim = c(2007, 2021), ylim = c(min(cases), max(cases)), type = "o", xlab = "Year", ylab = "Cases")

differenced_data3 <- diff(cases, 1,3)
differenced_data3
length(differenced_data3)



yearz <- 2008:2021
yearz

plot(yearz, differenced_data3, xlim = c(2008, 2021), ylim = c(-3000, max(cases)), type = "o", xlab = "Year", ylab = "Cases")

adf_test <- adf.test(differenced_data3)
adf_test



differenced_data4 <- diff(cases, 1,4)
differenced_data4
length(differenced_data4)

yearz <- 2009:2021
yearz
plot(yearz, differenced_data4, xlim = c(2009, 2021), ylim = c(-5000, max(cases)), type = "o", xlab = "Year", ylab = "Cases")

adf_test <- adf.test(differenced_data4)
adf_test



#If your data is not stationary, apply differencing to make it stationary.
original_data <- cases
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






acf(differenced_data4, lag.max=100)
pacf(differenced_data4, lag.max=100)



#model 1 ARIMA(0,4,1)
fit1 = arima(cases, order = c(0,4,1))
tsdiag(fit1)
summary(fit1)

#model 2 ARIMA(1,4,0)

fit2 = arima(cases, order = c(1,4,0))
tsdiag(fit2)
summary(fit2)


#model 3 auto
library(forecast)
fit3 = auto.arima(cases)
tsdiag(fit3)
summary(fit3)



#Conclusion

x_limits <- c(2005, 2024)

plot(yearz, cases, xlim =x_limits , ylim = c(-400, 8000), type = "o", xlab = "Year", ylab = "Cases")
lines(yearz, cases, type="l" )
lines(yearz, cases-fit1$residuals, type="l", col="red")




# forecast for 10 steps ahead
forecast = predict(fit1, n.ahead=3)
forecast
forecasted_values <- forecast$pred
forecasted_values

# Ensure you have the last observed value from your original time series
last_observed_value <- cases[length(cases)]
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

forecasted_years <- 2022:2024
lines(forecasted_years, recovered_predictions, type = "o", col = "red")
lines(forecasted_years, recovered_predictions-1.96*forecast$se, col="blue")
lines(forecasted_years, recovered_predictions+1.96*forecast$se, col="blue")
# Customize the x-axis ticks to increment by 1
axis(1, at = seq(2005, 2024, by = 1))





























