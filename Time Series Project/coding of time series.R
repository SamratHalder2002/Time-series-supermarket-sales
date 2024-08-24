rm(list = ls())
library(dplyr)
data <- read.csv("Dataset- Superstore (2015-2018).csv")
head(data)
dim(data)

unique(data$Category)
table(data$Category)
# use only relevant columns
data <- data.frame(data$Order.Date,data$Sales,data$Category)

new <- subset(data, data.Category == "Office Supplies")
df_office <- new[-c(3)]
head(df_office)

# Check for missing values in each column of df_office
sapply(df_office, function(x) sum(is.na(x)))

df_office$data.Order.Date <- as.Date(new$data.Order.Date, format = "%Y/%m/%d")

# Order the data by date
df_ordered <- df_office[order(df_office$data.Order.Date), ]

df_ordered$Year <- format(df_ordered$data.Order.Date, "%Y")
df_ordered$Month <- format(df_ordered$data.Order.Date, "%m")

df_ordered$Month_Year <- paste(df_ordered$Month, df_ordered$Year, sep = ", ")

# Reorder columns to include Month_Year after data.Order.Date
df_ordered <- df_ordered[, c("data.Order.Date", "Month_Year", "data.Sales")]

new_data <- df_ordered[-1]

monthly_totals <- new_data  %>%
  group_by(Month_Year) %>%
  summarise(Total_Sales = sum(data.Sales)/30)

plot.ts(monthly_totals$Total_Sales)

library(tseries)
ts_data <- ts(monthly_totals$Total_Sales, start = c(2014, 1), frequency = 48)
adf_result <- adf.test(ts_data)
adf_result

## this is non stationary data

##log transformation 
# Apply log transformation to stabilize variance
log_ts_data <- log(ts_data)

# Plot the log-transformed data
plot.ts(log_ts_data, main = "Log-transformed Time Series")

#In this case, we can see the plot is not a 
#forward trend in the data. So, take a log transform 
#is not a solution to make a time-series stationary

## Differencing
# Apply first differencing to remove trend
diff_ts_data <- diff(ts_data, differences = 1)

# Plot the differenced data
plot.ts(diff_ts_data, main = "Differenced Time Series")

# Perform ADF test on the differenced data
adf_result_diff <- adf.test(diff_ts_data)
print(adf_result_diff)
# this makes the data stationary 


## Detrending 
# Fit a linear model to the time series to get the trend
trend <- lm(ts_data ~ time(ts_data))

# Subtract the trend from the original data to detrend it
detrended_ts_data <- ts_data - trend$fitted.values

# Plot the detrended data
plot.ts(detrended_ts_data, main = "Detrended Time Series")

# Perform ADF test on the detrended data
adf_result_detrend <- adf.test(detrended_ts_data)
print(adf_result_detrend)
# This does not makes the data sationary
##################################################
# Define the size of the test set (12 months for forecasting)
forecast_steps <- 12

# Apply differencing to make the data stationary
diff_ts_data <- diff(ts_data, differences = 1)

# Split the differenced data into training and test sets
train_data <- diff_ts_data[1:(length(diff_ts_data) - forecast_steps)]
test_data <- diff_ts_data[(length(diff_ts_data) - forecast_steps + 1):length(diff_ts_data)]
########################################################
# ARIMA
# Fit an ARIMA model on the training data
fit <- auto.arima(train_data)

# Forecast future values (h = forecast_steps) on differenced data
forecasts <- forecast(fit, h = forecast_steps)
forecast_ts <- forecasts$mean

# Convert forecasts back to the original scale by reversing the differencing
last_observed_value <- ts_data[length(ts_data) - forecast_steps]
forecast_ts_original_scale <- cumsum(c(last_observed_value, forecast_ts))[-1]

# Calculate accuracy metrics
actual_values <- ts_data[(length(ts_data) - forecast_steps + 1):length(ts_data)]

# Define time indices for plotting
time_indices <- time(ts_data)
forecast_indices <- time(ts_data)[(length(ts_data) - forecast_steps + 1):length(ts_data)]

# Plot the data
plot(time_indices, ts_data, type = "l", col = "black", lwd = 2, 
     xlab = "Time", ylab = "Total Sales", main = "Train, Test, and Forecast Data")

# Add training data
lines(time_indices[1:(length(ts_data) - forecast_steps)], ts_data[1:(length(ts_data) - forecast_steps)], col = "blue", lwd = 2)

# Add test data
lines(forecast_indices, actual_values, col = "green", lwd = 2)

# Add forecasted data
forecast_start <- time(ts_data)[length(ts_data) - forecast_steps + 1]
forecast_end <- time(ts_data)[length(ts_data)]
lines(seq(forecast_start, forecast_end, length.out = forecast_steps), forecast_ts_original_scale, col = "red", lwd = 2)

# Add legend
legend("topleft", legend = c("Full Data", "Training Data", "Test Data", "Forecasts"), col = c("black", "blue", "green", "red"), lty = 1, lwd = 2)


# Mean Absolute Error (MAE)
mae <- mean(abs(actual_values - forecast_ts_original_scale))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((actual_values - forecast_ts_original_scale)^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((actual_values - forecast_ts_original_scale) / actual_values)) * 100

# Mean Absolute Scaled Error (MASE)
# Calculate the naive forecast MAE (using last value as forecast)
naive_forecast <- ts_data[(length(ts_data) - forecast_steps)]
mae_naive <- mean(abs(actual_values - naive_forecast))
mase <- mae / mae_naive

# Print accuracy metrics
print(paste("MAE:", round(mae, 2)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAPE:", round(mape, 2), "%"))
print(paste("MASE:", round(mase, 2)))

######################################################
# AR

# Fit AR model
# Here, we specify AR(1) model, you can adjust the order as needed
fit_ar <- Arima(train_data, order = c(1, 0, 0))

# Forecast future values
forecasts <- forecast(fit_ar, h = forecast_steps)
forecast_ts <- forecasts$mean

# Calculate accuracy metrics
actual_values <- test_data

# Mean Absolute Error (MAE)
mae <- mean(abs(actual_values - forecast_ts))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((actual_values - forecast_ts)^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((actual_values - forecast_ts) / actual_values)) * 100

# Print accuracy metrics
print(paste("MAE:", round(mae, 2)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAPE:", round(mape, 2), "%"))

###############################################################
# MA
# Fit MA model
# Here, we specify MA(1) model, you can adjust the order as needed
fit_ma <- Arima(train_data, order = c(0, 0, 1))

# Forecast future values
forecasts <- forecast(fit_ma, h = forecast_steps)
forecast_ts <- forecasts$mean

# Calculate accuracy metrics
actual_values <- test_data

# Mean Absolute Error (MAE)
mae <- mean(abs(actual_values - forecast_ts))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((actual_values - forecast_ts)^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((actual_values - forecast_ts) / actual_values)) * 100

# Print accuracy metrics
print(paste("MAE:", round(mae, 2)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAPE:", round(mape, 2), "%"))

#############################################################################
# Fit ARMA model
# Here, we specify ARMA(1,1) model; adjust the orders (p, q) as needed
fit_arma <- Arima(train_data, order = c(1, 0, 1))

# Forecast future values
forecasts <- forecast(fit_arma, h = forecast_steps)
forecast_ts <- forecasts$mean

# Calculate accuracy metrics
actual_values <- test_data

# Mean Absolute Error (MAE)
mae <- mean(abs(actual_values - forecast_ts))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((actual_values - forecast_ts)^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((actual_values - forecast_ts) / actual_values)) * 100

# Print accuracy metrics
print(paste("MAE:", round(mae, 2)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAPE:", round(mape, 2), "%"))


