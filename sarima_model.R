setwd("C:\\Users\\User\\OneDrive\\Desktop")
data <- read.csv("weather_cleaned_dataset1.csv")
library(dplyr)
library(urca)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)

# Convert 'datetime' in date format
data$date <- as.Date(data$datetime, format = "%Y-%m-%d")

# Check structure
str(data)
head(data)

# Plot raw temperature time series
ggplot(data, aes(x = date, y = temp)) + geom_line() + labs(title = "Temperature Time Series", x = "Date", y = "Temperature (°C)")

# Aggregate to weekly data (average temperature per week)
data_weekly <- data %>%
  mutate(
    week = format(date, "%Y-%U"),  # Year-week (e.g., "2023-32")
    # Convert "YYYY-WW" to the first day of the week (Monday)
    date = as.Date(paste0(week, "-1"), "%Y-%U-%u")
  ) %>%
  group_by(week, date) %>%  # Group by both to keep the date
  summarise(avg_temp = mean(temp, na.rm = TRUE))  # Weekly average

# Plot weekly average temperature
ggplot(data_weekly, aes(x = date, y = avg_temp)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Weekly Average Temperature Time Series",
    x = "Year",
    y = "Average Temperature (°C)"
  ) +
  scale_x_date(
    date_labels = "%Y",  # Show only year (e.g., "2020")
    date_breaks = "1 year"  # One label per year
  ) +
  theme_minimal()

start_year <- as.integer(substr(data_weekly$week[1], 1, 4))  # Extract year
start_week <- as.integer(substr(data_weekly$week[1], 7, 8))  # Extract week

# Create time series object with weekly frequency
ts_temp_weekly <- ts(data_weekly$avg_temp, frequency = 52, start = c(start_year, start_week))

# ADF test on original weekly series
cat("ADF Test on original weekly temperature series:\n")
print(adf.test(ts_temp_weekly))

# ACF test on original monthly series
cat("ACF Test on original monthly temperature series:\n")
print(acf(ts_temp_weekly))

cat("Recommended number of trend differences (d):", ndiffs(ts_temp_weekly), "\n")
cat("Recommended number of seasonal differences (D):", nsdiffs(ts_temp_weekly), "\n")

# First-order differencing
diff_temp_weekly <- diff(ts_temp_weekly)

# Plot differenced series
plot(diff_temp_weekly, main = "Differenced Weekly Temperature", ylab = "Δ Temp", xlab = "Weeks")

# ADF test after differencing
cat("ADF Test after first differencing (weekly):\n")
print(adf.test(diff_temp_weekly))

# ACF and PACF plots
par(mfrow = c(1,2))
acf(diff_temp_weekly, main = "ACF of Differenced Weekly Temp")
pacf(diff_temp_weekly, main = "PACF of Differenced Weekly Temp")

# Reset layout
par(mfrow = c(1,1))

# Split data into training and test sets
n <- length(ts_temp_weekly)
train_size <- floor(0.8 * n)
train_ts <- window(ts_temp_weekly, end = c(start_year + ((train_size - 1) %/% 52), ((train_size - 1) %% 52 + 1)))
test_ts <- window(ts_temp_weekly, start = c(start_year + (train_size %/% 52), (train_size %% 52 + 1)))

# Fit Seasonal ARIMA (SARIMA) model using auto.arima
sarima_model <- auto.arima(train_ts, seasonal = TRUE)
summary(sarima_model)

# Model Diagnostic
# Residual diagnostics using custom layout to include PACF
cat("\nModel Residual Diagnostics for SARIMA Model:\n")

# Set up a 2x2 layout for plots
par(mfrow = c(2, 2))

# Plot 1: Residuals vs. Time
plot(residuals(sarima_model), 
     main = "Residuals over Time", 
     ylab = "Residuals", 
     xlab = "Time")

# Plot 2: Histogram of Residuals
hist(residuals(sarima_model), 
     breaks = 20, 
     main = "Histogram of Residuals", 
     xlab = "Residuals")

# Plot 3: ACF of Residuals (using forecast::Acf for consistent styling)
forecast::Acf(residuals(sarima_model), 
              main = "ACF of Residuals")

# Plot 4: PACF of Residuals (using forecast::Pacf for consistent styling)
forecast::Pacf(residuals(sarima_model), 
               main = "PACF of Residuals")

# Reset layout
par(mfrow = c(1, 1))

# Shapiro-Wilk test for normality
cat("\nShapiro-Wilk Test for normality:\n")
print(shapiro.test(residuals(sarima_model)))

# Ljung-Box test
cat("\nLjung-Box Test for residual autocorrelation:\n")
print(Box.test(residuals(sarima_model), lag = 20, type = "Ljung-Box"))

# Forecast and RMSE on test set
forecast_sarima <- forecast(sarima_model, h = length(test_ts))
rmse_val_sarima <- sqrt(mean((forecast_sarima$mean - test_ts)^2, na.rm = TRUE))
cat("\nRMSE on test set for SARIMA model:", rmse_val_sarima, "\n")

# MAE (Mean Absolute Error)
mae_sarima <- mean(abs(forecast_sarima$mean - test_ts), na.rm = TRUE)
cat("\nSARIMA MAE:", mae_sarima, "\n")

# MAPE (Mean Absolute Percentage Error)
mape_sarima <- mean(abs((forecast_sarima$mean - test_ts) / test_ts) * 100, na.rm = TRUE)
cat("\nSARIMA MAPE:", mape_sarima, "%\n")

write.csv(data.frame(RMSE = rmse_val_sarima, MAE = mae_sarima, MAPE = mape_sarima), "sarima_metrics.csv", row.names = FALSE)
saveRDS(sarima_model, file = "sarima_model.rds")