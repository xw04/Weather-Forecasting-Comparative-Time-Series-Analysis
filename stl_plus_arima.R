# STL+ARIMA was implemented but excluded from final comparison due to ADF/ACF violations


# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)

# Step 1: Load and preprocess data
data <- read.csv("weather_cleaned_dataset1.csv")
data$date <- as.Date(data$datetime, format = "%Y-%m-%d")

# Step 2: Plot raw temperature time series
ggplot(data, aes(x = date, y = temp)) +
  geom_line() +
  labs(title = "Daily Temperature Time Series", x = "Date", y = "Temperature (°C)") +
  theme_minimal()

# Step 3: Aggregate to weekly average temperature
data_weekly <- data %>%
  mutate(week = format(date, "%Y-%U")) %>%
  group_by(week) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  ungroup()

# Convert to time series (remove smoothed MA and use STL instead)
start_year <- as.integer(substr(data_weekly$week[1], 1, 4))
start_week <- as.integer(substr(data_weekly$week[1], 7, 8))

ts_weekly <- ts(data_weekly$avg_temp, frequency = 52, start = c(start_year, start_week))

# Step 4: ADF Test (stationarity check)
cat("ADF Test on weekly temperature series:\n")
print(adf.test(na.omit(ts_weekly)))

# Step 5: Train-test split (80/20)
n <- length(ts_weekly)
train_size <- floor(0.8 * n)

train_ts <- window(ts_weekly, end = c(start_year + ((train_size - 1) %/% 52),
                                      ((train_size - 1) %% 52 + 1)))
test_ts <- window(ts_weekly, start = c(start_year + (train_size %/% 52),
                                       (train_size %% 52 + 1)))

# Step 6: STL + ARIMA modeling using stlm
stlm_model <- stlm(train_ts, s.window = "periodic", method = "arima")
cat("\nSTL + ARIMA Model Summary:\n")
print(summary(stlm_model$model))

plot(stlm_model$stl)

# Step 7: Forecast
h <- length(test_ts)
forecast_stlm <- forecast(stlm_model, h = h)

# Step 8: Plot forecast vs actual
autoplot(forecast_stlm) +
  autolayer(test_ts, series = "Actual") +
  labs(title = "STL + ARIMA Forecast vs Actual (Weekly)",
       x = "Week", y = "Temperature (°C)") +
  theme_minimal()

# Step 9: RMSE and MAE Calculation
rmse_stlm <- sqrt(mean((forecast_stlm$mean - test_ts)^2, na.rm = TRUE))
mae_stlm <- mean(abs(forecast_stlm$mean - test_ts), na.rm = TRUE)

cat("\nSTL+ARIMA RMSE:", rmse_stlm, "\n")
cat("STL+ARIMA MAE:", mae_stlm, "\n")

# Step 10: Residual diagnostics
residuals_stlm <- residuals(stlm_model$model)

par(mfrow = c(2, 2))
plot(residuals_stlm, main = "Residuals over Time", ylab = "Residuals", xlab = "Time")
hist(residuals_stlm, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")
Acf(residuals_stlm, main = "ACF of Residuals")
Pacf(residuals_stlm, main = "PACF of Residuals")
par(mfrow = c(1, 1))

# Step 11: Normality & Autocorrelation tests
cat("\nShapiro-Wilk Test for Normality:\n")
print(shapiro.test(residuals_stlm))

cat("\nLjung-Box Test for Residual Autocorrelation:\n")
print(Box.test(residuals_stlm, lag = 20, type = "Ljung-Box"))
