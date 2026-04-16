library(dplyr)
library(prophet)
library(ggplot2)
library(lubridate)
library(forecast)
library(tseries)

data <- read.csv("weather_cleaned_dataset1.csv")

data$date <- as.Date(data$datetime, format = "%Y-%m-%d")

# Aggregate to weekly data (average temperature per week)
data_weekly <- data %>%
  mutate(
    week = format(date, "%Y-%U"),
    date = as.Date(paste0(week, "-1"), "%Y-%U-%u")
  ) %>%
  group_by(week, date) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop")

# Plot weekly average temperature
ggplot(data_weekly, aes(x = date, y = avg_temp)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Weekly Average Temperature Time Series",
    x = "Year",
    y = "Average Temperature (°C)"
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal()

# Create data splits for both models (80% train, 20% test)
n <- nrow(data_weekly)
train_size <- floor(0.8 * n)

# Get start information for SARIMA time series
start_year <- as.integer(substr(data_weekly$week[1], 1, 4))
start_week <- as.integer(substr(data_weekly$week[1], 6, 7))
if (is.na(start_week)) {
  start_week <- 1
}

# PART 2: PROPHET MODEL ANALYSIS
prophet_data <- data_weekly %>%
  select(date, avg_temp) %>%
  rename(ds = date, y = avg_temp)

prophet_data <- prophet_data[complete.cases(prophet_data), ]  

prophet_train <- prophet_data[1:train_size, ]
prophet_test <- prophet_data[(train_size + 1):n, ]

prophet_model <- prophet(
  prophet_train,
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = 'multiplicative'
)

future_dates <- make_future_dataframe(prophet_model, periods 
                                      = nrow(prophet_test), freq = 'week')
forecast <- predict(prophet_model, future_dates)

prophet_forecast <- tail(forecast, nrow(prophet_test))

# Calculate residuals
prophet_residuals <- prophet_test$y - prophet_forecast$yhat

# Plot 1: Residuals over Time
ggplot(data.frame(residuals = prophet_residuals, date = prophet_test$ds), 
       aes(x = date, y = residuals)) +
  geom_line() +
  labs(title = "Prophet: Residuals over Time", x = "Date", y = "Residuals") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

# Plot 2: Histogram of residuals
ggplot(data.frame(residuals = prophet_residuals), aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Prophet: Histogram of Residuals", x = "Residuals", y = "Frequency")

# Plot 3: ACF of residuals
Acf(prophet_residuals, main = "Prophet: ACF of Residuals")

# Plot 4: PACF of residuals
Pacf(prophet_residuals, main = "Prophet: PACF of Residuals")

# Residual hypothesis tests
# Shapiro-Wilk test for normality
prophet_shapiro <- shapiro.test(prophet_residuals)
cat("\nProphet: Shapiro-Wilk Test for Normality:\n")
print(prophet_shapiro)

# Ljung-Box test for residual autocorrelation
prophet_lb <- Box.test(prophet_residuals, lag = 20, type = "Ljung-Box")
cat("\nProphet: Ljung-Box Test for Residual Autocorrelation:\n")
print(prophet_lb)

# Calculate evaluation metrics
prophet_actual <- prophet_test$y
prophet_predicted <- prophet_forecast$yhat

# Calculate RMSE, MAE, MAPE
prophet_rmse <- sqrt(mean((prophet_actual - prophet_predicted)^2))
prophet_mae <- mean(abs(prophet_actual - prophet_predicted))
prophet_mape <- mean(abs((prophet_actual - prophet_predicted) 
                         / prophet_actual)) * 100

cat("\n=== Prophet Model Performance Summary ===\n")
cat("Prophet RMSE:", prophet_rmse, "\n")
cat("Prophet MAE:", prophet_mae, "\n")
cat("Prophet MAPE:", prophet_mape, "%\n")

# Save metrics for comparison
write.csv(data.frame(RMSE = prophet_rmse, MAE = prophet_mae, MAPE = prophet_mape), 
          "prophet_metrics.csv", row.names = FALSE)
saveRDS(prophet_model, file = "prophet_model.rds")
saveRDS(forecast, file = "prophet_forecast.rds")