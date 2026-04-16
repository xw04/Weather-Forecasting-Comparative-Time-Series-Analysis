library(dplyr)
library(ggplot2)
library(forecast)

prophet_metrics <- read.csv("prophet_metrics.csv")
sarima_metrics <- read.csv("sarima_metrics.csv")


model_comparison <- data.frame(
  Model = c("Prophet", "SARIMA"),
  RMSE = c(prophet_metrics$RMSE, sarima_metrics$RMSE),
  MAE = c(prophet_metrics$MAE, sarima_metrics$MAE),
  MAPE = c(prophet_metrics$MAPE, sarima_metrics$MAPE)
)

# Convert metrics to numeric to avoid potential type issues
model_comparison$RMSE <- as.numeric(model_comparison$RMSE)
model_comparison$MAE <- as.numeric(model_comparison$MAE)
model_comparison$MAPE <- as.numeric(model_comparison$MAPE)

# Display the comparison table
print(model_comparison)

# Find the best model based on RMSE
best_model <- model_comparison %>%
  arrange(RMSE) %>%
  slice(1) %>% 
  pull(Model)

cat("Best Model based on RMSE:", best_model, "\n")

ggplot(model_comparison, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison by RMSE",
       x = "Model",
       y = "RMSE (Root Mean Square Error)") +
  theme_minimal() +
  theme(legend.position = "none")

write.csv(model_comparison, "model_comparison.csv", row.names = FALSE)

cat("\nLoading the best model (", best_model, ") for visualization...\n", sep = "")

data <- read.csv("weather_cleaned_dataset1.csv")
data$date <- as.Date(data$datetime, format = "%Y-%m-%d")

if (best_model == "Prophet") {

  prophet_model <- readRDS("prophet_model.rds")
  

  future <- make_future_dataframe(prophet_model, periods = 52, freq = 'week')
  

  forecast <- predict(prophet_model, future)
  

  prophet_plot <- plot(prophet_model, forecast)
  print(prophet_plot + 
          labs(title = "Prophet: 1-Year Weather Forecast"))
  
} else if (best_model == "SARIMA") {

  sarima_model <- readRDS("sarima_model.rds")
  

  sarima_forecast <- forecast(sarima_model, h = 52)
  

  autoplot(sarima_forecast) + 
    labs(title = "SARIMA: 1-Year Weather Forecast")
}


cat("\n====== Best Model Summary ======\n")
cat("Model:", best_model, "\n")
cat("RMSE:", model_comparison$RMSE[model_comparison$Model == best_model], "\n")
cat("MAE:", model_comparison$MAE[model_comparison$Model == best_model], "\n")
cat("MAPE:", model_comparison$MAPE[model_comparison$Model == best_model], "%\n")

cat("\nRecommendation for Forecast Model:\n")
cat("Based on forecast error comparison, the", best_model, "model is recommended for forecasting\n")
cat("future temperature values. This model provided the lowest prediction error on the test set.\n")


# FORECASTING WITH THE BEST MODEL

future_periods <- 52

if (best_model == "Prophet") {
  # Refit on full data
  full_prophet_model <- prophet(
    prophet_data,  # Use full dataset
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE,
    daily.seasonality = FALSE,
    seasonality.mode = 'multiplicative'
  )
  
  # Make future predictions
  future <- make_future_dataframe(prophet_model, periods = 52, freq = 'week') 
  forecast <- predict(prophet_model, future)
  
  # Plot full forecast (history + future)
  plot(prophet_model, forecast) +
    labs(title = "Prophet: 1-Year Future Forecast (2024-2025)",
         x = "Date",
         y = "Temperature (°C)") +
    theme_minimal()
  
  # Save the best model
  saveRDS(full_prophet_model, file = "best_model.rds")
  
} else if (best_model == "SARIMA") {
  # Refit on full data
  full_sarima_model <- Arima(ts_temp_weekly, model = sarima_model)
  future_forecast <- forecast(full_sarima_model, h = future_periods)
  
  # Plot forecast
  autoplot(future_forecast) +
    labs(title = "SARIMA: 1-Year Weather Forecast", y = "Temperature (°C)")
  
  # Save the best model
  saveRDS(full_sarima_model, file = "best_model.rds")
}