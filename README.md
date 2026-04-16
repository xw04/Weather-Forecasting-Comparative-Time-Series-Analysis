# Weather Forecasting: Comparative Time Series Analysis

## Project Overview
This project focuses on predicting future temperature trends using historical weather data. I implemented a rigorous statistical pipeline to evaluate and compare multiple forecasting architectures, ranging from classical econometric models (SARIMA) to modern additive models (Facebook Prophet).

## Tech Stack & Methodology
- **Language:** R
- **Core Libraries:** `prophet`, `forecast`, `tseries`, `tidyverse`, `lubridate`.
- **Workflow:** - **Data Preprocessing:** Daily data was aggregated into weekly averages to reduce noise and reveal underlying seasonal patterns.
    - **Stationarity Testing:** Applied the **Augmented Dickey-Fuller (ADF) Test** to determine the need for differencing.
    - **Model Selection:** Benchmarked four distinct models: SARIMA, Prophet, TBATS, and STL+ARIMA.

## Models Evaluated
1. **SARIMA (Seasonal Auto-Regressive Integrated Moving Average):** Configured using `auto.arima` with seasonal components to handle cyclical weather variations.
2. **Facebook Prophet:** Utilized for its ability to handle multiplicative seasonality and non-linear trends.
3. **STL + ARIMA:** Seasonal-Trend decomposition using LOESS followed by ARIMA on the seasonally adjusted series.
4. **TBATS:** Leveraged for handling complex seasonalities using trigonometric terms.

> **Note on Statistical Rigor:** While all four models were implemented, **STL+ARIMA** and **TBATS** were excluded from the final selection due to violations in ADF/ACF residual diagnostics, ensuring the final results are statistically valid.

## Performance Results
The models were evaluated on a 20% test set using standard error metrics.

| Model | RMSE | MAE | MAPE |
| :--- | :--- | :--- | :--- |
| **Facebook Prophet** | [Insert RMSE] | [Insert MAE] | [Insert MAPE]% |
| **SARIMA** | [Insert RMSE] | [Insert MAE] | [Insert MAPE]% |

**Key Finding:** The **[Insert Best Model Name]** provided the lowest prediction error, making it the recommended model for future temperature forecasting in this dataset.

## Model Diagnostics & Validation
To ensure the models were not just "guessing" but actually learning the data structure, I performed extensive residual analysis:
- **Ljung-Box Test:** Confirmed no remaining autocorrelation in the residuals.
- **Shapiro-Wilk Test:** Evaluated the normality of the error distribution.
- **ACF/PACF Analysis:** Visualized residuals to verify they approximate "White Noise".

## Future Forecast
The best-performing model was used to generate a **1-year future forecast (52 weeks)**, providing actionable insights into upcoming temperature trends.
