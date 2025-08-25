Load required libraries
library(lubridate)
library(forecast)

# Read data
df <- read.csv("C:/Users/biola/Downloads/example_air_passengers (1).csv")
df$ds <- as.Date(df$ds)

# Convert to time series object
passenger_ts <- ts(df$y, start = c(1949, 1), frequency = 12)

# Plot the time series
png("ts_plot.png")
plot(passenger_ts, main = "Monthly Air Passengers (1949-1960)", ylab = "Passengers", xlab = "Year", col.main = "darkblue")
dev.off()

# Decompose the time series
decomposed <- decompose(passenger_ts)

# Plot decomposition
png("ts_decomposition.png")
plot(decomposed)
dev.off()

# Fit ARIMA model
fit <- auto.arima(passenger_ts)

# Forecast next 24 months
forecast_values <- forecast(fit, h = 24)

# Plot forecast
png("ts_forecast.png")
plot(forecast_values, main = "ARIMA Forecast of Air Passengers")
dev.off()

# Print model summary and forecast
print(summary(fit))
print(forecast_values)

