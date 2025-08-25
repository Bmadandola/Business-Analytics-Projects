# Load library
library(prophet)

# Read data
df <- read.csv("C:/Users/biola/Downloads/example_air_passengers (1).csv")
df$ds <- as.Date(df$ds)  # Make sure date format is correct

# Fit model
m <- prophet(df)

# Future dataframe: forecast 365 days
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)

# Plot components
prophet_plot_components(m, forecast)

# View forecast
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])
