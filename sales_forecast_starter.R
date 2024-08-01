# Weekly sales forecasting starter:
#  Install forecast-package first.
library(tidyverse)
library(forecast)

# historical records from multiple stores
fsales <- "https://raw.githubusercontent.com/multidis/hult-inter-bus-reports-r/main/forecasting/sales_weekly.csv"
sales <- read_csv(fsales)
sales

# latest (current) week
nweek_now <- max(sales$Week)

# sales in store 3 over the most recent quarter;
# counting 52 weeks/year, 13 weeks/quarter
sales %>%
  filter(Store == 3) %>%
  subset(Week > nweek_now - 13, Week <= nweek_now) %>%
  summarise(TotalSales = sum(Weekly_Sales))

# check forecast accuracy for the most recent quarter
sales_hist <- sales %>%
  filter(Store == 3) %>%
  subset(Week <= nweek_now - 13)
sales_last <- sales %>%
  filter(Store == 3) %>%
  subset(Week > nweek_now - 13)

# time series with annual periodicity to account seasonality
sales_hist_ts <- ts(sales_hist$Weekly_Sales, frequency = 52)
autoplot(sales_hist_ts)

# ARIMA: Auto-Regressive Integrated Moving Average
# methodological details:
#  https://otexts.com/fpp3/arima.html
arima_model <- auto.arima(sales_hist_ts, seasonal.test = "seas")

# forecast horizon to match most recent quarter
arima_pred <- forecast(arima_model, h = 13)

# note: confidence intervals (lower, upper) are available as well
sales_pred_eval <- data.frame(predicted = as.numeric(arima_pred$mean),
                              actual = sales_last$Weekly_Sales,
                              Week = sales_last$Week)
sales_pred_eval %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = predicted, col = "red")) +
  geom_line(aes(y = actual, col = "green"))

# time series for the entire dataset to use for future forecasting
sales_ts <- ts(sales %>% filter(Store == 3) %>% select(Weekly_Sales), frequency = 52)
autoplot(sales_ts)

# forecasting for the next 2 months
arima_model <- auto.arima(sales_ts, seasonal.test = "seas")
arima_pred <- forecast(arima_model, h = 8)
arima_pred$mean
