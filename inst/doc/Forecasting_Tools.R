## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
# install.packages("TSstudio")
library(TSstudio)
packageVersion("TSstudio")

## ----fig.height=5, fig.width=10------------------------------------------
# Loading the data
data("USgas", package = "TSstudio")

ts_info(USgas)

ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet"
        )

## ------------------------------------------------------------------------
USgas_splits <- ts_split(ts.obj = USgas, sample.out = 12)
train <- USgas_splits$train
test <- USgas_splits$test

ts_info(train)
ts_info(test)

## ----fig.height=5, fig.width=10------------------------------------------
library(forecast)

# Setting the forecasting horizon to the length of the testing partition
h <- length(test)

md1 <- auto.arima(train)
fc1 <- forecast(md1, h = h)

test_forecast(actual = USgas, forecast.obj = fc1, test = test)

## ----fig.height=5, fig.width=10------------------------------------------
md2 <- auto.arima(USgas)
fc2 <- forecast(md2, h = 60)

plot_forecast(fc2)

## ----fig.height=5, fig.width=10------------------------------------------
check_res(md2)

## ----message=FALSE, warning=FALSE, fig.height=5, fig.width=10------------
USgas_backtesting <- ts_backtesting(ts.obj = USgas,
                                    models = "abehntw",
                                    periods = 6,
                                    error = "MAPE",
                                    window_size = 12,
                                    h = 60, 
                                    plot = FALSE)

## ---- fig.height=10, fig.width=10----------------------------------------
USgas_backtesting$summary_plot

## ------------------------------------------------------------------------
USgas_backtesting$leaderboard

## ---- fig.height=5, fig.width=10-----------------------------------------
plot_forecast(USgas_backtesting$Forecast_Final$nnetar)
plot_forecast(USgas_backtesting$Forecast_Final$bsts)
plot_forecast(USgas_backtesting$Forecast_Final$hybrid)

