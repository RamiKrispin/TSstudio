## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(list(menu.graphics = FALSE, scipen=99, digits= 3))
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

## ------------------------------------------------------------------------
# install.packages("TSstudio")
library(TSstudio)
packageVersion("TSstudio")
# Function documentation
?ts_plot

## ------------------------------------------------------------------------
# Loading the USgas dataset
data("USgas")

ts_info(USgas)


## ----fig.height=5, fig.width=10------------------------------------------
ts_plot(USgas)

## ----fig.height=5, fig.width=10------------------------------------------
# Setting the plot titles
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet")

## ----fig.height=5, fig.width=10------------------------------------------
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet",
        line.mode = "lines+markers",
        width = 3,
        color = "green")

## ----fig.height=5, fig.width=10------------------------------------------
ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet",
        line.mode = "lines+markers",
        width = 1,
        color = "#66C2A5",
        Xgrid = TRUE,
        Ygrid = TRUE,
        slider = TRUE)

## ----fig.height=5, fig.width=10, message=FALSE, warning=FALSE------------
library(TSstudio)
library(xts)
library(zoo)
library(quantmod)
# Loading the stock price of key technology companies:
tckrs <- c("GOOGL", "FB", "AAPL", "MSFT")
getSymbols(tckrs, 
           from = "2013-01-01",
           src = "yahoo"
           )

# Creating a multiple time series object
closing <- cbind(AAPL$AAPL.Close, FB$FB.Close, GOOGL$GOOGL.Close, MSFT$MSFT.Close)
names(closing) <- c("Apple", "Facebook", "Google", "Microsoft")

ts_info(closing)

## ----fig.height=5, fig.width=10------------------------------------------
# Plot each series in a sepreate (default option)
ts_plot(closing,
        title = "Top Technology Companies Stocks Prices Since 2013", 
        type = "multiple")


# All the series in one plot
ts_plot(closing, 
        title = "Top Technology Companies Stocks Prices Since 2013",
        type = "single")



## ------------------------------------------------------------------------
data(US_indicators)
str(US_indicators)

## ----fig.height=5, fig.width=10------------------------------------------
ts_plot(US_indicators)

