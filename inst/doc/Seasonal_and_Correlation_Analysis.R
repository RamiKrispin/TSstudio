## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(list(menu.graphics = FALSE, scipen=99, digits= 3))

## ------------------------------------------------------------------------
# install.packages("TSstudio")
library(TSstudio)
packageVersion("TSstudio")

## ----fig.height=5, fig.width= 7, message=FALSE, warning=FALSE------------
# Load the US monthly natural gas consumption
library(TSstudio)
data("USgas")

ts_info(USgas)

ts_plot(USgas,
        title = "US Natural Gas Consumption",
        Xtitle = "Year",
        Ytitle = "Billion Cubic Feet"
        )


## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "normal")


## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "cycle")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "box")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "all")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_seasonal(USgas, type = "all", palette_normal = "inferno", palette = "Accent")

## ----fig.height= 7, fig.width= 7, message=FALSE, warning=FALSE-----------
RColorBrewer::display.brewer.all() 

## ----fig.height= 5, fig.width= 7, message=FALSE, warning=FALSE-----------
n_col <- 128

img <- function(obj, nam) {
  image(1:length(obj), 1, as.matrix(1:length(obj)), col=obj, 
        main = nam, ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}

par(mfrow=c(5, 1), mar=rep(1, 4))
img(rev(viridis::viridis(n_col)), "viridis")
img(rev(viridis::magma(n_col)), "magma")
img(rev(viridis::plasma(n_col)), "plasma")
img(rev(viridis::inferno(n_col)), "inferno")
img(rev(viridis::cividis(n_col)), "cividis")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_heatmap(USgas)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_heatmap(USgas, color = "Reds")

## ----fig.height=6, fig.width= 7, message=FALSE, warning=FALSE------------
#install.packages("UKgrid")
library(UKgrid)

UKgrid_daily <- extract_grid(type = "tbl", aggregate = "daily")
head(UKgrid_daily)

ts_heatmap(UKgrid_daily, color = "BuPu")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
UKgrid_half_hour <- extract_grid(type = "xts", aggregate = NULL)
library(xts)
ts_info(UKgrid_half_hour)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_plot(UKgrid_half_hour)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_quantile(UKgrid_half_hour, period = NULL, title = "The UK National Grid Net Demand for Electricity - Quantile Plot")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_quantile(UKgrid_half_hour, 
            period = NULL, 
            lower = 0.15, 
            upper = 0.85,
            title = "The UK National Grid Net Demand for Electricity - 24 Hours Quantile Plot")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_quantile(UKgrid_half_hour, 
            period = "weekdays",
            title = "The UK National Grid Net Demand for Electricity - 24 Hours Quantile Plot by Weekdays")

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_quantile(UKgrid_half_hour, 
            period = "weekdays", 
            title = "The UK National Grid Net Demand for Electricity - 24 Hours Quantile Plot by Weekdays",
            n = 2)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_surface(USgas)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_polar(USgas)

## ----fig.height=4, fig.width= 7, message=FALSE, warning=FALSE------------
ts_lags(USgas)

## ----fig.height= 4, fig.width= 7, message=FALSE, warning=FALSE-----------
ts_lags(USgas, lags = c(12, 24, 36, 48))

## ---- fig.height=4, fig.width= 7, message=FALSE, warning=FALSE-----------
ts_acf(USgas, lag.max = 36)
ts_pacf(USgas, lag.max = 36)


