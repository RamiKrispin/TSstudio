
TSstudio
========

The TSstudio package provides a set of interactive visualization tools for time series analysis and forecasting.

Overview
--------

The TSstudio package provides a set of tools for time series analysis, supporting “ts”, “mts”, “zoo”, and “xts” objects. It includes interactive visualizations tools based on the [plotly](https://plot.ly/r/) package for descriptive time series analysis. In addition for the visualization tools, the package provides a set of utility functions for preprocessing time series objects.

All the visualization tools in the package, support multiple time series objects (“ts”, “mts”, “zoo”, “xts” and model output from the forecast package) without the need of any data transformation. They include the following plots:

1.  Visualization of time series objects (ts\_plot)
2.  Seasonality plots (ts\_seasonal)
3.  Heatmap, surface and polar plots (ts\_heatmap, ts\_surface, ts\_polar)
4.  Lags and correlation plots (ts\_lags, ts\_acf, ts\_pacf)
5.  Decompose plot (ts\_decompose)
6.  Residual plot (check\_res)
7.  Forecast performance (test\_forecast)

Besides the visualization functions, there are set of utility functions:

1.  ts\_split - for splitting time series object to training and testing partitions
2.  ts\_reshape - transforming time series objects to year/cycle unit data frame format
3.  xts\_to\_ts and zoo\_to\_ts - functions for transforming xts or zoo objects to ts format

Note: most of the current functions support only monthly and quarterly frequencies, the plan is to expend the ability to lower frequencies such as daily and hourly in the next release

Examples
--------

A detailed examples of the package's key functions is available [here](https://cran.r-project.org/web/packages/TSstudio/vignettes/TSstudio_Intro.html).

![The TSstudio package](https://github.com/RamiKrispin/TSstudio/blob/master/vignettes/gif/TSstudio.gif)

Installation
------------

Install the stable version from [CRAN](https://CRAN.R-project.org/package=TSstudio):

``` r
install.packages("TSstudio")
```

or install the development version from [Github](https://github.com/RamiKrispin/TSstudio):

``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/TSstudio")
```
