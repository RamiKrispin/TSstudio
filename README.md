
TSstudio
========

The TSstudio package provides a set of interactive visualization tools for time series analysis.

Overview
--------

The TSstudio package provides a set of interactive visualization tools for time series analysis supporting ts, mts, zoo and xts objects. That includes several visualization functions such as forecasting model performance (forecasted vs. actual), time series interactive plots (single and multiple series) and seasonality plots utilizing the visualization applications of the [Plotly](https://plot.ly/r/) package. First version available on CRAN or Github.

![The TSstudio package](https://github.com/RamiKrispin/TSstudio/blob/master/vignettes/gif/TSstudio.gif)

Installation
------------

Install the stable version from [CRAN](https://cran.r-project.org/web/packages/TSstudio/index.html):
``` r
install.packages("TSstudio")
```

or install the development version from [Github](https://github.com/RamiKrispin/TSstudio):
``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/TSstudio")
```

Package Road Map
----------------

The TSstudio package provides a set of tools for time series descriptive analysis and performance measurement of forecasting. Currently first version (0.1.0) is available on CRAN and the version (0.1.1) is schedule for submission to CRAN on March. While working on the next version, some of its functions are already available on the development version in Github (0.1.0.9000).

Below is the road map plan for Q1, where some of the functions are already available on the [CRAN] version or the development version on [Github] and the rest will be available on [ver. 0.1.1] hopefully by March 18: 

* Seasonal plots of time series object, in order to identity seasonal pattern that includes:
    + Plot a series on a range of a full cycle (i.e. by full year) [CRAN]
    + Plot a series over time by units of the cycle (i.e. plot separately each month over a time) [Github]
    + Polar plot by cycle [ver. 0.1.1]
    + Box plot by cycle units (i.e. by months, quarters, etc.) [ver. 0.1.1]

* Correlation analysis plots:
    + ACF and PACF plots [CRAN]
    + Lags plot [ver. 0.1.1]

* Forecasting tools:
    + Performance plot of the forecasted and fitted values vs. the actuals [CRAN]
    + Split function for training and testing partitions [Github]
    + Plot of the forecasted object with confidence intervals [ver. 0.1.1]  

* Utility functions:
    + Decompose plots [Github]
    + Converting xts and zoo objects to ts object [Github]
    + Box-Cox transformation plots using different parameters [ver. 0.1.1]
    + Diagnostic dashboard – quick view of the series characteristics [ver. 0.1.1] 



Examples
--------

Interactive examples can be found [here](http://rpubs.com/ramkrisp/TSstudio)
