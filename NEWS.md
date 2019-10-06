## Version 0.1.5 is now available on CRAN
Updating the package license to MIT (from GPL-3)

New functions

*train_model - a flexible framework for training, testing, evaluating, and forecasting models. This function provides the ability to run multiple models with backtesting or single training/testing partitions
* plot_model - animation the performance of the train_model output on the backtesting partitions
* plot_error - plotting the error distribution of the train_model output 
* ts_cor - for acf and pacf plots with seasonal lags

Deprecated functions

*ts_backtesting - will be replaced by the train_model function
* ts_acf / ts_pacf functions - will be replaced by the ts_cor function

Fix errors 
* ts_seasonal - aligning the box plot color 
* ts_plot - setting the dash and marker mode for multiple time series

## Version 0.1.4 is now available on CRAN
New functions
* forecast_sim - creating different forecast paths for forecast objects (when applicable), by utilizing the underline model distribution with the simulate function
* ts_grid - tuning time series models with grid search approach using backtesting method. Currently, support only the Holt-Winters model
* plot_grid - plotting the output of the ts_grid function

Fix errors
* ts_plot, test_forecast - avoid default setting of the plot_ly function, and set explicitly the plot setting (e.g., color, line mode, etc.). This allows using the function with the plotly subplot function 
* ts_seasonal - define the order of the frequency units of the box plot option
plot_forecast - fixing a gap between the forecast values and the time (x-axis) values 

## Version 0.1.3 is now available on CRAN
* ts_to_prophet function for converting ts objects ("ts", "zoo" and "xts" class) to prophet object
* ccf_plot function for plotting corss correlation lags between two time series
* Fixed error in the ts_backtesting function - supprting xreg option

## Version 0.1.2 is now available on CRAN
New functions:
* ts_backtesting -  a horce race of multiple forecasting models with backtestin
* ts_quantile - time series quantile plot for time series data
* ts_seasonal - supports multiple inputs and new color palattes 

## Version 0.1.1 is now available on CRAN
What's new:
* New options for the seasonality plot
* Heatmap and surface plots
* Polar plot
* Converting function from xts and zoo to ts class
* Spliting function for ts object for training and testing partitions

## Updates for the development version (0.1.0.9000) on Github
* Time series lags plot - ts_lags() function
* Function ts_split() to split 'ts' object into training and testing partitions 
* Functions for converting xts and zoo objects for ts object:
    + xts_to_ts(), and
    + zoo_to_ts()
* Two types for the seasonal_ly() plot:
    + "normal" - seasonal variation by year, or 
    + "cycle" - seasonal variation by the cycle units over time (months or quarters) 
    + "polar" - polar plot for seasonality
    + "box" - box-plot by cycle units
* Decompose plot with the decompose_ly() function  
* Data set - US monthly total vehicle sales: 1976 - 2017 (USVSales), 'ts' object
* Data set - US monthly civilian unemployment rate: 1948 - 2017 (USUnRate), 'ts' object
* Data set - US monthly natural gas consumption: 2000 - 2017 (USgas), 'ts' object
* Data set - University of Michigan Consumer Survey, Index of Consumer Sentiment: 1980 - 2017 (Michigan_CS), 'xts' object 
* Data set - Monthly crude Oil Prices: Brent - Europe: 1987 - 2017 (EURO_Brent), 'zoo' object

## Version 0.1.0 is now available on CRAN

* Function for plotting univariate and multivariate time series data
* Evaluation plot for the testing set (hold-out data)
* Interactive seasonality plot
* Functions for interactive plot for the ACF and PACF
