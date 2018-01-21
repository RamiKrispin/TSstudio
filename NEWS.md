## Updates for the development version (0.1.0.9000) on Github
* Function ts_split() to split 'ts' object into training and testing partitions 
* Functions for converting xts and zoo objects for ts object:
    + xts_to_ts(), and
    + zoo_to_ts()
* Two types for the seasonal_ly() plot:
    + "normal" - seasonal variation by year, or 
    + "cycle" - seasonal variation by the cycle units over time (months or quarters) 
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
