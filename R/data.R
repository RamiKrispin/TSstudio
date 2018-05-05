#' US Monthly Total Vehicle Sales
#' 
#' US monthly total vehicle sales: 1976 - 2017.
#' Units: Thousands of units
#' 
#' 
#' @format Time series data - 'ts' object
#' @source U.S. Bureau of Economic Analysis, Total Vehicle Sales [TOTALNSA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/TOTALNSA, January 7, 2018.
#' @keywords datasets
#' @examples
#' ts.plot_ly(USVSales)
#' seasonal_ly(USVSales)

"USVSales"

#' US Monthly Civilian Unemployment Rate
#' 
#' US monthly civilian unemployment rate: 1948 - 2017.
#' Units: Percent
#' 
#' @format Time series data - 'ts' object
#' @source U.S. Bureau of Labor Statistics, Civilian Unemployment Rate [UNRATENSA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/UNRATENSA, January 6, 2018.
#' @keywords datasets
#' @examples
#' ts_plot(USUnRate)
#' ts_seasonal(USUnRate)

"USUnRate"



#' US monthly natural gas consumption
#' 
#' US monthly natural gas consumption: 2000 - 2017.
#' Units: Billion Cubic Feet
#' 
#' @format Time series data - 'ts' object
#' @source U.S. Bureau of Transportation Statistics, Natural Gas Consumption [NATURALGAS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/NATURALGAS, January 7, 2018.
#' @keywords datasets
#' @examples
#' ts_plot(USgas)
#' ts_seasonal(USgas, type = "all")

"USgas"

#' University of Michigan Consumer Survey, Index of Consumer Sentiment
#' 
#' University of Michigan Consumer Survey, Index of Consumer Sentiment: 1980 - 2017.
#' Units: Index 1966:Q1=100
#' 
#' @format Time series data - 'xts' object
#' @source University of Michigan, University of Michigan: Consumer Sentiment
#' @keywords datasets
#' @examples
#' ts_plot(Michigan_CS)
#' ts_heatmap(Michigan_CS)

"Michigan_CS"


#' Crude Oil Prices: Brent - Europe
#' 
#' Crude Oil Prices: Brent - Europe: 1987 - 2017.
#' Units: Dollars per Barrel
#' 
#' @format Time series data - 'zoo' object
#' @source U.S. Energy Information Administration, Crude Oil Prices: Brent - Europe [MCOILBRENTEU], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/MCOILBRENTEU, January 8, 2018.
#' @keywords datasets
#' @examples
#' ts_plot(EURO_Brent)
#' ts_decompose(EURO_Brent, type = "both")

"EURO_Brent"

#' Coffee Prices: Robusta and Arabica 
#' 
#' Coffee Prices: Robusta and Arabica: 1960 - 2018.
#' Units: Dollars per Kg
#' 
#' @format Time series data - 'mts' object
#' @source WIKI Commodity Prices - Quandle
#' @keywords datasets
#' @examples
#' ts_plot(Coffee_Prices)

"Coffee_Prices"

#' US Key Indicators - data frame format
#' 
#' Monthly total vehicle sales and unemployment rate: 1976 - 2018.
#' Units: Dollars per Kg
#' 
#' @format Time series data - 'data.frame' object
#' @source U.S. Bureau of Economic Analysis, Total Vehicle Sales [TOTALNSA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/TOTALNSA, January 7, 2018.
#' U.S. Bureau of Labor Statistics, Civilian Unemployment Rate [UNRATENSA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/UNRATENSA, January 6, 2018.
#' @keywords datasets
#' @examples
#' ts_plot(US_indicators)

"US_indicators"