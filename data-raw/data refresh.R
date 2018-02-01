# Update data sets

#Total US Vehicle Sales
USVSales <- Quandl::Quandl("FRED/TOTALNSA", collapse="monthly", type = "ts") 
devtools::use_data(USVSales, overwrite = TRUE)

# US Monthly Unemployment Rate
USUnRate <- Quandl::Quandl("FRED/UNRATENSA", collapse="monthly", type = "ts")
devtools::use_data(USUnRate, overwrite = TRUE)

# US Natural Gas Consumption
USgas <- Quandl::Quandl("FRED/NATURALGAS", collapse="monthly", type = "ts")
devtools::use_data(USgas, overwrite = TRUE)

# University of Michigan Consumer Survey, Index of Consumer Sentiment
Michigan_CS <- Quandl::Quandl("UMICH/SOC1", collapse="monthly", start_date="1980-01-01", type = "xts")
devtools::use_data(Michigan_CS, overwrite = TRUE)

# Crude Oil Prices: Brent - Europe
EURO_Brent <- Quandl::Quandl("FRED/MCOILBRENTEU", collapse="monthly", type = "zoo")
devtools::use_data(EURO_Brent, overwrite = TRUE)
