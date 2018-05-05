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

# Coffee Prices: Robusta and Arabica $ per kg
`%>%` <- magrittr::`%>%`
Robusta <- Quandl::Quandl("COM/WLD_COFFEE_ROBUS", collapse="monthly", type = "raw")
Arabica <- Quandl::Quandl("COM/WLD_COFFEE_ARABIC", collapse="monthly", type = "raw")
names(Robusta) <- c("date", "Robusta")
names(Arabica) <- c("date", "Arabica")
coffee <- Robusta %>% dplyr::left_join(Arabica) %>% dplyr::arrange(date)

Coffee_Prices <- stats::ts(data = coffee[, c("Robusta", "Arabica")], start = c(lubridate::year(min(coffee$date)), lubridate::month(min(coffee$date))), frequency = 12)
devtools::use_data(Coffee_Prices, overwrite = TRUE)


#Total US Vehicle Sales vs US Monthly Unemployment Rate data frame format
USVSales_df <- Quandl::Quandl("FRED/TOTALNSA", collapse="monthly", type = "raw") 
USUnRate_df <- Quandl::Quandl("FRED/UNRATENSA", collapse="monthly", type = "raw")
names(USVSales_df) <- c("date", "Vehicle Sales")
names(USUnRate_df) <- c("date", "Unemployment Rate")
US_indicators <- USVSales_df %>% 
                 dplyr::left_join(USUnRate_df) %>% 
                 dplyr::arrange(date)
devtools::use_data(US_indicators, overwrite = TRUE)
