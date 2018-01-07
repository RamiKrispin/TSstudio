# Update data sets

#Total US Vehicle Sales
USVSales <- Quandl::Quandl("FRED/TOTALNSA", collapse="monthly", type = "ts") 
devtools::use_data(USVSales)

# US Monthly Unemployment Rate
USUnRate <- Quandl::Quandl("FRED/UNRATENSA", collapse="monthly", type = "ts")
devtools::use_data(USUnRate)

# US Natural Gas Consumption
USgas <- Quandl::Quandl("FRED/NATURALGAS", collapse="monthly", type = "ts")
devtools::use_data(USgas)
