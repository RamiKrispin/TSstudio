# Update data sets

# US Natural Gas Consumption
gas <- Quandl::Quandl("FRED/NATURALGAS", collapse="monthly", type = "ts")
devtools::use_data(gas)