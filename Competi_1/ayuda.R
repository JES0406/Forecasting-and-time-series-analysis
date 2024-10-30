# Ussing the ARMA_Hackathon_data.xls file we will try to get p, q and lambda values for the ARMA model
library(readxl)

# Load the data
data <- read_excel("ARIMA_Hackaton_data.xls")

# The data has 7 columns, each one is a time series
# We have to get p, q and lambda values for each time series

# We will use the forecast package to get the p, q and lambda values
library(forecast)

# We will use the auto.arima function to get the p, q and lambda values
# We will use the AIC criterion to select the best model
# We will use the stepwise algorithm to get the best model
# We will use the Hannan-Rissanen algorithm to get the best model
# We will use the OCSB test to get the best model

for (i in 1:5) {
  # Get the time series
  print(paste("Time series", i))
  ts <- ts(data[,i], frequency = 1)
  
  # Get the p, q and lambda values
  model <- auto.arima(ts, stepwise = TRUE, approximation = FALSE, trace = TRUE)
  
  # Print the p, q and lambda values
  print(model)
}

# The p, q and lambda values for each time series are:
