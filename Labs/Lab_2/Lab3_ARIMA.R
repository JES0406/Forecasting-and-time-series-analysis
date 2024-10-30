########################################################################################
##############       Forecasting:      ARIMA models         ############################
########################################################################################


library(MLTools)
library(fpp2)
library(readxl)
library(tidyverse)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("ARIMA_example.xlsx")
head(fdata)

# Convert to time series object and inspect
fdata_ts <- ts(fdata)
head(fdata_ts)

# index to select a time series
y <- fdata_ts[,1]


## Identification and fitting process -------------------------------------------------------------------------------------------------------
autoplot(y)
ggtsdisplay(y)

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y, window.width = 10)

# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y, Lambda)

autoplot(y, linewidth=0.25) + 
  autolayer(z, color = "blue", alpha=0.5)



# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z, lag.max = 25)

# Alternative test
adf.test(z, alternative = "stationary")
ndiffs(z)

# If differencing is needed
Bz <- diff(z, differences = 1)

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(Bz, lag.max = 25)

# Fit model with estimated order
arima.fit <- Arima(y,
                   order=c(2, 1, 0), 
                   lambda = Lambda,
                   #lambda = 1,
                   include.constant = FALSE)

summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
autoplot(arima.fit) #root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag = 25)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit), lag.max = 20)
#######

#Check  forecast
autoplot(y, series = "Real") +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast (select h and start)
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)

autoplot(subset(y, start = 900)) + 
  autolayer(y_est)


