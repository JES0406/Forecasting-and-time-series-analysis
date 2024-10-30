#################################################################################
##########################       Lab4: SARIMA      ##############################
##############     ----------- CarRegistrations --------- #######################
#################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(tidyverse)

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("CarRegistrations.xls")
head(fdata)

# Order and Check for missing dates
fdata <- fdata |> arrange(Fecha)
day(fdata$Fecha) <- 01
date_range <- seq(min(fdata$Fecha), max(fdata$Fecha), by = "months")
date_range[!date_range %in% fdata$Fecha] 
sum(is.na(fdata$CarReg))

# Check for duplicates
length(fdata$Fecha) 
length(unique(fdata$Fecha))


# Convert to time series object
y <- ts(fdata$CarReg,start = 1960, frequency = 12)
# for daily data
autoplot(y)


## Training and validation ------------------------------------------------------------
y.TR <- subset(y, end = length(y)-5*12) #Leave 5 years for validation
y.TV <- subset(y, start = length(y)-5*12+1)

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y.TR, color = "orange") + 
  autolayer(y.TV, color = "blue")

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y.TR, window.width = 12)

# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y.TR,Lambda)

autoplot(z) + autolayer(y)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# Seasonal Differentiation
B12z<- diff(z, lag = 12, differences = 1)
ggtsdisplay(B12z,lag.max = 4*12) #differences contains the order of differentiation

# Regular Differentiation
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz, lag.max = 4*12) #differences contains the order of differentiation

# Regular & Seasonal Differentiation
B12Bz <- diff(Bz, lag = 12, differences = 1)
ggtsdisplay(B12Bz, lag.max = 4 * 12)


# Order does not matter
B_B12z<- diff(B12z, differences = 1)
autoplot(B12Bz, color = "blue", size = 2) + autolayer(B_B12z, color = "orange", size = 0.5)


# Fit seasonal model with estimated order
arima.fit <- Arima(y.TR,
                   order=c(0, 1, 1),
                   seasonal = list(order=c(0, 1, 1), period=12),
                   lambda = Lambda,
                   include.constant = FALSE)

summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=100)
# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit),lag.max = 100)


# Check fitted
autoplot(y.TR, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)

## Validation error for h = 1 -------------------------------------------------------------------------------------------------------
# Obtain the forecast in validation for horizon = 1 using the trained parameters of the model
y.TV.est <- y * NA

for (i in seq(length(y.TR) + 1, length(y), 1)){# loop for validation period
  y.TV.est[i] <- forecast(subset(y, end=i-1), # y series up to sample i 
                    model = arima.fit,       # Model trained (Also valid for exponential smoothing models)
                    h=1)$mean                # h is the forecast horizon
}

head(y.TV.est, 48)
na.omit(y.TV.est)

#Plot series and forecast 
autoplot(y) +
  forecast::autolayer(y.TV.est)

#Compute validation errors
accuracy(y.TV.est, y)



