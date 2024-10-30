# 1.) Plot to search for outliers
# 2.) Stabilize the variance using Box-Cox
# 3.) Analyze de stationarity (ct. mean and ACF and PACF cancel rapidly)
# 4.) If not stationary, use differencing (if it's seasonal, apply it and do the regular once the ACF and PACF stabilize)
# 5.) Identify seasonal coefficients of ACF and PACF
# 6.) Identify regular coefficients with ACF and PACF of the residuals of the seasonal model
# 7.) Check significance of coefficients
# 8.) Analyze residuals: outliers, serial correlation (Ljung y Box Test), plot histogram (normality)
# 9.) Compare different models

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function

## Load dataset -------------------------------------------------------------------------------------------------------
fdata<-read.table("C:/Users/rodri/Documents/3 IMAT/Cuatri 1/Forecasting/SARIMA/ARIMA_08.dat", sep = "")

TT <-  12
y <- ts(fdata, frequency = TT)
ggtsdisplay(y,lag.max = 50)

## Training and validation ------------------------------------------------------------
y.TR <- subset(y, end = 1000)
y.TV <- subset(y, start = 1000)
autoplot(y.TR, color = "orange") + 
  autolayer(y.TV, color = "blue")

Lambda <- BoxCox.lambda.plot(y.TR,TT)
z <- BoxCox(y.TR,Lambda)
ggtsdisplay(z,lag.max = 100)

# If differencing is needed
z.sdiff <- diff(z,lag=TT,differences = 1)
ggtsdisplay(z.sdiff,lag.max = 100) #differences contains the order of differentiation

# If differencing is needed
z.rdiff <- diff(z, differences = 1)
ggtsdisplay(z.rdiff,lag.max = 100)

# Both
z.rdiff.sdiff <- diff(z.rdiff,lag=TT,differences = 1)
ggtsdisplay(z.rdiff.sdiff,lag.max = 100)

# Fit seasonal model with estimated order
arima.fit <- Arima(y.TR,
                   order=c(2,1,1),
                   seasonal = list(order=c(1,1,1), period=TT),
                   lambda = Lambda)
#summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot
# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=120)

# Check fitted forecast
autoplot(y, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)

# Autoarima
auto.arima(y.TR,trace=TRUE)

# Outlier media
#q1 <- quantile(y, 0.01)
#q3 <- quantile(y, 0.99)
#iqr <- q3 - q1

#lower_bound <- q1 - 1.5 * iqr
#upper_bound <- q3 + 1.5 * iqr

# Identify outliers
#outliers <- which(y < lower_bound | y > upper_bound)
#outliers
#index <- 
#y[index] <- (y[index-1] + y[index+1])/2
#ggtsdisplay(y,lag.max = 50)
