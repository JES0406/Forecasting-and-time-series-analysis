library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables


## Load dataset -------------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fdata <- read.table(file = "DailyPrice_Load_Wind_Spain_2019_2020_TR.csv",
                    header = T,
                    sep = ";")
fdata$Date <- lubridate::dmy(fdata$Date)

# TR - TS index
#ind_TR <- fdata$Date <= lubridate::ymd("2020-09-30")
#ind_TV <- !ind_TR

# Convert to time series object
fdata_ts <- ts(fdata, frequency = 7)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values 
y <- fdata_ts[,c("Price")]
x <- fdata_ts[,c("Demand","Wind")]
autoplot(cbind(y,x), facets = TRUE)

#Scale
y.tr <- y/10
x.tr <- x/100
autoplot(cbind(y.tr,x.tr), facets = TRUE)


## Identification and fitting process ----------------------------------------------------------------------
## FIRST --------------------------------------------
#### Fit initial FT model with large s
#This arima function belongs to the TSA package
xlag <- lag(x, 0)
arima.fit <- arima(y.tr,
                xtransf = xlag,
                order=c(1,1,1),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(2,0),c(0,0)),
                method="ML")
# b=0, s=0, r=2
# b=0, s=0, r=0

summary(arima.fit)
TF.RegressionError.plot(y, x, arima.fit, lag.max = 50)
TF.Identification.plot(x,arima.fit) #Plot values of coefficients
x[,1]
ccf(y = residuals(arima.fit), x = x[,1])
ccf(y = residuals(arima.fit), x = x[,2])

## Load dataset -----------------------------------------------------------------------------------------------
fdataTS <- read.table(file = "DailyPrice_Load_Wind_Spain_2019_2020_TS.csv",
                    header = T,
                    sep = ";")

#Scale
x.ts <- ts(fdataTS[,c("Demand","Wind")]/100, frequency = 7)

y_est <- TF.forecast(y.old = y.tr, #past values of the series
                    x.old = x.tr, #Past values of the explanatory variables
                    x.new = x.ts, #New values of the explanatory variables
                    model = arima.fit, #fitted transfer function model
                    h=7) #Forecast horizon

write.table(y_est*10, file = "PredTF.dat", col.names = FALSE, row.names = FALSE)
