##############################################################################
#############       Lab Practice 1: Decomposition Methods     ################
##############################################################################

## Load libraries
library(fpp2) 
library(tidyverse)

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_csv("Unemployment.dat")

fdata
glimpse(fdata)

# Check for missing dates
fdata$DATE <- as.Date(fdata$DATE, format = "%d/%m/%Y")
fdata <- fdata %>% arrange(DATE)
# same as: fdata <- arrange(fdata,DATE)
glimpse(fdata)

date_range <- seq.Date(min(fdata$DATE), max(fdata$DATE), by = "months")
head(date_range)
tail(date_range)

date_range[!date_range %in% fdata$DATE] 
sum(is.na(fdata$TOTAL))

# Convert to time series object
# start -> year
# frequency = 12 -> monthly data
# frequency = 4 -> quarterly data
y <- ts(fdata$TOTAL, start = c(2010,1), frequency = 12)

#Plot time series:
autoplot(y) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")

# or:
plot.ts(y, 
        main="Unemployment in Spain",
        xlab="Year",
        ylab="Number unemployed")

# Select time series time frame
y <- window(y, start = c(2010,1), end = c(2019,12))

# or:
y <- ts(fdata$TOTAL, start = c(2010,1), end = c(2019,12), frequency = 12)

#Plot time series
autoplot(y) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")


#################################################################################
# Decomposition methods
#################################################################################


## Classical additive decomposition
y_dec_add <- decompose(y,type="additive")
autoplot(y_dec_add) + xlab("Year") +
  ggtitle("Classical additive decomposition")


## Classical Multiplicative decomposition
y_dec_mult <- decompose(y, type="multiplicative")
autoplot(y_dec_mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")


## SEATS
library(seasonal)
y_dec_seas <- seas(y)
autoplot(y_dec_seas) + xlab("Year") +
  ggtitle("SEATS decomposition")


# Use seasonal(), trendcycle() and remainder() functions to extract the individual components.
# Use seasadj() to compute the seasonally adjusted time series.

#Compare seasonal components
autoplot(seasonal(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasonal(y_dec_seas), series = "SEATS")


#Compare seasonal adjustment components (i.e. subtracting the seasonal component from the raw series)
autoplot(seasadj(y_dec_add), series = "Additive") +
  forecast::autolayer(seasadj(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasadj(y_dec_seas),series = "SEATS")

autoplot(seasadj(y_dec_seas), series = "SEATS")

#Seasonal subseries plot
ggsubseriesplot(seasonal(y_dec_add)) 
ggsubseriesplot(seasonal(y_dec_mult)) 
ggsubseriesplot(seasonal(y_dec_seas)) 


# Other time series and libraries
#################################

library(TSstudio)
data(US_indicators)
glimpse(US_indicators)
str(US_indicators)

# Rename variables and order dates
US_indicators <- US_indicators %>% 
  rename( VehicleSales = 'Vehicle Sales', UnemploymentRate= `Unemployment Rate`) %>% 
  arrange(Date)
head(US_indicators)
tail(US_indicators)

# Check for complete dates and data
lubridate::day(US_indicators$Date) <- 1
head(US_indicators)
date_range <- seq.Date(min(US_indicators$Date), max(US_indicators$Date), by = "months")
head(date_range)
tail(date_range)

date_range[!date_range %in% US_indicators$Date] 
sum(is.na(US_indicators$VehicleSales))
sum(is.na(US_indicators$UnemploymentRate))

tvs <- US_indicators[, c("Date", "VehicleSales")]
str(tvs)


library(lubridate)
start_point <- c(year(min(tvs$Date)), month(min(tvs$Date)))
start_point

tvs_ts <- ts(data = tvs$'VehicleSales',
             start = start_point,
             frequency = 12)

plot.ts(tvs_ts,
        main = "US Monthly Total Vehicle Sales",
        ylab = "Thousands of Vehicle",
        xlab = "Time"
)


# Multiple ts
US_indicators_ts <- ts(data = US_indicators[, c("VehicleSales",
                                                "UnemploymentRate")],
                       start = c(year(min(tvs$Date)),
                                 month(min(tvs$Date))),
                       frequency = 12)
str(US_indicators_ts)

plot.ts(US_indicators_ts,
        plot.type = "multiple",
        main = "US Monthly Vehicle Sales vs. Unemployment Rate",
        xlab = "Time")


# From library TSstudio:
ts_plot(tvs_ts,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Vehicle",
        slider = TRUE
)

ts_plot(US_indicators_ts,
        title = "US Monthly Vehicle Sales vs. Unemployment Rate",
        type = "multiple")

