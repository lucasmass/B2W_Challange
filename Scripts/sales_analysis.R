##################################################
# This file loads daily retail sales case for B2W
# We will use the data to forecast the next year of sales per day
#
# Created 2019/06/24
# Author: L.M.
##################################################

# Clear all variables in workspace
rm(list=ls())

# Load the GGally package
library(GGally)

# Load the lubridate package
library(lubridate)

# Load the tseries package
library(tseries)

# Load the forecasting package
library(fpp2)

# Load the data
library(magrittr)
library(dplyr)

# Reading CSV file
data.sales <- read.csv(file = "./sales.csv",
                        head = TRUE, 
                        dec = ".", 
                        sep = ",", 
                        na.strings = "")

# Date as charecter string
datime <- as.character(data.sales$DATE_ORDER)

# Order the data by product id and date and then take the mean value of the prices (revenue), regarding products quantity
ordered_data <- data.sales %>% 
  group_by(PROD_ID, DATE_ORDER) %>% 
  summarise(REVENUE = mean(REVENUE)) 

# Separate the products time series, regardless the payment type and the competitors
P1.data <- subset(ordered_data, ordered_data$PROD_ID == "P1")
P2.data <- subset(ordered_data, ordered_data$PROD_ID == "P2")
P3.data <- subset(ordered_data, ordered_data$PROD_ID == "P3")
P4.data <- subset(ordered_data, ordered_data$PROD_ID == "P4")
P5.data <- subset(ordered_data, ordered_data$PROD_ID == "P5")
P6.data <- subset(ordered_data, ordered_data$PROD_ID == "P6")
P7.data <- subset(ordered_data, ordered_data$PROD_ID == "P7")
P8.data <- subset(ordered_data, ordered_data$PROD_ID == "P8")
P9.data <- subset(ordered_data, ordered_data$PROD_ID == "P9")

# Plot the time series to a quick check
par(mfrow = c(3, 3))

plot.ts(P1.data$REVENUE, xlim = c(0, length(P1.data$REVENUE)), col = "blue", ylab = "P1 Revenue")

plot.ts(P2.data$REVENUE, xlim = c(0, length(P2.data$REVENUE)), col = "blue", ylab = "P2 Revenue")

plot.ts(P3.data$REVENUE, xlim = c(0, length(P3.data$REVENUE)), col = "blue", ylab = "P3 Revenue")

plot.ts(P4.data$REVENUE, xlim = c(0, length(P4.data$REVENUE)), col = "blue", ylab = "P4 Revenue")

plot.ts(P5.data$REVENUE, xlim = c(0, length(P5.data$REVENUE)), col = "blue", ylab = "P5 Revenue")

plot.ts(P6.data$REVENUE, xlim = c(0, length(P6.data$REVENUE)), col = "blue", ylab = "P6 Revenue")

plot.ts(P7.data$REVENUE, xlim = c(0, length(P7.data$REVENUE)), col = "blue", ylab = "P7 Revenue")

plot.ts(P8.data$REVENUE, xlim = c(0, length(P8.data$REVENUE)), col = "blue", ylab = "P8 Revenue")

plot.ts(P9.data$REVENUE, xlim = c(0, length(P9.data$REVENUE)), col = "blue", ylab = "P9 Revenue")


##################################################
# Forecasting Procedure
##################################################

# Data preparation to forecasting procedure

# Declare as time series data
Y <- ts(P9.data$REVENUE)

##################################################
# Preliminary Analysis for the Forecast
##################################################

# Investigate for unit root and trend stationarity null hypothesis

# kY <- kpss.test(Y, null = "Trend")

# Investigate transformations
# Take the first difference to remove the trend

# DY <- diff(Y)
# kD <- kpss.test(DY, null = "Trend")

##################################################
# Our series, Y, has trend
# To remove the trend, we make the first difference
# The first differenced series still does not have seasonality
#
# Now to the forecast
##################################################

##################################################
# Use a benchmark method to forecast
# Let's use a linear method (ARIMA) as our benchmark
##################################################

fit_arima <- auto.arima(Y,
                        seasonal = FALSE,
                        stepwise = FALSE,
                        approximation = FALSE,
                        trace = FALSE)
print(summary(fit_arima))
checkresiduals(fit_arima)

##################################################
# Use exponential smoothing model to forecast
# Fit ETS model
##################################################

fit_ets <- ets(Y)
print(summary(fit_ets))
checkresiduals(fit_ets)

##################################################
# Use neural network autoregression model
# Fit a NNTEAR(p,k) (p lagged inputs and k nodes in the hidden layer)
##################################################

(fit.nnetar <- nnetar(Y, 30, 0, 30, lambda = 0))

##################################################
# Forecast with NNETAR
# y{t} = f(y_{t-1}) + e_{t}
##################################################

fit.nnetar %>% forecast(PI = TRUE, h = 30) %>% autoplot(include = 20)
