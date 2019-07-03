##################################################
# This file loads daily retail sales case for B2W's competitors
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
data.comp  <- read.csv(file = "comp_prices.csv", 
                        head = TRUE, 
                        dec = ".", 
                        sep = ",", 
                        na.strings = "")

# Separate date and time, from DATE_EXTRACTION variable
datime <- do.call(rbind, strsplit(as.character(data.comp$DATE_EXTRACTION), " "))

##################################################
# Dimensionality reduction
##################################################

###
### Investigating the importance of PAY_TYPE variable
###

# Group the data by payment type, product id, date and time and then take the mean value of the prices, regardless the competitors and products
ordered_data_pay <- data.comp %>% 
  group_by(PAY_TYPE, DATE_ORDER = datime[,1], HOUR_ORDER = datime[,2]) %>% 
  summarise(COMPETITOR_PRICE =  mean(COMPETITOR_PRICE))

# Order the data by period of the day: nocturnal (N --- 12pm -- 12am) and diurnal (D --- 12am -- 12pm)
ordered_data_pay$period <- rep("N", nrow(ordered_data_pay))
ordered_data_pay$period[ordered_data_pay$HOUR_ORDER < '12:00'] <- "D"

# Separate the payment type time series, regardless the competitors and products
PAY1.data   <- subset(ordered_data_pay, ordered_data_pay$PAY_TYPE == 1)
PAY1.data.D <- subset(PAY1.data, PAY1.data$period == "D")
PAY1.data.N <- subset(PAY1.data, PAY1.data$period == "N")

PAY2.data   <- subset(ordered_data_pay, ordered_data_pay$PAY_TYPE == 2)
PAY2.data.D <- subset(PAY2.data, PAY2.data$period == "D")
PAY2.data.N <- subset(PAY2.data, PAY2.data$period == "N")

# Plot the time series to a quick check if the diurnal (D) or nocturnal (N) periods are too different from each other
par(mfrow = c(2, 1))

plot.ts(PAY1.data.D$COMPETITOR_PRICE, ylim = c( 690, 1900), xlim = c(0, length(PAY1.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "Deferred Payment")
par(new = TRUE)
plot.ts(PAY1.data.N$COMPETITOR_PRICE, ylim = c( 690, 1900), xlim = c(0, length(PAY1.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(PAY2.data.D$COMPETITOR_PRICE, ylim = c( 690, 1900), xlim = c(0, length(PAY2.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "Immediate Payment")
par(new = TRUE)
plot.ts(PAY2.data.N$COMPETITOR_PRICE, ylim = c( 690, 1900), xlim = c(0, length(PAY2.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

###
### Investigating the importance of COMPETITOR variable
###

# Group the data by competitor, date and time and then take the mean value of the prices, regardless the competitors and product id
ordered_data_comp <- data.comp %>% 
  group_by(COMPETITOR, DATE_ORDER = datime[,1], HOUR_ORDER = datime[,2]) %>% 
  summarise(COMPETITOR_PRICE =  mean(COMPETITOR_PRICE))

# Order the data by period of the day: nocturnal (N --- 12pm -- 12am) and diurnal (D --- 12am -- 12pm)
ordered_data_comp$period <- rep("N", nrow(ordered_data_comp))
ordered_data_comp$period[ordered_data_comp$HOUR_ORDER < '12:00'] <- "D"

# Separate the competitors time series, regardless the payment type and the products
C1.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C1")
C1.data.D <- subset(C1.data, C1.data$period == "D")
C1.data.N <- subset(C1.data, C1.data$period == "N")

C2.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C2")
C2.data.D <- subset(C2.data, C2.data$period == "D")
C2.data.N <- subset(C2.data, C2.data$period == "N")

C3.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C3")
C3.data.D <- subset(C3.data, C3.data$period == "D")
C3.data.N <- subset(C3.data, C3.data$period == "N")

C4.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C4")
C4.data.D <- subset(C4.data, C4.data$period == "D")
C4.data.N <- subset(C4.data, C4.data$period == "N")

C5.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C5")
C5.data.D <- subset(C5.data, C5.data$period == "D")
C5.data.N <- subset(C5.data, C5.data$period == "N")

C6.data   <- subset(ordered_data_comp, ordered_data_comp$COMPETITOR == "C6")
C6.data.D <- subset(C6.data, C6.data$period == "D")
C6.data.N <- subset(C6.data, C6.data$period == "N")

# Plot the time series to a quick check if the diurnal (D) or nocturnal (N) periods are too different from each other
par(mfrow = c(2, 3))

plot.ts(C1.data.D$COMPETITOR_PRICE, ylim = c( 550, 2400), xlim = c(0, length(C1.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C1 Mean Price")
par(new = TRUE)
plot.ts(C1.data.N$COMPETITOR_PRICE, ylim = c( 550, 2400), xlim = c(0, length(C1.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(C2.data.D$COMPETITOR_PRICE, ylim = c( 570, 2200), xlim = c(0, length(C2.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C2 Mean Price")
par(new = TRUE)
plot.ts(C2.data.N$COMPETITOR_PRICE, ylim = c( 570, 2200), xlim = c(0, length(C2.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(C3.data.D$COMPETITOR_PRICE, ylim = c( 450, 2000), xlim = c(0, length(C3.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C3 Mean Price")
par(new = TRUE)
plot.ts(C3.data.N$COMPETITOR_PRICE, ylim = c( 450, 2000), xlim = c(0, length(C3.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(C4.data.D$COMPETITOR_PRICE, ylim = c( 420, 1600), xlim = c(0, length(C4.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C4 Mean Price")
par(new = TRUE)
plot.ts(C4.data.N$COMPETITOR_PRICE, ylim = c( 420, 1600), xlim = c(0, length(C4.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(C5.data.D$COMPETITOR_PRICE, ylim = c( 450, 1550), xlim = c(0, length(C5.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C5 Mean Price")
par(new = TRUE)
plot.ts(C5.data.N$COMPETITOR_PRICE, ylim = c( 450, 1550), xlim = c(0, length(C5.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(C6.data.D$COMPETITOR_PRICE, ylim = c( 510, 2800), xlim = c(0, length(C6.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "C6 Mean Price")
par(new = TRUE)
plot.ts(C6.data.N$COMPETITOR_PRICE, ylim = c( 510, 2800), xlim = c(0, length(C6.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

# summary(C1.data.D$COMPETITOR_PRICE)
# summary(C2.data.D$COMPETITOR_PRICE)
# summary(C3.data.D$COMPETITOR_PRICE)
# summary(C4.data.D$COMPETITOR_PRICE)
# summary(C5.data.D$COMPETITOR_PRICE)
# summary(C6.data.D$COMPETITOR_PRICE)

###
### As the aforementioned analysis tells us the variables PAY_TYPE and COMPETITOR are not that different
### during the defined diurnal shift (D --- 12am - 12pm) and nocturnal (N --- 12pm - 12am)
### Therefore, as a grouping variable we will use PROD_ID, regardless PAY_TYPE and COMPETITORS variables
###

# Group the data by product id, date and time and then take the mean value of the prices, regardless the competitors
ordered_data <- data.comp %>% 
  group_by(PROD_ID, DATE_ORDER = datime[,1], HOUR_ORDER = datime[,2]) %>% 
  summarise(COMPETITOR_PRICE =  mean(COMPETITOR_PRICE))

# Order the data by period of the day: nocturnal (N --- 12pm - 12am) and diurnal (D --- 12am - 12pm)
ordered_data$period <- rep("N", nrow(ordered_data))
ordered_data$period[ordered_data$HOUR_ORDER < '12:00'] <- "D"

# Separate the products time series, regardless the payment type and the competitors
P1.data   <- subset(ordered_data, ordered_data$PROD_ID == "P1")
P1.data.D <- subset(P1.data, P1.data$period == "D")
P1.data.N <- subset(P1.data, P1.data$period == "N")

P2.data   <- subset(ordered_data, ordered_data$PROD_ID == "P2")
P2.data.D <- subset(P2.data, P2.data$period == "D")
P2.data.N <- subset(P2.data, P2.data$period == "N")

P3.data   <- subset(ordered_data, ordered_data$PROD_ID == "P3")
P3.data.D <- subset(P3.data, P3.data$period == "D")
P3.data.N <- subset(P3.data, P3.data$period == "N")

P4.data   <- subset(ordered_data, ordered_data$PROD_ID == "P4")
P4.data.D <- subset(P4.data, P4.data$period == "D")
P4.data.N <- subset(P4.data, P4.data$period == "N")

P5.data   <- subset(ordered_data, ordered_data$PROD_ID == "P5")
P5.data.D <- subset(P5.data, P5.data$period == "D")
P5.data.N <- subset(P5.data, P5.data$period == "N")

P6.data   <- subset(ordered_data, ordered_data$PROD_ID == "P6")
P6.data.D <- subset(P6.data, P6.data$period == "D")
P6.data.N <- subset(P6.data, P6.data$period == "N")

P7.data   <- subset(ordered_data, ordered_data$PROD_ID == "P7")
P7.data.D <- subset(P7.data, P7.data$period == "D")
P7.data.N <- subset(P7.data, P7.data$period == "N")

P8.data   <- subset(ordered_data, ordered_data$PROD_ID == "P8")
P8.data.D <- subset(P8.data, P8.data$period == "D")
P8.data.N <- subset(P8.data, P8.data$period == "N")

P9.data   <- subset(ordered_data, ordered_data$PROD_ID == "P9")
P9.data.D <- subset(P9.data, P9.data$period == "D")
P9.data.N <- subset(P9.data, P9.data$period == "N")

# Plot the time series to a quick check if the diurnal (D) or nocturnal (N) periods are too different from each other
# If not, just take the mean value between the series and forecast them
par(mfrow = c(3, 3))

plot.ts(P1.data.D$COMPETITOR_PRICE, ylim = c(1000, 2000), xlim = c(0, length(P1.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P1 Mean Price")
par(new = TRUE)
plot.ts(P1.data.N$COMPETITOR_PRICE, ylim = c(1000, 2000), xlim = c(0, length(P1.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P2.data.D$COMPETITOR_PRICE, ylim = c( 600,  850), xlim = c(0, length(P2.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P2 Mean Price")
par(new = TRUE)
plot.ts(P2.data.N$COMPETITOR_PRICE, ylim = c( 600,  850), xlim = c(0, length(P2.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P3.data.D$COMPETITOR_PRICE, ylim = c( 880, 1500), xlim = c(0, length(P3.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P3 Mean Price")
par(new = TRUE)
plot.ts(P3.data.N$COMPETITOR_PRICE, ylim = c( 880, 1500), xlim = c(0, length(P3.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P4.data.D$COMPETITOR_PRICE, ylim = c( 400,  700), xlim = c(0, length(P4.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P4 Mean Price")
par(new = TRUE)
plot.ts(P4.data.N$COMPETITOR_PRICE, ylim = c( 400,  700), xlim = c(0, length(P4.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P5.data.D$COMPETITOR_PRICE, ylim = c( 700, 1100), xlim = c(0, length(P5.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P5 Mean Price")
par(new = TRUE)
plot.ts(P5.data.N$COMPETITOR_PRICE, ylim = c( 700, 1100), xlim = c(0, length(P5.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P6.data.D$COMPETITOR_PRICE, ylim = c(1400, 2400), xlim = c(0, length(P6.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P6 Mean Price")
par(new = TRUE)
plot.ts(P6.data.N$COMPETITOR_PRICE, ylim = c(1400, 2400), xlim = c(0, length(P6.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P7.data.D$COMPETITOR_PRICE, ylim = c( 680, 1000), xlim = c(0, length(P7.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P7 Mean Price")
par(new = TRUE)
plot.ts(P7.data.N$COMPETITOR_PRICE, ylim = c( 680, 1000), xlim = c(0, length(P7.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P8.data.D$COMPETITOR_PRICE, ylim = c( 380,  550), xlim = c(0, length(P8.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P8 Mean Price")
par(new = TRUE)
plot.ts(P8.data.N$COMPETITOR_PRICE, ylim = c( 380,  550), xlim = c(0, length(P8.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

plot.ts(P9.data.D$COMPETITOR_PRICE, ylim = c( 400,  550), xlim = c(0, length(P9.data.D$COMPETITOR_PRICE)), col = "blue", ylab = "P9 Mean Price")
par(new = TRUE)
plot.ts(P9.data.N$COMPETITOR_PRICE, ylim = c( 400,  550), xlim = c(0, length(P9.data.D$COMPETITOR_PRICE)), col = "red",  ylab = "")

##################################################
# Forecasting Procedure
##################################################

# Data preparation to forecasting procedure

# Declare as time series data
Y1 <- ts(P9.data.D$COMPETITOR_PRICE)
Y2 <- ts(P9.data.N$COMPETITOR_PRICE)
# Time series of the mean value between the dianurnal and nocturnal series
Y  <- cbind(Y1, Y2) %>% 
  rowMeans() %>% 
  na.omit() %>% 
  ts()

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
