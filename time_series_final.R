#Set up working directory
wd <- '/Users/emily.dahlberg/Documents/Data Science/450/Project 1/'
setwd(wd)

#Load necessary packages for analysis
library(nlme)
library(timeSeries)
library(TTR)
library("forecast")

#Load and prepare data
raw_data <- read.csv('monthly-us-female-1619-years-une.csv')
names(raw_data) <- c('year_month', 'unempl')

raw_data <- raw_data[-409, ] #The last row was some sort of summary

#Use the ts function to create a time series object out of the employment numbers
data_ts <- raw_data$unempl
timeseries <- ts(data_ts, freq = 12, start = c(1948,1))
timeseries
plot.ts(timeseries, main = 'Timeseries plot of Unemployment', ylab = "Women's Unemployment")

#Decompose the time series to get the different components
timeseries_comp <- decompose(timeseries)
timeseries_comp$seasonal
timeseries_comp$trend
timeseries_comp$random
plot(timeseries_comp)

#Create new timeseries that ignores the seasonal component
timeseries_adj <- timeseries - timeseries_comp$seasonal
plot(timeseries_adj, main = 'Seasonally Adjusted Unemployment Numbers', ylab = 'Unemployment (thousands), seasonally adjusted'
      , xlab = 'Year, Month')

#Fit linear model to the adjusted timeseries
#First step: create a dataframe out of the adjusted time series
dataframe_adj <- data.frame(timeseries_adj)
#View(dataframe_adj)
dataframe_adj$time <- seq(1,408)
dataframe_adj <- dataframe_adj[c('time', 'timeseries_adj')]

#Then use the dataframe to fit a linear model
linear <- lm(dataframe_adj$timeseries_adj ~ dataframe_adj$time, dataframe_adj)

plot(dataframe_adj, type = 'l', main = 'Seasonally Adjusted Unemployment Numbers', xlab = 'Month index', 
     ylab = 'Unemployment (thousands), seasonally adjusted')
abline(linear, col = 'red')

#Autocorrelation function
acf(timeseries)
acf(linear$residuals, lag.max = 50)
pacf(raw_data$unempl)
pacf(timeseries)

#Holt Winters
exp_forecast <- HoltWinters(timeseries_adj)
exp_forecast
exp_forecast$alpha
summary(exp_forecast)
plot(exp_forecast)
hw_predict <- predict(exp_forecast, n.ahead = 60)

plot(exp_forecast, xlim = c(1948, 1990), ylim = c(0,1200))
par(new = TRUE)
plot(hw_predict, xlim = c(1948, 1990), ylim = c(0,1200), col = "blue")

exp_forecast2 <- forecast.HoltWinters(timeseries_adj, h = 8)


#Part 4: ARIMA function
p = acf(raw_data$unempl, lag = 100) #to find p
q = pacf(raw_data$unempl) #pact to find q
plot(q)
fit=arima(timeseries, order = c(100,0,2))
fit
plot(fit)
plot(fit$residuals)
fit2 <- auto.arima(timeseries_adj)
plot(fit2$residuals)
