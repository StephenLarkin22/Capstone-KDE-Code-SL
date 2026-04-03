#Log Returns Case Study
#Extracting data
library(quantmod)
getSymbols("AAPL",src="yahoo", from =as.Date("2020-01-01"),to=as.Date("2025-01-01"))
prices <- Ad(AAPL)

#Calculating log returns for adjusted closing prices
log_returns <- diff(log(prices))
log_returns<-as.numeric(log_returns[-1])

#Kurtosis calculation
library(moments)
kurtosis(log_returns)