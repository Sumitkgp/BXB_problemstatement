library(forecast)
library(tseries)

setwd("E:/R")
trainbxb = read.csv("bxbtrain.csv")
trainbxb = as.data.frame(trainbxb)

testbxb = read.csv("bxbtest.csv")
testbxb1 = testbxb

TS = (trainbxb$IssuesInMonth - trainbxb$TransfersInMonth)/trainbxb$BusinessDaysInMonth

adf.test(TS)

trModel <- lm(TS ~ c(1:length(TS)))
#plot(resid(trModel), type="l")  # resid(trModel)

acfRes <- acf(TS) # autocorrelation
pacfRes <- pacf(TS)  # partial autocorrelation

fit = auto.arima(TS,seasonal=FALSE)

tsdisplay(residuals(fit), lag.max=3, main='(3,1,1) Model Residuals')

fit2 = arima(TS,order=c(3,1,1))

tsdisplay(residuals(fit2), lag.max=3, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=6)
#plot(fcast)

testbxb$IssuesInMonth = (fcast$lower[,1]*testbxb$BusinessDaysInMonth) + testbxb$TransfersInMonth

submitbxb = data.frame(Month = testbxb$Month, IssuesInMonth = testbxb$IssuesInMonth)
write.csv(submitbxb,file = "TS_BXB_Submission.csv", row.names = FALSE)
