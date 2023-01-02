library(ggplot2)
library(xts)
library(forecast)
library(zoo)
library(tseries)
#par(mfcol = c(2,1))
data <- read.csv("C:\Users\mnimi\Documents\Sem3\20XT36 StatisticalComputingandRProgramming\Package\Final\co2.CSV")
class(data)

#data.set <- xts(data, frequency = 12)
data.set1 <- ts(data$average_seasonal, frequency = 12)
data.set2 <- ts(data$interpolated_corr, frequency = 12)
data.set1 <- na.approx(data.set1)
data.set2 <- na.approx(data.set2)

par(mfcol = c(2,2))
autoplot(data.set1)
autoplot(data.set2)

ddata1 <- decompose(data.set1)
ddata2 <- decompose(data.set2)
#par(mfcol = c(2,1))
#ddata1 <- decompose(coredata(data.set1), type = c("additive")
#ddata2 <- decompose(coredata(data.set2), type = c("additive"))
autoplot(ddata1)
autoplot(ddata2)

best.model1 <- auto.arima(coredata(data.set1), ic="aic", trace = TRUE)
best.model2 <- auto.arima(coredata(data.set2), ic="aic", trace = TRUE)
par(mfcol = c(2,1))
autoplot(best.model1$residuals)
autoplot(best.model2$residuals)

#acf and pacf for datset1
par(mfcol = c(2,1))
acf(ts(best.model1$residuals), main = 'ACF - Correlation for I')
pacf(ts(best.model1$residuals), main = 'PACF -  Correlation for I')

#acf and pacf for datset1
par(mfcol = c(2,1))
acf(ts(best.model2$residuals), main = 'ACF - Correlation for II')
pacf(ts(best.model2$residuals), main = 'PACF -  Correlation for II')

#forecast prediction for dataset1 and dataset2
forecast.prediction1 <- forecast(best.model1, level = c(95), h=10*12)
forecast.prediction2 <- forecast(best.model2, level = c(95), h=10*12)

#forecast plotting for dataset1 and dataset2
#par(mfcol = c(2,1))
autoplot(forecast.prediction1)
autoplot(forecast.prediction2)

#acf for forecast prediction 1
par(mfcol = c(2,1))
acf(as.numeric(forecast.prediction1$residuals), main = "ACF - forecast residuals I")
pacf(forecast.prediction1$residuals, main = "PACF - forecast residuals I")

#acf for forecast prediction 2
par(mfcol = c(2,1))
acf(as.numeric(forecast.prediction2$residuals), main = "ACF - forecast residuals II")
pacf(forecast.prediction2$residuals, main = "PACF - forecast residuals II")

#Test for forecast prediction 1
Box.test(best.model1$residuals, lag=5, type="Ljung-Box")
Box.test(best.model1$residuals, lag=10, type="Ljung-Box")
Box.test(best.model1$residuals, lag=15, type="Ljung-Box")

#Test for forecast prediction 2
Box.test(best.model2$residuals, lag=5, type="Ljung-Box")
Box.test(best.model2$residuals, lag=10, type="Ljung-Box")
Box.test(best.model2$residuals, lag=15, type="Ljung-Box")

mean(forecast.prediction1$residuals)
mean(forecast.prediction2$residuals)

