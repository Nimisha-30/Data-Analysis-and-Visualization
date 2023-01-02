library(ggplot2)
library(xts)
library(forecast)
library(zoo)
library(tseries)

data <- read.csv("C:\Users\mnimi\Documents\Sem3\20XT36 StatisticalComputingandRProgramming\Package\Final\co2.CSV")
class(data)

#data.set <- xts(data, frequency = 12)
data.set <- ts(data$average_seasonal, frequency = 12)
data.set <- na.approx(data.set)
class(data.set)
autoplot(data.set)

ddata <- decompose(data.set)
#par(mar=c(1,1,1,1))
#par(mfcol=c(3,1))
#plot(data.set)
#plot(data$average_seasonal, type='l', main = "Seasonality")
#plot(data$trend, type='l', main = "Trend")

ddata <- decompose(coredata(data.set), type = c("additive"))
autoplot(ddata)

best.model <- auto.arima(coredata(data.set), ic="aic", trace = TRUE)
#best.model <- arima(coredata(data.set), order = c(1,0,2))
autoplot(best.model$residuals)
#acf and pacf

acf(ts(best.model$residuals), main = 'ACF - Correlation')
pacf(ts(best.model$residuals), main = 'PACF -  Correlation')

forecast.prediction <- forecast(best.model, level = c(95), h=10*12)
autoplot(forecast.prediction)

acf(as.numeric(forecast.prediction$residuals), main = "ACF - forecast residuals")
pacf(forecast.prediction$residuals, main = "PACF - forecast residuals")
autoplot(forecast.prediction, facets = FALSE)

Box.test(best.model$residuals, lag=5, type="Ljung-Box")
Box.test(best.model$residuals, lag=10, type="Ljung-Box")
Box.test(best.model$residuals, lag=15, type="Ljung-Box")

mean(forecast.prediction$residuals)

