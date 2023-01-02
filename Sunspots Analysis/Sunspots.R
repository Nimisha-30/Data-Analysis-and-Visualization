library(ggplot2)
library(xts)
library(forecast)
library(zoo)
library(tseries)
library(TTR)

data <- read.csv("C:\Users\mnimi\Documents\Sem3\20XT36 StatisticalComputingandRProgramming\Package\Final\SunspotsDataset.csv")
plot(data$Sunspots, type='l', col = 'dark orange')

data.set <- ts(data$Sunspots, frequency = 12)
data.set <- na.approx(data.set)
class(data.set)

#ddata <- SMA(data.set, n=4)
ddata = decompose(data.set, type = c("additive"))
plot(ddata, type='l', col = c('dark green'))

best.model <- auto.arima(data.set)
plot.ts(best.model$residuals, col = 'blue')

#acf and pacf
acf(ts(best.model$residuals), main = 'ACF - Correlation')
pacf(ts(best.model$residuals), main = 'PACF -  Correlation')

#forecast
#forecast.prediction <- forecast(best.model, level = c(95), h=10*12)
forecast.prediction <- forecast(best.model, h=15*12)
plot(forecast.prediction, col = 'magenta')
plot(forecast.prediction$residuals, col = 'purple')

acf(as.numeric(forecast.prediction$residuals), main = "ACF - forecast residuals")
pacf(forecast.prediction$residuals, main = "PACF - forecast residuals")
autoplot(forecast.prediction, facets = FALSE)

Box.test(best.model$residuals, lag=10, type="Ljung-Box")
Box.test(best.model$residuals, lag=20, type="Ljung-Box")
Box.test(best.model$residuals, lag=50, type="Ljung-Box")

mean(forecast.prediction$residuals)

