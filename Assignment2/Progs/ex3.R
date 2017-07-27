beer_data <- read.csv("E:/ST3233/Assignment2/Datasets/quarterly-beer-production-in-aus.csv",header= TRUE,sep=",")
beer <- ts(beer_data$beer_production,frequency = 4,start=c(1956))
par(mfrow=c(1,1))
plot(beer,lwd = 2, main = "Quarterly Beer Production in Austrilia")
#From the plot, we can clearly see that there is periodicity and trend, thus we decompose it.
beer_decmp <-stl(beer,s.window = "periodic", robust = T)
plot(beer_decmp)
#seasonal plot
seasonplot(beer - beer_decmp$time.series[,"trend"], s = 4,col = 1:6, type = "l")
#There is seasonal behavior, thus we first use SARIMA model

#Notice that the time series has a non-linear trend, so we consider a new time series: log(beer)
log_beer = log(beer)
plot(log_beer, lwd=2, main="log_beer")

log_beer_d4 = diff(log_beer, lag = 4)
plot(log_beer_d4,lwd = 2, main = "diff(log_beer, lag = 4)")

log_beer_d4_d1 = diff(log_beer_d4, lag = 1)
plot(log_beer_d4_d1,lwd = 2, main = "diff(diff(log_beer, lag = 4))")

par(mfrow=c(2,1))
acf(log_beer_d4_d1,lwd = 2, main = "ACF::diff(diff(log_beer, lag = 4))",lag.max = 20)
pacf(log_beer_d4_d1,lwd = 2, main = "PACF::diff(diff(log_beer, lag = 4))",lag.max = 20)
# From acf plot ,q <= 4, and from partial-acf plot, p <= 2 with d = D = 1 and P <= 1, Q <= 1

AIC_best <- 10**6
for (p in 0:2){
  for (q in 0:4){
    for (P in 0:1){
      for (Q in 0:1){
        fit_sarima <- Arima(log_beer, order = c(p,1,q), seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          AIC_best <- fit_sarima$aic
          cat("p = ",p,", q = ",q,",P = ",P,",Q = ",Q,"\t AIC = ",AIC_best,"\n")
        }
      }
    }
  }
}
# The lowest AIC gives the best fitted model of log_beer, which is SARIMA(0,1,2)(0,1,1)[4]
sarima_fit <- Arima(log_beer, order = c(0,1,2), seasonal = c(0,1,1))

#Then consider the residuals of the SARIMA model.
par(mfrow=c(1,1))
plot(resid(sarima_fit),lwd=2, main="resid(SARIMA(0,1,2)(0,1,1)[4])")

par(mfrow=c(2,1))
acf(resid(sarima_fit),lwd=2, main="ACF::resid(SARIMA(0,1,2)(0,1,1)[4])")
pacf(resid(sarima_fit),lwd=2, main="PACF::resid(SARIMA(0,1,2)(0,1,1)[4])")
#The residual is stationary.

par(mfrow=c(1,1))
qqnorm(resid(sarima_fit),lwd=2, main="QQplot::resid(SARIMA(0,1,2)(0,1,1)[4])")
qqline(resid(sarima_fit), lwd=2, col="red")
#The distribution of residuals can be regard as a gaussian distribution.
#So, SARIMA(0,1,2)(0,1,1)[4] is a good model.

#Another method is to use Triple exponential smoothing
DES_fit <- hw(beer, initial = "optimal", seasonal = "additive", h = 6)
DES_fit
plot(DES_fit,main = "Beer Forecasts from triple Exponential Smoothing", lwd = 2)

#Use cross-validation to compare these two models
CV <-  function(time_series, start, forecast_length,ts_model){
  ts_length <-  length(time_series)
  accuracy_list = c()
  for(k in start:(ts_length - forecast_length)){
    fitted_model <- ts_model(ts(time_series[0:k],frequency = 4))
    RMSE <-  accuracy(forecast(fitted_model, h = forecast_length))[2]
    accuracy_list = c(accuracy_list, RMSE)
  }
  return(accuracy_list)
}

#Define two models
model_SARIMA <- function(ts) 
  return(Arima(ts, order = c(0,1,2), seasonal = c(0,1,1)))
model_TES <- function(ts) 
  return(hw(ts,initial = "optimal", seasonal = "additive"))

start <- 90
forecast_length <- 6
CV_beer <- data.frame(
  sarima = CV(log_beer, start, forecast_length, model_SARIMA),
  tes = CV(log_beer, start, forecast_length, model_TES)
)

boxplot(CV_beer,main = "Beer::Cross Validation for RMSE", lwd=2)

#From boxplot, SARIMA(0,1,2)(0,1,1)[4] gives a better prediction.

#Forecast the number of birth during the two weeks.
sarima_forecast <- forecast(sarima_fit, h = 6)
sarima_forecast$x<-exp(sarima_forecast$x)
sarima_forecast$lower<-exp(sarima_forecast$lower)
sarima_forecast$upper<-exp(sarima_forecast$upper)
sarima_forecast$mean<-exp(sarima_forecast$mean)
sarima_forecast
plot(sarima_forecast, main = "Beer Forecasts from SARIMA(0,1,2)(0,1,1)[4]",lwd = 2)
