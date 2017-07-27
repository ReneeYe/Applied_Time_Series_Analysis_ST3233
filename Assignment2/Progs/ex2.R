birth_data <- read.csv("E:/ST3233/Assignment2/Datasets/daily-total-female-births-in-cal.csv",header= TRUE,sep=",")
birth_cal <- ts(birth_data$Daily.total.female.births.in.California,frequency = 356,start = c(1959))
par(mfrow=c(1,1))
plot(birth_cal,lwd = 2, main = "Daily total female births in California")
#Plot ACF to see whether the time series is stationary or not.
acf(birth_cal,lwd = 2, main ="ACF::Birth in California",lag.max = 20)
#From the plot, the time series is not stationary, thus we cannot use ARMA model.
#Besides, there is no seasonal component, so we first choose ARIMA model.

#Consider a new time series: diff(birth_cal)
birth_d1 = diff(birth_cal)
plot(birth_d1,lwd = 2, main = "Diff(birth in California)")
#Diff(birth_cal) is stationary, and then consider the acf and pacf.

par(mfrow=c(2,1))
acf(birth_d1,lwd = 2, main =  "ACF::diff(birth)",lag.max = 20)
pacf(birth_d1,lwd = 2, main =  "PACF::diff(birth)",lag.max = 20)
#From acf plot ,q <= 1, and from partial-acf plot, p <= 6 with d = 1
AIC_best <- 10**6
for (p in 0:6){
  for (q in 0:1){
    fit_arima <- Arima(birth_cal, order = c(p,1,q))
    if (fit_arima$aic < AIC_best){
      AIC_best <- fit_arima$aic
      cat("p = ",p,",d = 1, q = ",q,", AIC = ",AIC_best,"\n")
    }
  }
}

#ARIMA(1,1,1) has the lowest AIC, so we choose ARIMA(1,1,1)
arima_fit <- Arima(birth_cal,order = c(1,1,1))

#Then examine whether this model is appropriate or not by examining the normality of the residuals.
par(mfrow=c(1,1))
plot(resid(arima_fit),lwd=2, main="resid(ARIMA(1,1,1))")

par(mfrow=c(2,1))
acf(resid(arima_fit),lwd=2, main="ACF::resid(ARIMA(1,1,1))",lag.max = 20)
pacf(resid(arima_fit),lwd=2, main="PACF::resid(ARIMA(1,1,1))",lag.max = 20)
par(mfrow=c(1,1))
qqnorm(resid(arima_fit), main="QQplot::resid(ARIMA(1,1,1))", lwd=3)
qqline(resid(arima_fit), lwd=2, col="red")
# Thus, the residuals are follows a Gaussian Distribution.

#Forecast the number of birth during the two weeks by using ARIMA(1,1,1)
arima_forecast <- forecast(arima_fit, h = 2*7)
arima_forecast
plot(arima_forecast,main = "Birth Forecasts from ARIMA(1,1,1)",lwd = 2)

#Another model is Double exponential smooting
DES_fit <- holt(birth_cal, initial = "optimal", h =2*7)
DES_fit
plot(DES_fit,main = "Birth Forecasts from Double Exponential Smoothing", lwd = 2)

#Compare ARIMA(1,1,1) and DES by using cross-validation
CV <-  function(time_series, start, forecast_length,ts_model){
  ts_length <-  length(time_series)
  accuracy_list = c()
  for(k in start:(ts_length - forecast_length)){
    fitted_model <- ts_model(ts(time_series[0:k]))
    RMSE <-  accuracy(forecast(fitted_model, h = forecast_length))[2]
    accuracy_list = c(accuracy_list, RMSE)
  }
  return(accuracy_list)
}

#Define two models
model_ARIMA <- function(ts) return(Arima(ts,order = c(1,1,1)))
model_DES <- function(ts) return(holt(ts,initial = "optimal"))

start <- 250
forecast_length <- 7
CV_birth_Cal <- data.frame(
  arima = CV(birth_cal, start, forecast_length, model_ARIMA),
  des = CV(birth_cal, start, forecast_length, model_DES)
)

boxplot(CV_birth_Cal,main = "Birth::Cross Validation for RMSE", lwd=2)

#From boxplot, ARIMA(1,1,1) has a lower RMSE, which is a better model.

