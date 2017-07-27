temp_data <- read.csv("E:/ST3233/Assignment2/Datasets/temperature_in_singapore.csv",header= TRUE,sep=",")
temp <- ts(temp_data$mean_temp,frequency = 12,start=c(1982,1))
par(mfrow=c(1,1))
plot(temp,lwd = 2, main = "Temperature in SG")
acf(temp,lwd = 2 , main = "ACF::Temp")
#The time series {temp} is not stationary and has seasonal behavior, decompose it.
temp_decmp <-stl(temp,s.window = "periodic", robust = T)
plot(temp_decmp)
seasonplot(temp-temp_decmp$time.series[,"trend"],s = 12, col = 1:12, type = "l")
#There is seasonal component and trend, thus, use SARIMA model

temp_d12 = diff(temp, lag = 12)
plot(temp_d12,lwd = 2, main = "diff(temp, lag = 12)")

temp_d12_d1 = diff(temp_d12, lag = 1)
plot(temp_d12_d1,lwd = 2, main = "diff(diff(temp, lag = 12))")
par(mfrow=c(2,1))
acf(temp_d12_d1,lwd = 2, main = "ACF::diff(diff(temp, lag = 12))")
pacf(temp_d12_d1,lwd = 2, main = "PACF::diff(diff(temp, lag = 12))")
# From acf plot ,q <= 1, and from partial-acf plot, p <= 5 with d = D = 1 and P <= 1, Q <= 1

AIC_best <- 10**6
for (p in 0:5){
  for (q in 0:1){
    for (P in 0:1){
      for (Q in 0:1){
        fit_sarima <- Arima(temp, order = c(p,1,q),seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          AIC_best <- fit_sarima$aic
          cat("p = ",p,", q = ",q,",P = ",P,",Q = ",Q,"\t AIC = ",AIC_best,"\n")
        }
      }
    }
  }
}
# SARIMA(2,1,1)(0,1,1)[12] gives a lower AIC, with number of parameters = 4
temp_fit <- Arima(temp, order = c(2,1,1), seasonal = c(0,1,1))

#Then consider the residuals of the SARIMA model.
par(mfrow=c(1,1))
plot(resid(temp_fit),lwd=2, main="resid(SARIMA(2,1,1)(0,1,1)[12])")
par(mfrow=c(2,1))
acf(resid(temp_fit),lwd=2, main="ACF::resid(SARIMA(2,1,1)(0,1,1)[12])",lag.max = 11)
pacf(resid(temp_fit),lwd=2, main="PACF::resid(SARIMA(2,1,1)(0,1,1)[12])",lag.max = 11)
#The residual is stationary.
par(mfrow=c(1,1))
qqnorm(resid(temp_fit),lwd=2, main="QQplot::resid(SARIMA(2,1,1)(0,1,1)[12])")
qqline(resid(temp_fit), lwd=2, col="red")
#The residual follows a Gaussian Distribution.
#So, SARIMA(2,1,1)(0,1,1)[12] is a good smodel.

#Another method is to use Triple exponential smoothing
DES_fit <- hw(temp, initial = "optimal", seasonal = "additive", h = 2*12)
DES_fit
plot(DES_fit,main = "Temperature Forecasts from triple Exponential Smoothing", lwd = 2)

#Use cross-validation to compare these two models
CV <-  function(time_series, start, forecast_length,ts_model){
  ts_length <-  length(time_series)
  accuracy_list = c()
  for(k in start:(ts_length - forecast_length)){
    fitted_model <- ts_model(ts(time_series[0:k],frequency = 12))
    RMSE <-  accuracy(forecast(fitted_model, h = forecast_length))[2]
    accuracy_list = c(accuracy_list, RMSE)
  }
  return(accuracy_list)
}

model_SARIMA <- function(ts) 
  return(Arima(ts, order = c(2,1,1), seasonal = c(0,1,1)))
model_TES <- function(ts) 
  return(hw(ts,initial = "optimal", seasonal = "additive"))

start <- 300
forecast_length <- 24
CV_temp <- data.frame(
  sarima = CV(temp, start, forecast_length, model_SARIMA),
  tes = CV(temp, start, forecast_length, model_TES)
)

boxplot(CV_temp,main = "Temp::Cross Validation for RMSE", lwd=2)

#From boxplot, SARIMA(2,1,1)(0,1,1)[12] gives a better prediction, due to the lower RMSE.

#Forecast the number of birth during the two weeks by using SARIMA(2,1,1)(0,1,1)[12] model.
temp_forecast <- forecast(temp_fit, h = 2*12)
temp_forecast
plot(temp_forecast, main = "Temperature Forecasts from SARIMA(2,1,1)(0,1,1)[12]",lwd = 2)
