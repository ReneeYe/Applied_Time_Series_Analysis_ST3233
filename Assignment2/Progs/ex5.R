carSale_data <- read.csv("E:/ST3233/Assignment2/Datasets/monthly-car-sales-in-quebec-1960.csv",header= TRUE,sep=",")
carSale <- ts(carSale_data$Monthly.car.sales.in.Quebec.1960.1968[1:108], frequency = 12,start=c(1960,1))
par(mfrow=c(1,1))
plot(carSale,lwd = 2, main = "monthly car sales")
acf(carSale,lwd = 2 , main = "ACF::carSale")
#The time series is not stationary and has periodicity and trend.

carSale_decmp <-stl(carSale, s.window = "periodic", robust = T)
plot(carSale_decmp)
seasonplot(carSale-carSale_decmp$time.series[,"trend"],s = 12, col = 1:12, type = "l")
#There is seasonal component and trend, thus, use SARIMA model

carSale_d12 = diff(carSale, lag = 12)
plot(carSale_d12,lwd = 2, main = "diff(carSale, lag = 12)")
acf(carSale_d12,lwd = 2, main ="ACF::diff(carSale, lag = 12)")
#carSale_d12 is not stationary, consider diff(carSale_d12)

carSale_d12_d1 = diff(carSale_d12, lag = 1)
plot(carSale_d12_d1,lwd = 2, main = "diff(diff(carSale, lag = 12))")
par(mfrow=c(2,1))
acf(carSale_d12_d1,lwd = 2, main = "ACF::diff(diff(carSale, lag = 12))")
pacf(carSale_d12_d1,lwd = 2, main = "PACF::diff(diff(carSale, lag = 12))")
# From acf plot ,q <= 1, and from partial-acf plot, p <= 2 with d = D = 1 and P <= 1, Q <= 1

AIC_best <- 10**6
for (p in 0:2){
  for (q in 0:1){
    for (P in 0:1){
      for (Q in 0:1){
        fit_sarima <- Arima(carSale, order = c(p,1,q),seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          AIC_best <- fit_sarima$aic
          cat("p = ",p,", q = ",q,",P = ",P,",Q = ",Q,"\t AIC = ",AIC_best,"\n")
        }
      }
    }
  }
}
# The lowest AIC gives the best fitted model, which is SARIMA(0,1,1)(0,1,1)[12]
carSale_fit <- Arima(carSale, order = c(0,1,1), seasonal = c(0,1,1))

#Then consider the residuals of the SARIMA model.
par(mfrow=c(1,1))
plot(resid(carSale_fit),lwd=2, main="resid(SARIMA(0,1,1)(0,1,1)[12])")
par(mfrow=c(2,1))
acf(resid(carSale_fit),lwd=2, main="ACF::resid(SARIMA(0,1,1)(0,1,1)[12])",lag.max = 11)
pacf(resid(carSale_fit),lwd=2, main="PACF::resid(SARIMA(0,1,1)(0,1,1)[12])",lag.max = 11)
#The residual is stationary.
par(mfrow=c(1,1))
qqnorm(resid(carSale_fit),lwd=2, main="QQplot::resid(SARIMA(0,1,1)(0,1,1)[12])")
qqline(resid(carSale_fit), lwd=2, col="red")
shapiro.test(resid(carSale_fit))
#From the qq-plot and Shapiro test(p-value = 0.3211>0.05), the residual can be regard as a gaussian distribution.
#So, SARIMA(0,1,1)(0,1,1)[12] is a good model.

# Another method is using Triple exponential smoothing
TES_fit <- hw(carSale, initial = "optimal", seasonal = "additive", h = 2*12)
TES_fit
plot(TES_fit,main = "Car Sales Forecasts from Triple Exponential Smoothing", lwd = 2)

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
  return(Arima(ts, order = c(0,1,1), seasonal = c(0,1,1)))
model_TES <- function(ts) 
  return(hw(ts,initial = "optimal", seasonal = "additive"))

start <- 60
forecast_length <- 2*12
CV_carSale <- data.frame(
  sarima = CV(carSale, start, forecast_length, model_SARIMA),
  tes = CV(carSale, start, forecast_length, model_TES)
)

boxplot(CV_carSale,main = "Car Sales::Cross Validation for RMSE", lwd=2)

#From boxplot, TES gives a better prediction due to the lower RMSE.

#Forecast the number of birth during the two weeks by using triple exponential smoothing.
TES_fit <- hw(carSale, initial = "optimal", seasonal = "additive", h = 2*12)
TES_fit
plot(TES_fit,main = "Car Sales Forecasts from Triple Exponential Smoothing", lwd = 2)
