library(forecast)
library(fpp)

# 1. Fit a SARIMA model
load("E:/ST3233/Assignment2/Datasets/tsa3.rda")
plot(birth,lwd = 2 ,main = "Monthly Birth(in thousand)")

birth_d12 <- diff(birth, lag = 12)
plot(birth_d12,lwd = 2,main = "diff(birth, lag = 12)")

birth_d12_d1 <- diff(birth_d12,lag = 1)
plot(birth_d12_d1,lwd = 2,main = "diff(birth_d12, lag = 1)")

par(mfrow=c(2,1))
acf(birth_d12_d1,lwd = 3, main = "ACF::diff(diff(birth, lag = 12))")
pacf(birth_d12_d1,lwd = 3, main = "PACF::diff(diff(birth, lag = 12))")

AIC_best = 10**6
k <-  0
for(p in 0:4){
  for(q in 0:1){
    for(P in 0:1){
      for(Q in 0:1){
        fit_sarima = Arima(birth, order = c(p,1,q), seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          k = k + 1
          AIC_best <- fit_sarima$aic
          cat("model",k,"\t p=",p,"q=",q,"P=",P,"Q=",Q,"\t AIC=",AIC_best,"\n")
        }
      }
    }
  }
}

birth_fit <- Arima(birth, order = c(4,1,0), seasonal = c(0,1,1))
#The SARIMA model is : SARIMA((4,1,0)(0,1,1)[12])

# 2. Use your model to get 80% confidence interval for the number of births in Feb 1979
forecast(birth_fit, h=1)
#Thus,the 80% confidence interval for the number of births in Feb 1979 is [248.108, 265.4349]

#3. Use an approach similar to cross validation to estimate whether you can trust the 80% confidence interval.
# define a function CV_sarima to do the cross validation for the SARIMA model with parameters: p,q,P,Q(d,D and s are given)

ts_length <-  length(birth)
forecast_length <- 1
start <- 250
lower_bounds <- c()
upper_bounds<- c()
correct_num <- 0
wrong_num <- 0
for(i in start:(ts_length - forecast_length)){

  fitted_sarima<-  Arima(birth[0:i], order = c(4,1,0), seasonal = c(0,1,1))
  forecast_result <-  forecast(fitted_sarima, h = forecast_length)
  lower_bounds[i] <- forecast_result$lower[1]
  upper_bounds[i] <- forecast_result$upper[1]
  if (birth[i+1] > lower_bounds[i] & birth[i+1] < upper_bounds[i]){
    correct_num = correct_num + 1
  }else{
    wrong_num= wrong_num + 1
  }
}
correct_num
wrong_num

upper_bounds_ts<-ts(upper_bounds, start = c(1948,2), frequency = 12)
lower_bounds_ts<-ts(lower_bounds, start = c(1948,2), frequency = 12)

par(mfrow=c(1,1))
plot(birth, lwd =2, main="Prediction of Birth Time Series")
lines(upper_bounds_ts, lwd = 3, col = "blueviolet")
lines(lower_bounds_ts, lwd = 3, col = "blueviolet")

#We did 123 forecasts to examine whether the true value lies in the 80% confidence interval of prediction,
#There are 104/123 in the confidence interval, and 19/123 of the forecasts doesn't.
#Also, from the plot, we can trust 80% confidence interval.