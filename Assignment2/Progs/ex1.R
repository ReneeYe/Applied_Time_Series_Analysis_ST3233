# 1. Fit a SARIMA model
library(forecast)
load("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Datasets/tsa3.rda")
plot(birth,lwd = 2 ,main = "Monthly Birth(in thousand)")

birth_d12 <- diff(birth, lag = 12)
plot(birth_d12,lwd = 2,main = "diff(birth, lag = 12)")

birth_d12_d1 <- diff(birth_d12,lag = 1)
plot(birth_d12_d1)

Acf(birth_d12_d1,lwd = 3, main = "ACF::diff(diff(birth, lag = 12))")
Pacf(birth_d12_d1,lwd = 3, main = "PACF::diff(diff(birth, lag = 12))")

AIC_best = 10**6
k <-  0
model_p <-  c()
model_q <-  c()
model_P <-  c()
model_Q <-  c()
for(p in 0:4){
  for(q in 0:1){
    for(P in 0:1){
      for(Q in 0:1){
        fit_sarima = Arima(birth, order = c(p,1,q), seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          k = k + 1
          AIC_best <- fit_sarima$aic
          model_p[k] <- p
          model_q[k] <- q
          model_P[k] <- P
          model_Q[k] <- Q
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
#Thus,the 80% confidence interval for the number of births in Feb 1979 is [250.0562,267.3686]

#3. Use an approach similar to cross validation to estimate whether you can trust the 80% confidence interval.
# define a function CV_sarima to do the cross validation for the SARIMA model with parameters: p,q,P,Q(d,D and s are given)
library(fpp)
CV_sarima <-  function(time_series, start, forecast_length, p, q, P, Q){
  ts_length <-  length(time_series)
  accuracy_list = c()
  for(j in start:(ts_length - forecast_length)){
    fitted_model <-  Arima(time_series[0:j], order = c(p,1,q), seasonal = c(P,1,Q))
    RMSE <-  accuracy(forecast(fitted_model, h = forecast_length))[2]
    accuracy_list = c(accuracy_list, RMSE)
  }
  return(accuracy_list)
}


start <-  240
forecast_length <- 36
#Initial the CV_birth
CV_birth <- data.frame(rep(0,each = 98))
for (i in 1:k){
  sarima<- CV_sarima(birth, start, forecast_length, p=model_p[i],q=model_q[i],P=model_P[i],Q=model_Q[i])
  CV_birth<- data.frame(CV_birth,sarima)
}

boxplot(CV_birth[1:k+1],main = "Birth::Cross Validation for RMSE", lwd=2)

#According to the boxplot, the best model should be SARIMA(2,1,3)(1,1,1)[12]
