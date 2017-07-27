tempData <- read.csv("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Tutorial/Tutorial7/durian.csv",header= TRUE,sep=",")
durian <- ts(tempData$query,frequency = 12,start = c(2004,1))
library(forecast)

#plot the time series
plot.ts(durian,type="l",main = "Queries of durian",lwd = 2)
#Notice that the lager t is , the lager variance is. so take log

durian_log <- log(durian)
plot(durian_log, main = "Log durian dataset")
Acf(durian_log, main = "ACF::log durian",lag.max = 12)

durian_log_d12 <- diff(durian_log, lag = 12)
plot(durian_log_d12,lwd = 2,main = "diff(log durian, lag = 12)")
Acf(durian_log_d12, main = "ACF::diff(log durian, lag = 12)")

durian_log_d12_d1 <- diff(durian_log_d12, lag = 1)
plot(durian_log_d12_d1,lwd = 2,main = "durian_log_d12_d1")
Acf(durian_log_d12_d1, main = "ACF::durian_log_d12_d1")
#q = 0:2
Pacf(durian_log_d12_d1, main = "PACF::durian_d12_d1")
#p = 0:3

AIC_best <- 10**6
for (p in 0:3){
  for (q in 0:2){
    for (P in 0:1){
      for (Q in 0:1){
        fit_sarima <- Arima(durian_log, order = c(p,1,q),seasonal = c(P,1,Q))
        if (fit_sarima$aic < AIC_best){
          AIC_best <- fit_sarima$aic
          cat("p = ",p,", q = ",q,",P = ",P,",Q = ",Q,"\t AIC = ",AIC_best,"\n")
        }
      }
    }
  }
}
# The model of log_durian is SARIMA(0,1,2)(0,1,1)[12]

### Cross validation:
#define CV function
CV <-  function(time_series, start, forecast_length,ts_model){
  ts_length <-  length(time_series)
  accuracy_list = c()
  for(k in start:(ts_length - forecast_length)){
    fitted_model <- ts_model(ts(time_series[0:k], frequency=12))
    RMSE <-  accuracy(forecast(fitted_model, h = forecast_length))[2]
    accuracy_list = c(accuracy_list, RMSE)
  }
  return(accuracy_list)
}

#make models(4 models):
model_sarima <- function(ts) return(Arima(ts,order = c(0,1,0), seasonal = c(0,1,0),include.drift = F))
model_TES <- function(ts) return(hw(ts,initial = "optimal",seasonal = "additive",h = 12))

start = 100
forecast_length <- 12
CV_durian <- data.frame(
  sarima = CV(durian_log, start, forecast_length, model_sarima),
  triple_exp = CV(durian_log, start, forecast_length, model_TES)
)

boxplot(CV_durian,main = "Durian::Cross Validation for RMSE", lwd=2)

#From cross validation, a better model should be triple exponential smoothing