load("D:/@复旦u/5.大三第一学期@NUS/1.学习/ST3233 Applied time Series Analysis/Datasets/tsa3.rda")
library(forecast)

# Consider time series gnp
plot(gnp, lwd = 3, main = "Time series: GNP")
plot(diff(gnp), lwd=3, main="Diff(GNP)")
# After differenciate by lag = 1, it is not stationary - it is increasing
# try to diff log(gnp)
plot(diff(log(gnp)), lwd=3, main="Diff(log(GNP))")


#using ARIMA model.

acf(diff(log(gnp)), lwd = 3, main = "diff(log(GNP))::ACF" ,lag.max = 20)
pacf(diff(log(gnp)), lwd = 3, main = "diff(log(GNP))::PACF")
#p = 0 : 2  
#q = 0 or 1
#d = 1

gnp_AIC_best <- 10**6
for (p in 0:2){
  for (q in 0:1){
    gnp_fit_arima <- Arima(log(gnp), order = c(p,1,q),include.drift = T)
    if (gnp_fit_arima$aic < gnp_AIC_best){
      gnp_AIC_best <- gnp_fit_arima$aic
      cat("p = ",p,",d = 1, q = ",q,", AIC = ",gnp_AIC_best,"\n")
    }
  }
}

#Choose ARIMA(1,1,0)

gnp_model <- arima(log(gnp), order = c(1,1,0))
plot(resid(gnp_model),lwd = 3,main = "residual of gnp: ARIMA(1,1,1)")

acf(resid(gnp_model),lwd = 3, main = "resid(gnp_model)::ACF")
pacf(resid(gnp_model),lwd = 3, main = "resid(gnp_model)::PACF")

qqnorm(resid(gnp_model),main = "resid(gnp_model)::QQPlot")
qqline(resid(gnp_model),lwd = 1, col = "red")

plot(forecast(gnp_model, h = 50),lwd =3)

#################################
#consider time series oil
plot(oil,lwd = 3, main = "Time series: Oil")
plot(diff(oil), lwd=3, main="Diff(GNP)")
plot(diff(log(oil)), lwd=3, main="Diff(GNP)")

acf(diff(log(oil)), lwd = 3, main = "diff(GNP)::ACF")
pacf(diff(log(oil)), lwd = 3, main = "diff(GNP)::PACF")
#p = 0:3 q = 0:1

oil_AIC_best <- 10**6
for (p in 0:3){
  for (q in 0:1){
    oil_fit_arima <- Arima(log(oil), order = c(p,1,q),include.drift = F)
    if (oil_fit_arima$aic < oil_AIC_best){
      oil_AIC_best <- oil_fit_arima$aic
      cat("p = ",p,",d = 1, q = ",q,", AIC = ",oil_AIC_best,"\n")
    }
  }
}

oil_model <- Arima(log(oil), order = c(3,1,1),include.drift = F)
plot(resid(oil_model),lwd = 3,main = "residual of oil: ARIMA(1,1,2)")

acf(resid(oil_model),lwd = 3, main = "resid(oil_model)::ACF")
pacf(resid(oil_model),lwd = 3, main = "resid(oil_model)::PACF")

qqnorm(resid(oil_model),main = "resid(gnp_model)::QQPlot")
qqline(resid(oil_model),lwd = 1, col = "red")

plot(forecast(oil_model, h = 50),lwd =3)
